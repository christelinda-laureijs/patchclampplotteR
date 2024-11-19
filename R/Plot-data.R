#' Add a customized ggplot2 theme
#'
#' @returns A ggplot theme
#' @export
#'
#' @examples
#'
#' ggplot2::ggplot(cars, ggplot2::aes(x = speed, y = dist)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(x = "Speed (mph)", y = "Stopping Distance (ft)") +
#'   patchclampplotteR_theme()
#'
patchclampplotteR_theme <- function() {
  ggplot2::theme_classic() %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(),
      plot.title = ggplot2::element_text(
        color = "black",
        size = 20,
        margin = ggplot2::margin(b = 25),
        hjust = 0.5
      ),
      plot.margin = ggplot2::margin(25, 25, 25, 25),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 12
      ),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(size = 12, color = "black"),
      axis.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 25),
        angle = 90,
        vjust = 0.5
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(b = 25, t = 20)),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(color = NA, fill = NA),
      strip.text = ggplot2::element_text(size = 20)
    )
}

#' Make baseline comparison plot
#'
#' This function creates a scatterplot of parameters such as raw amplitude
#' grouped according to treatment. The data are limited to values from the
#' baseline period, allowing for a quick comparison of baseline parameters
#' across treatments. For evoked currents, only raw amplitude is available for
#' comparison. For spontaneous currents, raw amplitude and raw frequency during
#' the baseline period can be plotted.
#'
#'
#' @param data A dataframe containing the summary data generated from
#'   [make_summary_EPSC_data()].
#' @param treatment_colour_theme A dataframe containing treatment names and
#'   their associated colours as hex values. See
#'   [sample_treatment_names_and_colours] for an example of what this dataframe
#'   should look like.
#' @param theme_options A dataframe containing theme options. See
#'   [sample_theme_options] for an example of what this dataframe should
#'   look like.
#' @param include_all_treatments A character ("yes" or "no") specifying if the
#'   plot will include data from all treatments. If "no", you must specify a
#'   list of treatments in `list_of_treatments`.
#' @param list_of_treatments A list of character values describing the
#'   treatments that will be in the plot. Defaults to NULL, since
#'   include_all_treatments is "yes" by default.
#' @param baseline_interval A character value indicating the name of the
#'   interval used as the baseline. Defaults to "t0to5", but can be changed.
#'   Make sure that this matches the baseline interval that you set in
#'   [make_normalized_EPSC_data()].
#' @param filename_suffix Optional character value to add a suffix to the
#'   filename of the .png file created with this plot. Could be useful if you
#'   have specified a custom list of treatments.
#' @param current_type A character describing the current type. Allowed values
#'   are "eEPSC" or "sEPSC".
#' @param parameter A character describing the parameter used on the y-axis. If
#'   current_type == "eEPSC", the allowed parameter is "raw_amplitude". If
#'   `current_type == "sEPSC"`, the allowed parameters are "raw_amplitude" or
#'   "raw_frequency". Note: It does not make sense to use normalized/baseline
#'   transformed amplitudes, since these will all be 100, and the graph will be
#'   a flat line.
#' @param large_axis_text A character ("yes" or "no"). If "yes", a ggplot theme
#'   layer will be applied which increases the axis text.
#' @param plot_width A numeric value specifying the width of the plot. Defaults
#'   to 8, but you will need to adjust this depending on how many treatments you
#'   have.
#' @param save_plot_png A character ("yes" or "no"). If "yes", the plot will be
#'   saved as a .png using ggsave. The filepath depends on the current type, but
#'   they will all go in subfolders below Figures/ in your project directory.
#'
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in `Figures/Evoked-currents/Output-summary-plots` or
#'   `Figures/Spontaneous-currents/Output-summary-plots`. The .png filename will
#'   contain the `parameter`.
#' @export
#'
#' @examples
#' plot_baseline_data(
#'   data = sample_summary_eEPSC_df,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   include_all_treatments = "yes",
#'   list_of_treatments = NULL,
#'   current_type = "eEPSC",
#'   parameter = "raw_amplitude",
#'   baseline_interval = "t0to5",
#'   large_axis_text = "no",
#'   plot_width = 8,
#'   save_plot_png = "no"
#' )
plot_baseline_data <- function(data,
                               treatment_colour_theme,
                               theme_options,
                               include_all_treatments = "yes",
                               list_of_treatments = NULL,
                               baseline_interval = "t0to5",
                               filename_suffix = "",
                               current_type = "eEPSC",
                               parameter = "raw_amplitude",
                               large_axis_text = "no",
                               plot_width = 8,
                               save_plot_png = "no") {
  if (is.null(current_type) ||
    length(current_type) != 1L ||
    !current_type %in% c("eEPSC", "sEPSC")) {
    stop("'current_type' argument must be one of: 'eEPSC' or 'sEPSC'")
  }

  if (is.null(baseline_interval) ||
    !is.character(baseline_interval)) {
    stop("'baseline_interval' must be a character (e.g. \"t0to5\", \"t0to3\")")
  }

  if (!save_plot_png %in% c("yes", "no")) {
    stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
  }

  if (!large_axis_text %in% c("yes", "no")) {
    stop("'large_axis_text' argument must be one of: 'yes' or 'no'")
  }

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      warning(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is NULL.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    if (!is.character(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is not a character object.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }

  if (current_type == "sEPSC") {
    filepath <- "Figures/Spontaneous-currents/Output-summary-plots"

    allowed_parameters_list <- "\"raw_amplitude\", or \"raw_frequency\""

    if (!parameter %in% c("raw_amplitude", "raw_frequency")) {
      stop(
        "parameter must be ",
        allowed_parameters_list,
        " for current_type \"",
        current_type,
        "\" \nbecause transformed data are all 100 during the baseline."
      )
    }

    if (parameter == "raw_amplitude") {
      y_var <- "mean_raw_amplitude"
      y_title <- "Baseline sEPSC Amplitude (pA)"
    }

    if (parameter == "raw_frequency") {
      y_var <- "mean_raw_frequency"
      y_title <- "Baseline sEPSC Frequency (Hz)"
    }
  }

  if (current_type == "eEPSC") {
    allowed_parameters_list <- c("\"raw_amplitude\"")

    if (!parameter %in% c("raw_amplitude")) {
      stop(
        "parameter must be ",
        allowed_parameters_list,
        " for current_type \"",
        current_type,
        "\"",
        " \nbecause transformed data are all 100 during the baseline."
      )
    }

    filepath <- "Figures/Evoked-currents/Output-summary-plots"

    if (parameter == "raw_amplitude") {
      y_var <- "mean_P1_raw"
      y_title <- "Baseline eEPSC Amplitude (pA)"
    }
  }

  baseline_comparison_plot <- plot_data %>%
    dplyr::filter(.data$interval == baseline_interval) %>%
    dplyr::mutate(
      treatment = stringr::str_replace_all(
        .data$treatment,
        stats::setNames(treatment_info$display_names, treatment_info$treatment)
      ),
      treatment = factor(.data$treatment, levels = treatment_info$display_names)
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$treatment,
        y = .data[[y_var]],
        color = .data$treatment,
        shape = .data$sex
      )
    ) +
    ggplot2::labs(x = NULL, y = y_title) +
    ggforce::geom_sina(
      bw = 12,
      alpha = 0.8,
      maxwidth = 0.5,
      size = 2
    ) +
    ggplot2::stat_summary(
      fun.data = ggplot2::mean_se,
      geom = "pointrange",
      color = theme_options["mean_point_colour", "value"],
      size = as.numeric(theme_options["mean_point_size", "value"]) + 0.02,
      alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      breaks = treatment_info$display_names,
      values = treatment_info$colours
    ) +
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = NA)
    ) +
    ggplot2::guides(
      color = "none",
      shape = ggplot2::guide_legend(reverse = TRUE)
    ) +
    patchclampplotteR_theme()

  if (large_axis_text == "yes") {
    baseline_comparison_plot <- baseline_comparison_plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(size = 24, face = "plain")
      )
  }

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      baseline_comparison_plot,
      path = here::here(filepath),
      file = paste0(
        "Baseline-",
        parameter,
        "-comparison",
        filename_suffix,
        ".png"
      ),
      width = plot_width,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(baseline_comparison_plot)
}


#' Make raw current plots
#'
#' This function enables you to create a scatterplot of raw evoked (or
#' spontaneous) current amplitude over time in minutes. The plot title will be
#' pulled from the recording ID (the `letter` column in the raw data), and the
#' subtitle will include the sex and treatment. The plot will also contain a
#' horizontal line annotation displayed over the time region where a hormone was
#' applied. If a high-frequency stimulation (HFS) protocol is used, the plot
#' will display an arrow with the label "HFS" to indicate the time when HFS was
#' applied.
#'
#'
#' @param data A dataframe containing the raw evoked current data generated from
#'   [make_normalized_EPSC_data()].
#' @param plot_treatment A character value specifying the treatment you would
#'   like to plot (e.g. "Control"). `plot_treatment` represents antagonists that
#'   were present on the brain slice, or the animals were fasted, etc.
#' @param plot_category A numeric value specifying the category, which can be
#'   used to differentiate different protocol types. In the sample dataset for
#'   this package, `plot_category == 2` represents experiments where insulin was
#'   applied continuously after a 5-minute baseline period.
#' @param current_type A character describing the current type. Allowed values
#'   are "eEPSC" or "sEPSC".
#' @param parameter A character value specifying the parameter to be plotted on
#'   the y-axis. For evoked currents (`current_type = "eEPSC"`), the available
#'   parameters are "P1", "mean_P1" and "PPR". *Note*: If you select "mean_P1",
#'   you must set the `pruned` argument to "yes". For spontaneous currents
#'   (`current_type = "sEPSC"`), the available parameters are "amplitude" or
#'   "frequency".
#' @param pruned A character value ("yes" or "no") specifying if the data are
#'   pruned. This is only used for evoked current data where `parameter =
#'   "mean_P1`. The plot will then present the data as means with error bars.
#' @param hormone_added A character value that will be used as the label over
#'   the line annotating the period when a hormone was applied. Examples include
#'   "500 nM Insulin", "CCK + Leptin", and "Insulin". If you applied a
#'   high-frequency stimulation (HFS) protocol instead, write "HFS", and an
#'   annotation arrow will be added instead.
#' @param hormone_or_HFS_start_time A numeric value indicating the time (in
#'   minutes) when a hormone was added or when HFS was applied. This will set
#'   the annotation line start point.
#' @param hormone_end_time A numeric value indicating the time (in minutes) when
#'   a hormone stopped being applied, such as for a washout experiment.
#' @param theme_options A dataframe containing theme options. See
#'   [sample_theme_options] for an example of what this dataframe should
#'   look like.
#' @param save_plot_png A character ("yes" or "no"). If "yes", the plot will be
#'   saved as a .png using ggsave. The filepath depends on the current type, but
#'   they will all go in subfolders below Figures/ in your project directory.
#' @param treatment_colour_theme A dataframe containing treatment names and
#'   their associated colours as hex values. See
#'   [sample_treatment_names_and_colours] for an example of what this dataframe
#'   should look like.
#'
#' @returns A list of ggplot objects, where each list element is a scatterplot of
#'   one recording. If `save_plot_png == "yes"`, it will also generate a .png
#'   file from each ggplot element in the list. The figures will be exported to
#'   `Figures/Evoked-currents/Output-individual-plots` or
#'   `Figures/Spontaneous-currents/Output-individual-plots`, depending on the
#'   `current_type`. The .png filename will contain the `letter`. If the data
#'   are pruned, the filename will also include `_pruned` in the filename.
#'   Example filenames include "A.png", and "A_pruned.png".
#' @export
#'
#' @examples
#'
#' plot_raw_current_data(
#'   data = sample_raw_eEPSC_df,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   current_type = "eEPSC",
#'   parameter = "P1",
#'   pruned = "no",
#'   hormone_added = "Insulin",
#'   hormone_or_HFS_start_time = 5,
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
plot_raw_current_data <-
  function(data,
           plot_treatment,
           plot_category,
           current_type,
           parameter,
           pruned,
           hormone_added,
           hormone_or_HFS_start_time,
           hormone_end_time = NULL,
           save_plot_png = "no",
           theme_options,
           treatment_colour_theme) {
    list_of_plots <- list()

    if (is.null(current_type) ||
      length(current_type) != 1L ||
      !current_type %in% c("eEPSC", "sEPSC")) {
      stop("'current_type' argument must be one of: 'eEPSC' or 'sEPSC'")
    }

    if (is.null(hormone_added) ||
      length(hormone_added) != 1L ||
      !is.character(hormone_added)) {
      stop(
        "\"hormone_added\" must be a character. Use \"HFS\" for high-frequency stimulation or \"Insulin\", \"CCK\", etc. for any hormones"
      )
    }

    if (!save_plot_png %in% c("yes", "no")) {
      stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
    }


    if (is.null(hormone_or_HFS_start_time) ||
      !is.numeric(hormone_or_HFS_start_time)) {
      stop(
        "\"hormone_or_HFS_start_time\" must be numeric (e.g. 5 for
        HFS or a hormone applied at five minutes into the recording)."
      )
    }

    if (!is.null(hormone_end_time) &
      !is.numeric(hormone_end_time)) {
      stop("\"hormone_end_time\" must be numeric
        (e.g. 25 for a hormone ending at 25 minutes).")
    }

    df <- data %>%
      dplyr::filter(.data$category == plot_category) %>%
      dplyr::filter(.data$treatment == plot_treatment)


    letters <- as.character(unique(unlist(df$letter)))

    plot_colour <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::pull(.data$colours)

    treatment_label <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::pull(.data$treatment)

    if (current_type == "eEPSC") {
      # The plots should go to specific folders depending on current type
      filepath <- "Figures/Evoked-currents/Output-individual-plots"

      allowed_parameters_list <- "\"P1\", \"mean_P1\", or \"PPR\""

      if (!parameter %in% c("P1", "mean_P1", "PPR")) {
        stop(
          "parameter must be ",
          allowed_parameters_list,
          " for current_type \"",
          current_type,
          "\". \nCheck parameter, current_type or data."
        )
      }

      if (parameter == "P1") {
        y_title <- "eEPSC Amplitude (pA)"

        if (pruned == "yes") {
          stop(
            "pruned = \"yes\"), but parameter = \"P1\". ",
            "\nDid you mean \"mean_P1\"?. "
          )
        }
      }

      if (parameter == "mean_P1") {
        y_title <- "eEPSC Amplitude (pA)"


        if (pruned == "no") {
          stop(
            "parameter = \"mean_P1\", but pruned = \"no\". ",
            "Are you trying to create a pruned plot? \nIf so, change pruned to \"yes\" ",
            "and ensure that you have specified the correct dataframe in the data argument"
          )
        }
      }

      if (parameter == "PPR") {
        y_title <- "Paired-pulse Ratio"
      }
    }


    if (current_type == "sEPSC") {
      filepath <- "Figures/Spontaneous-currents/Output-individual-plots"

      allowed_parameters_list <- "\"amplitude\" or \"frequency\""

      if (!parameter %in% c("amplitude", "frequency")) {
        stop(
          "parameter must be ",
          allowed_parameters_list,
          " for current_type \"",
          current_type,
          "\". \nCheck parameter, current_type or data."
        )
      }


      if (parameter == "amplitude") {
        y_title <- "sEPSC Amplitude (pA)"
      }

      if (parameter == "frequency") {
        y_title <- "sEPSC Frequency (Hz)"
      }
    }

    if (pruned == "yes") {
      annotation <- "_pruned"
    } else {
      annotation <- ""
    }

    # Pruned sEPSC amplitude plots use mean +/- SE, unlike the other plots


    for (i in letters) {
      plot_df <- df %>% dplyr::filter(.data$letter == i)
      if (current_type == "sEPSC" &
        pruned == "yes" &
        parameter == "amplitude") {
        y_title <- "sEPSC Amplitude (pA)"

        list_of_plots[[i]] <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = .data$time,
            y = .data$mean_amplitude,
            ymin = .data$mean_amplitude - .data$se,
            ymax = .data$mean_amplitude + .data$se
          )
        )
      } else {
        list_of_plots[[i]] <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$time, y = .data[[parameter]]))
      }

      list_of_plots[[i]] <- list_of_plots[[i]] +
        ggplot2::ggtitle(
          paste("Recording", i),
          subtitle = paste(
            "Treatment:",
            treatment_label,
            " Sex:",
            unique(plot_df$sex)
          )
        ) +
        ggplot2::labs(x = "Time (min)", y = y_title) +
        patchclampplotteR_theme()

      if (current_type == "sEPSC" &
        pruned == "yes" &
        parameter == "amplitude") {
        list_of_plots[[i]] <- list_of_plots[[i]] +
          ggplot2::geom_pointrange(
            shape = if (unique(plot_df$sex) == "Male") {
              as.numeric(theme_options["male_shape", "value"])
            } else {
              as.numeric(theme_options["female_shape", "value"])
            },
            colour = plot_colour,
            size = 1,
            alpha = 1
          )
      } else {
        list_of_plots[[i]] <- list_of_plots[[i]] +
          ggplot2::geom_point(
            shape = if (unique(plot_df$sex) == "Male") {
              as.numeric(theme_options["male_shape", "value"])
            } else {
              as.numeric(theme_options["female_shape", "value"])
            },
            colour = plot_colour,
            size = if (current_type == "sEPSC" & pruned == "no") {
              1
            } else {
              3.5
            },
            alpha = if (pruned == "yes") {
              1
            } else {
              0.7
            }
          )
      }

      # Get limits of x- and y-axes
      ymax <- ggplot2::layer_scales(list_of_plots[[i]])$y$get_limits()[2]
      xmax <- ggplot2::layer_scales(list_of_plots[[i]])$x$get_limits()[2]
      ymax2 <- ggplot2::layer_scales(list_of_plots[[i]])$y$get_limits()[2]

      # If hormone_added = Insulin, CCK, i.e. anything other than "HFS" (high
      # frequency stimulation), add an annotated line over the application
      # period:

      if (hormone_added != "HFS") {
        list_of_plots[[i]] <-
          list_of_plots[[i]] +
          ggplot2::annotate(
            geom = "segment",
            x = hormone_or_HFS_start_time,
            xend = if (is.null(hormone_end_time)) {
              xmax
            } else {
              hormone_end_time
            },
            y = ymax + 0.1 * ymax,
            yend = ymax + 0.1 * ymax,
            colour = theme_options["line_col", "value"],
            linewidth = 0.6
          )

        list_of_plots[[i]] <-
          list_of_plots[[i]] +
          ggplot2::annotate(
            geom = "text",
            x = hormone_or_HFS_start_time,
            y = ymax2 + 0.16 * ymax2,
            label = hormone_added,
            size = 4,
            hjust = 0
          )
      }

      # If hormone_added == HFS (experiments involving HFS)
      # add a labelled arrow at 5 minutes:

      if (hormone_added == "HFS") {
        list_of_plots[[i]] <-
          list_of_plots[[i]] +
          ggplot2::annotate(
            geom = "segment",
            x = hormone_or_HFS_start_time,
            y = ymax + 0.22 * ymax,
            xend = hormone_or_HFS_start_time,
            yend = ymax + 0.10 * ymax,
            arrow = grid::arrow(type = "closed", length = grid::unit(0.02, "npc"))
          )


        list_of_plots[[i]] <-
          list_of_plots[[i]] +
          ggplot2::annotate(
            geom = "text",
            x = hormone_or_HFS_start_time,
            y = ymax + 0.27 * ymax,
            label = "HFS",
            size = 3.5,
            hjust = 0.5
          )
      }

      if (save_plot_png == "yes") {
        ggplot2::ggsave(
          list_of_plots[[i]],
          path = here::here(filepath),
          file = paste0(i, annotation, ".png"),
          width = 7,
          height = 5,
          units = "in",
          dpi = 300
        )
      }
    }


    return(list_of_plots)
  }

#' Make a summary plot for a specific treatment
#'
#' This function enables you to create a scatterplot of mean evoked (or
#' spontaneous) current amplitude over over time in minutes. The data are
#' summarized by treatment and sex, and averaged per minute. The data are
#' presented as mean +/- the standard error.
#'
#' @inheritParams plot_raw_current_data
#' @param data A dataframe containing pruned summary data for all cells. This is
#'   the third element of the list generated from [make_pruned_EPSC_data()].
#' @param include_representative_trace A character ("yes" or "no") describing if
#'   a representative trace should be included as an overlay to the plot. This
#'   pulls from a png file stored in `Figures/Representative-Traces/`". Please
#'   specify the file-name in `representative_trace_filename`.
#' @param representative_trace_filename A character value describing the
#'   filename of the representative trace. This should be the name of a .png
#'   file stored within the subfolder `Figures/Representative-Traces/`.
#' @param signif_stars A character ("yes" or "no") describing if significance
#'   stars should be included as an overlay in the plot. If "yes", you must
#'   specify a dataframe containing the results of a t-test, which is generated
#'   using [perform_t_tests_for_summary_plot()].
#' @param t_test_df A dataframe of t-test results, which has been generated
#'   using [perform_t_tests_for_summary_plot()]. Important note! The t-test
#'   dataframe must be filtered to match the same conditions in the `data`
#'   argument, or the significance stars will be misleading.
#' @param large_axis_text A character ("yes" or "no"). If "yes", a ggplot theme
#'   layer will be applied which increases the size of the axis text.
#' @param shade_intervals A character ("yes" or "no"). If "yes", a ggplot theme
#'   layer will be applied which adds lightly shaded rectangles to highlight
#'   5-minute intervals.
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file exported to `Figures/Evoked-currents/Output-summary-plots` or
#'   `Figures/Spontaneous-currents/Output-summary-plots`, depending on the
#'   `current_type`. The .png filename will be in this format:
#'   "Summary-plot-`plot_treatment`-category-`plot_category`-`file_name_ending`-`text_size`.png".
#'   The `text_size` will only be added on if you are using `large_axis_text`
#'   ("LARGE" will be included in the filename). The `file_name_ending` will be
#'   automatically added on for spontaneous current data to specify what
#'   parameter is plotted (e.g. "raw_amplitude").
#'
#'   An example filename is: "Summary-plot-Control-category-2.png" or
#'   "Summary-plot-Control-category-2_raw_amplitude.png" for spontaneous
#'   currents.
#'
#' @export
#'
#' @seealso [perform_t_tests_for_summary_plot()] which produces the significance
#'   stars appended to the plot.
#' @seealso [make_pruned_EPSC_data()] for the function that will produce the
#'   summary data used in this plot.
#'
#' @examples
#'
#' plot_summary_current_data(
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   data = sample_pruned_eEPSC_df$all_cells,
#'   current_type = "eEPSC",
#'   parameter = "amplitude",
#'   include_representative_trace = "no",
#'   signif_stars = "yes",
#'   t_test_df = sample_eEPSC_t_test_df,
#'   hormone_added = "Insulin",
#'   large_axis_text = "no",
#'   shade_intervals = "no",
#'   hormone_or_HFS_start_time = 5,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
plot_summary_current_data <- function(plot_category,
                                      plot_treatment,
                                      data,
                                      current_type,
                                      parameter,
                                      hormone_added,
                                      hormone_or_HFS_start_time,
                                      include_representative_trace = "no",
                                      representative_trace_filename,
                                      t_test_df,
                                      signif_stars = "no",
                                      large_axis_text = "no",
                                      shade_intervals = "no",
                                      theme_options,
                                      treatment_colour_theme,
                                      save_plot_png = "no") {
  if (is.null(current_type) ||
    length(current_type) != 1L ||
    !current_type %in% c("eEPSC", "sEPSC")) {
    stop("'current_type' argument must be one of: 'eEPSC' or 'sEPSC'")
  }

  if (is.null(hormone_added) ||
    length(hormone_added) != 1L ||
    !is.character(hormone_added)) {
    stop(
      "\"hormone_added\" must be a character. Use \"HFS\" for high-frequency stimulation or \"Insulin\", \"CCK\", etc. for any hormones"
    )
  }

  if (!save_plot_png %in% c("yes", "no")) {
    stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
  }

  if (!signif_stars %in% c("yes", "no")) {
    stop("'signif_stars' argument must be one of: 'yes' or 'no'")
  }

  if (!include_representative_trace %in% c("yes", "no")) {
    stop("'include_representative_trace' argument must be one of: 'yes' or 'no'")
  }

  if (include_representative_trace == "yes") {
    if (is.null(representative_trace_filename) ||
      !is.character(representative_trace_filename)) {
      stop(
        "'include_representative_trace' is 'yes' but the filename is not
           specified or it is not a character value. Please define the filename
           as a character in
        'representative_trace_filename'"
      )
    }
  }

  if (signif_stars == "yes" & is.null(t_test_df)) {
    stop(
      "signif_stars == 'yes' but you did not specify a dataframe
      containing t-test summary data"
    )
  }


  if (is.null(hormone_or_HFS_start_time) ||
    !is.numeric(hormone_or_HFS_start_time)) {
    stop(
      "\"hormone_or_HFS_start_time\" must be numeric
      (e.g. 5 for HFS or a hormone applied at five minutes into the recording)."
    )
  }

  df <-
    data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment)

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  plot_colour_pale <- treatment_colour_theme %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::pull(.data$very_pale_colours)

  if (current_type == "eEPSC") {
    allowed_parameters_list <- "\"amplitude\""

    if (!parameter %in% c("amplitude")) {
      stop(
        "parameter must be ",
        allowed_parameters_list,
        " for current_type \"",
        current_type,
        "\". \nCheck parameter, current_type or data."
      )
    }

    if (parameter == "amplitude") {
      y_var <- "mean_P1_all_cells"
      se_var <- "se_P1_all_cells"
      filepath <- "Figures/Evoked-currents/Output-summary-plots"
      file_name_ending <- ""

      if (large_axis_text == "yes") {
        y_title <- "eEPSC Amplitude\n(% Baseline)"
      } else {
        y_title <- "eEPSC Amplitude (% Baseline)"
      }
    }
  }


  if (current_type == "sEPSC") {
    filepath <- "Figures/Spontaneous-currents/Output-summary-plots"

    allowed_parameters_list <- "\"amplitude\", \"raw_amplitude\",
    \"raw_frequency\", or \"frequency\""

    if (!parameter %in% c(
      "amplitude",
      "raw_amplitude",
      "frequency",
      "raw_frequency"
    )) {
      stop(
        "parameter must be ",
        allowed_parameters_list,
        " for current_type \"",
        current_type,
        "\". \nCheck parameter, current_type or data."
      )
    }


    if (parameter == "amplitude") {
      y_var <- "mean_all_amplitude"
      se_var <- "se_amplitude"
      file_name_ending <- paste0("_", parameter)

      if (large_axis_text == "yes") {
        y_title <- "sEPSC Amplitude\n(% Baseline)"
      } else {
        y_title <- "sEPSC Amplitude (% Baseline)"
      }
    }

    if (parameter == "raw_amplitude") {
      y_var <- "mean_all_raw_amplitude"
      se_var <- "se_raw_amplitude"
      file_name_ending <- paste0("_", parameter)

      y_title <- "sEPSC Amplitude (pA)"
    }

    if (parameter == "frequency") {
      y_var <- "mean_all_frequency"
      se_var <- "se_frequency"
      file_name_ending <- paste0("_", parameter)

      if (large_axis_text == "yes") {
        y_title <- "sEPSC Frequency\n(% Baseline)"
      } else {
        y_title <- "sEPSC Frequency (% Baseline)"
      }
    }

    if (parameter == "raw_frequency") {
      y_var <- "mean_all_raw_frequency"
      se_var <- "se_frequency"
      file_name_ending <- paste0("_", parameter)

      y_title <- "sEPSC Frequency (Hz)"
    }
  }

  treatment_plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$time,
      y = .data[[y_var]],
      ymin = .data[[y_var]] - .data[[se_var]],
      ymax = .data[[y_var]] + .data[[se_var]]
    ))

  if (shade_intervals == "yes") {
    treatment_plot <- treatment_plot +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = 5,
          xmax = 10,
          ymin = -5,
          ymax = as.numeric(theme_options["y_axis_limit", "value"])
        ),
        fill = theme_options["rectangle_shading_colour", "value"]
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = 15,
          xmax = 20,
          ymin = -5,
          ymax = as.numeric(theme_options["y_axis_limit", "value"])
        ),
        fill = theme_options["rectangle_shading_colour", "value"]
      )
  }

  treatment_plot <- treatment_plot +
    ggplot2::geom_pointrange(
      ggplot2::aes(color = .data$sex, shape = .data$sex),
      size = if (large_axis_text == "yes") {
        1.3
      } else {
        0.9
      },
      alpha = 1,
      position = ggplot2::position_dodge(width = if (current_type == "eEPSC") {
        0.3
      } else {
        0
      })
    ) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = c(0, as.numeric(theme_options["y_axis_limit", "value"]))) +
    ggplot2::labs(
      x = "Time (min)",
      y = y_title,
      shape = "Sex",
      color = "Sex"
    ) +
    patchclampplotteR_theme()

  if (large_axis_text == "yes") {
    treatment_plot <- treatment_plot +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 24, face = "plain"),
        legend.title = ggplot2::element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.17, 0.13),
        legend.text = ggplot2::element_text(size = 14),
        legend.key.spacing.y = grid::unit(0.5, "cm"),
        legend.background = ggplot2::element_rect(fill = NA)
      )
  }

  # Nested if statements enable correct legend labels even if only one sex is present

  if (is.na(df$n[df$sex == "Female"][1])) {
    treatment_plot <- treatment_plot +
      ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["male_shape", "value"])), labels = c((paste0(
        "Males, n = ", df$n[df$sex == "Male"][1]
      )))) +
      ggplot2::scale_color_manual(values = c(plot_colour), labels = c((paste0(
        "Males, n = ", df$n[df$sex == "Male"][1]
      ))))
  } else if (is.na(df$n[df$sex == "Male"][1])) {
    treatment_plot <- treatment_plot +
      ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["female_shape", "value"])), labels = c((paste0(
        "Females, n = ", df$n[df$sex == "Female"][1]
      )))) +
      ggplot2::scale_color_manual(
        values = c(plot_colour_pale),
        labels = c((paste0(
          "Females, n = ", df$n[df$sex == "Female"][1]
        )))
      )
  } else {
    treatment_plot <- treatment_plot +
      ggplot2::scale_shape_manual(
        values = c(as.numeric(theme_options["female_shape", "value"]), as.numeric(theme_options["male_shape", "value"])),
        labels = c((paste0(
          "Females, n = ", df$n[df$sex == "Female"][1]
        )), (paste0(
          "Males, n = ", df$n[df$sex == "Male"][1]
        )))
      ) +
      ggplot2::scale_color_manual(
        values = c(plot_colour_pale, plot_colour),
        labels = c((paste0(
          "Females, n = ", df$n[df$sex == "Female"][1]
        )), (paste0(
          "Males, n = ", df$n[df$sex == "Male"][1]
        )))
      )
  }

  # Get limits of x- and y-axes
  ymax <- as.numeric(theme_options["y_axis_limit", "value"]) - 25
  xmax <-
    ggplot2::layer_scales(treatment_plot)$x$get_limits()[2]

  # If hormone_added = Insulin, CCK, i.e. anything other than "HFS" (high frequency stimulation),
  # add an annotated line over the application period:

  if (hormone_added != "HFS") {
    treatment_plot <-
      treatment_plot +
      ggplot2::annotate(
        geom = "segment",
        x = hormone_or_HFS_start_time,
        xend = xmax,
        y = ymax,
        yend = ymax,
        colour = theme_options["line_col", "value"],
        linewidth = 0.5
      ) +
      ggplot2::annotate(
        geom = "text",
        x = hormone_or_HFS_start_time,
        y = ymax + 0.06 * ymax,
        label = hormone_added,
        size = if (large_axis_text == "yes") {
          6
        } else {
          4
        },
        hjust = 0
      )
  }

  # If plot_category = 1 or 3 (experiments involving HFS) add an annotation arrow at 5 minutes
  if (hormone_added == "HFS") {
    # Get limits of x- and y-axes
    ymax <- ggplot2::layer_scales(treatment_plot)$y$get_limits()[2]
    xmax <- ggplot2::layer_scales(treatment_plot)$x$get_limits()[2]
    ymax2 <- ggplot2::layer_scales(treatment_plot)$y$get_limits()[2]

    # Add an arrow showing when HFS was applied (x = 5 min)
    treatment_plot <-
      treatment_plot +
      ggplot2::annotate(
        geom = "segment",
        x = hormone_or_HFS_start_time,
        y = ymax + 0.22 * ymax,
        xend = hormone_or_HFS_start_time,
        yend = ymax + 0.10 * ymax,
        arrow = grid::arrow(type = "closed", length = grid::unit(0.02, "npc"))
      )

    # Add "HFS" text label
    treatment_plot <-
      treatment_plot +
      ggplot2::annotate(
        geom = "text",
        x = hormone_or_HFS_start_time,
        y = ymax + 0.27 * ymax,
        label = "HFS",
        size = 3.5,
        hjust = 0.5
      )
  }

  if (signif_stars == "yes") {
    treatment_plot <- treatment_plot +
      ggplot2::geom_text(
        data = t_test_df %>% dplyr::filter(.data$treatment == plot_treatment),
        ggplot2::aes(
          x = .data$asterisk_time,
          y = as.numeric(theme_options["y_axis_limit", "value"]) - 50,
          label = .data$significance_stars
        ),
        inherit.aes = FALSE,
        size = 8
      )
  }

  if (include_representative_trace == "yes") {
    representative_trace_file <- paste0(
      "Figures/Representative-Traces/Category-",
      plot_category,
      "-",
      plot_treatment,
      "-Trace.png"
    )

    # Representative traces must be saved as PNGS with the following file name convention:
    # Category-[number]-[treatment]-Trace.png or a warning will display about a missing file
    # e.g. "Category-2-Control-Trace.png"

    if (file.exists(here::here(representative_trace_file))) {
      representative_trace <- png::readPNG(here::here(representative_trace_file)) %>% grid::rasterGrob()

      treatment_plot <- treatment_plot +
        ggplot2::annotation_custom(
          representative_trace,
          xmin = 1,
          xmax = 8,
          ymin = 0,
          ymax = 40
        )
    } else {
      warning(
        "The file here::here(Figures/Representative-Traces/Category-",
        plot_category,
        "-",
        plot_treatment,
        "-Trace.png) does not exist. Plotting without a representative trace."
      )
    }
  }


  if (large_axis_text == "yes") {
    text_size <- "_LARGE_text"
  } else {
    text_size <- ""
  }

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      treatment_plot,
      path = here::here(filepath),
      file = paste0(
        "Summary-plot-",
        plot_treatment,
        "-category-",
        plot_category,
        file_name_ending,
        text_size,
        ".png"
      ),
      width = 10,
      height = 7,
      units = "in",
      dpi = 300,
      scaling = 1.25
    )
  }
  treatment_plot
}

#' Plot variance comparison for a treatment
#'
#' `plot_variance_comparison_data()` creates a connected  plot with time as a
#' categorical variable (i.e. baseline/before and after) on the x-axis and the
#' variance measure on the y-axis. There are also lines
#' connecting the "before" data point to the "after" data point for each letter.
#'
#' The function will perform a paired wilcox test and add brackets with
#' significance stars through `ggsignif::geom_signif()`.
#'
#' This allows you to visually determine if a change in synaptic plasticity is
#' due to a pre- or post-synaptic mechanism. For more information, please see
#' [Huijstee & Kessels (2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).
#'
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_baseline_data
#'
#' @param data A dataframe generated using [make_variance_data()].
#' @param variance_measure A character value ("cv" or "VMR"). The variance
#'   measures can be either the inverse coefficient of variation squared
#'   (`variance_measure == "cv"`) or variance-to-mean ratio (`variance_measure
#'   == "VMR"`).
#' @param post_hormone_interval A character value specifying the interval used
#'   for the data points after a hormone or protocol was applied. This must
#'   match the `post_hormone_interval` used in [make_variance_data()].
#' @param baseline_label A character value for the x-axis label applied to the
#'   pre-hormone state. Defaults to "Baseline".
#' @param post_modification_label A character value for x-axis label applied to
#'   the post-hormone or post-protocol state. Defaults to "Post-hormone" but you
#'   will likely change this to the hormone or protocol name.
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file exported to `Figures/Evoked-currents/Variance-plots`. The plot
#'   will be named in the form of
#'   "Variance-comparison-category-`plot_category`-`plot_treatment`-`variance_measure`.png".
#'   An example filename is "Variance-comparison-category-2-Control-cv.png".
#'
#' @export
#'
#' @examples
#' plot_variance_comparison_data(
#'   data = sample_eEPSC_variance_df,
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   large_axis_text = "no",
#'   variance_measure = "cv",
#'   baseline_interval = "t0to5",
#'   post_hormone_interval = "t20to25",
#'   post_modification_label = "Insulin",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
#'
plot_variance_comparison_data <- function(data,
                                          plot_category,
                                          plot_treatment,
                                          large_axis_text = "no",
                                          variance_measure,
                                          baseline_interval,
                                          post_hormone_interval,
                                          baseline_label = "Baseline",
                                          post_modification_label,
                                          treatment_colour_theme,
                                          save_plot_png = "no",
                                          theme_options) {
  if (is.null(post_hormone_interval) ||
    !is.character(post_hormone_interval)) {
    stop("'post_hormone_interval' must be a character (e.g. \"t20to25\")")
  }


  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  variance_comparison_data <- data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment)

  allowed_parameters_list <- "\"cv\" or \"VMR\""


  if (!variance_measure %in% c("cv", "VMR")) {
    stop("parameter must be ", allowed_parameters_list)
  }


  if (variance_measure == "cv") {
    variance_comparison_plot <- variance_comparison_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$interval,
          y = .data$cv_inverse_square,
          group = .data$letter
        )
      ) +
      ggplot2::labs(y = "1/CV<sup>2</sup>") +
      ggsignif::geom_signif(
        comparisons = list(c(
          baseline_interval, post_hormone_interval
        )),
        test = "wilcox.test",
        test.args = list(paired = TRUE),
        map_signif_level = c(
          "***" = 0.001,
          "**" = 0.01,
          "*" = 0.05,
          "ns" = 2
        ),
        vjust = -0.3,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = 0.4
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
  }

  if (variance_measure == "VMR") {
    variance_comparison_plot <- variance_comparison_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$interval,
        y = .data$VMR,
        group = .data$letter
      )) +
      ggplot2::labs(y = "VMR") +
      ggsignif::geom_signif(
        comparisons = list(c(
          baseline_interval, post_hormone_interval
        )),
        test = "wilcox.test",
        test.args = list(paired = TRUE),
        map_signif_level = c(
          "***" = 0.001,
          "**" = 0.01,
          "*" = 0.05,
          "ns" = 2
        ),
        vjust = -0.3,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = 0.4
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
  }

  variance_comparison_plot <- variance_comparison_plot +
    ggplot2::geom_point(color = theme_options["connecting_line_colour", "value"], size = 1.8) +
    ggplot2::geom_line(color = theme_options["connecting_line_colour", "value"], linewidth = 0.4) +
    ggplot2::labs(x = NULL) +
    ggplot2::scale_x_discrete(labels = c(baseline_label, post_modification_label)) +
    patchclampplotteR_theme() +
    ggplot2::theme(axis.title.y = ggtext::element_markdown())

  if (variance_measure == "cv") {
    variance_comparison_plot <- variance_comparison_plot +
      ggplot2::annotate(
        geom = "segment",
        x = baseline_interval,
        xend = post_hormone_interval,
        y = variance_comparison_data$mean_cv_inverse_square[variance_comparison_data$interval == baseline_interval][1],
        yend = variance_comparison_data$mean_cv_inverse_square[variance_comparison_data$interval == post_hormone_interval][1],
        color = plot_colour,
        linewidth = 1.2
      ) +
      ggplot2::annotate(
        geom = "point",
        x = baseline_interval,
        y = variance_comparison_data$mean_cv_inverse_square[variance_comparison_data$interval == baseline_interval][1],
        color = plot_colour,
        size = 2.5
      ) +
      ggplot2::annotate(
        geom = "point",
        x = post_hormone_interval,
        y = variance_comparison_data$mean_cv_inverse_square[variance_comparison_data$interval == post_hormone_interval][1],
        color = plot_colour,
        size = 2.5
      )
  }

  if (variance_measure == "VMR") {
    variance_comparison_plot <- variance_comparison_plot +
      ggplot2::annotate(
        geom = "segment",
        x = baseline_interval,
        xend = post_hormone_interval,
        y = variance_comparison_data$mean_VMR[variance_comparison_data$interval == baseline_interval][1],
        yend = variance_comparison_data$mean_VMR[variance_comparison_data$interval == post_hormone_interval][1],
        color = plot_colour,
        linewidth = 1.2
      ) +
      ggplot2::annotate(
        geom = "point",
        x = baseline_interval,
        y = variance_comparison_data$mean_VMR[variance_comparison_data$interval == baseline_interval][1],
        color = plot_colour,
        size = 2.5
      ) +
      ggplot2::annotate(
        geom = "point",
        x = post_hormone_interval,
        y = variance_comparison_data$mean_VMR[variance_comparison_data$interval == post_hormone_interval][1],
        color = plot_colour,
        size = 2.5
      )
  }


  if (large_axis_text == "yes") {
    variance_comparison_plot <- variance_comparison_plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 10)),
        axis.title.y = ggtext::element_markdown(size = 28, face = "plain")
      )
  }

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = variance_comparison_plot,
      path = here::here("Figures/Evoked-currents/Variance-plots"),
      file = paste0(
        "Variance-comparison-category-",
        plot_category,
        "-",
        plot_treatment,
        "-",
        variance_measure,
        ".png"
      ),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(variance_comparison_plot)
}


#' Make a plot of coefficient of variation over time
#'
#' `plot_cv_data()` enables you to save a plot of the coefficient of variation
#' in evoked current amplitudes over time.
#'
#' @inheritParams plot_raw_current_data
#' @param data A dataframe of the pruned current data for all cells. This is the
#'   third dataframe in the list generated from [make_pruned_EPSC_data()].
#'
#' @export
#'
#' @returns A ggplot object. If `save_plot_png == "yes"` it will also generate a
#'   .png file in the folder `Figures/Evoked-currents/CV` relative to the
#'   project directory. The treatment will be included with the filename.
#'
#' @examples
#' plot_cv_data(
#'   data = sample_pruned_eEPSC_df$all_cells,
#'   plot_treatment = "Control",
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   save_plot_png = "no"
#' )
#'
#' @seealso [plot_variance_comparison_data()] to make plots of inverse
#'   coefficient of variation squared and VMR, which are useful to determine if
#'   a mechanism is pre- or post-synaptic.


plot_cv_data <- function(data,
                         plot_treatment = "Control",
                         treatment_colour_theme,
                         theme_options,
                         save_plot_png = "no") {
  if (!save_plot_png %in% c("yes", "no")) {
    stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
  }


  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  cv_plot <- data %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$cv_P1_all_cells)) +
    ggplot2::geom_point(color = plot_colour) +
    ggplot2::labs(x = "Time (min)", y = "CV") +
    patchclampplotteR_theme()

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = cv_plot,
      path = here::here("Figures/Evoked-currents/CV"),
      file = paste0("CV_plot", plot_treatment, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 600
    )
  }

  return(cv_plot)
}



#' Make a PPR plot for a single treatment
#'
#' `plot_PPR_data_one_treatment()` creates a categorical scatter plot with
#' experimental state (i.e. baseline/before and after) on the x-axis and the
#' paired-pulse ratio (PPR) on the y-axis. There are also lines connecting the
#' "before" data point to the "after" data point for each letter.
#'
#' The function will perform a paired wilcox test and add brackets with
#' significance stars through `ggsignif::geom_signif()`.
#'
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_summary_current_data
#' @inheritParams plot_variance_comparison_data
#'
#' @param data Paired pulse ratio data generated from [make_PPR_data()].
#'
#' @export
#'
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Evoked-currents/PPR` relative to the
#'   project directory. The treatment will be included in the filename.
#'
#' @examples
#' plot_PPR_data_one_treatment(
#'   data = sample_PPR_df,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   large_axis_text = "no",
#'   baseline_label = "Baseline",
#'   post_modification_label = "Insulin",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
#'
#' @seealso [plot_PPR_data_multiple_treatments()] to plot changes in PPR for multiple treatments.

plot_PPR_data_one_treatment <- function(data,
                                        plot_treatment = "Control",
                                        plot_category,
                                        large_axis_text = "no",
                                        baseline_label = "Baseline",
                                        post_modification_label = "Post-hormone",
                                        treatment_colour_theme,
                                        theme_options,
                                        save_plot_png = "no") {
  if (!large_axis_text %in% c("yes", "no")) {
    stop("'large_axis_text' argument must be one of: 'yes' or 'no'")
  }

  if (!save_plot_png %in% c("yes", "no")) {
    stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)


  PPR_one_plot <- data %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::mutate(
      state = dplyr::case_match(
        .data$state,
        "Baseline" ~ baseline_label,
        "Post-modification" ~ post_modification_label
      )
    ) %>%
    dplyr::group_by(.data$treatment, .data$state, .data$letter, .data$sex) %>%
    dplyr::summarize(mean_PPR_cell = mean(.data$PPR), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$state,
      y = .data$mean_PPR_cell,
      shape = .data$sex
    )) +
    ggplot2::geom_point(
      size = 4,
      color = plot_colour,
      position = ggplot2::position_jitter(width = 0.04, height = 0),
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$letter),
      color = plot_colour,
      linewidth = as.numeric(theme_options["connecting_line_width_PPR", "value"]),
      alpha = 0.3
    ) +
    ggplot2::stat_summary(
      fun.data = ggplot2::mean_se,
      geom = "pointrange",
      color = theme_options["mean_point_colour", "value"],
      size = as.numeric(theme_options["mean_point_size", "value"]) + 0.2,
      alpha = 1,
      position = ggplot2::position_nudge(x = -0.04),
      show.legend = FALSE
    ) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::coord_cartesian(ylim = c(0, 3)) +
    patchclampplotteR_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(b = 5, t = 5))) +
    ggplot2::labs(x = NULL, y = "Paired pulse ratio", shape = NULL) +
    ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["female_shape", "value"]), as.numeric(theme_options["male_shape", "value"]))) +
    ggsignif::geom_signif(
      comparisons = list(c(
        baseline_label, post_modification_label
      )),
      test = "t.test",
      test.args = list(paired = TRUE),
      map_signif_level = c(
        "***" = 0.001,
        "**" = 0.01,
        "*" = 0.05,
        "ns" = 2
      ),
      vjust = -0.3,
      textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
      size = 0.4,
      y_position = 2.5
    )

  if (large_axis_text == "yes") {
    PPR_one_plot <- PPR_one_plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(size = 28, face = "plain"),
        legend.text = ggplot2::element_text(size = 18),
        legend.key.spacing.y = grid::unit(0.5, "cm")
      )
  }


  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = PPR_one_plot,
      path = here::here("Figures/Evoked-currents/PPR"),
      file = paste0("PPR_comparison-", plot_treatment, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }


  return(PPR_one_plot)
}


#' Make a PPR plot for multiple treatments
#'
#' `plot_PPR_data_multiple_treatments()` creates a categorical scatter plot with
#' experimental state (i.e. grouped as baseline/before and after) and treatment
#' on the x-axis, and the paired-pulse ratio (PPR) on the y-axis. There are also
#' lines connecting the "before" data point to the "after" data point for each
#' letter. It is the same as [plot_PPR_data_one_treatment()] but for more
#' than one treatment.
#'
#' @inheritParams plot_baseline_data
#' @inheritParams plot_variance_comparison_data
#' @inheritParams plot_raw_current_data

#' @param data Paired pulse ratio data generated from [make_PPR_data()].
#'
#' @export
#'
#' @returns A ggplot object. If save_plot_png is defined as "yes", it will also
#'   generate a .png file in the folder `Figures/Evoked-currents/PPR` relative
#'   to the project directory.
#'
#' @examples
#' plot_PPR_data_multiple_treatments(
#'   data = sample_PPR_df,
#'   include_all_treatments = "yes",
#'   plot_category = 2,
#'   baseline_label = "B",
#'   post_modification_label = "I",
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
#' @seealso [plot_PPR_data_one_treatment()] to plot changes in PPR for a single treatment.

plot_PPR_data_multiple_treatments <- function(data,
                                              include_all_treatments = "yes",
                                              list_of_treatments = NULL,
                                              plot_category,
                                              baseline_label,
                                              post_modification_label,
                                              treatment_colour_theme,
                                              theme_options,
                                              filename_suffix = "",
                                              save_plot_png = "no") {
  if (!include_all_treatments %in% c("yes", "no")) {
    stop("'include_all_treatments' argument must be one of: 'yes' or 'no'")
  }

  if (!save_plot_png %in% c("yes", "no")) {
    stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
  }

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      warning(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is NULL.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    if (!is.character(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is not a character object.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }

  PPR_summary_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::mutate(
      state = dplyr::case_match(
        .data$state,
        "Baseline" ~ baseline_label,
        "Post-modification" ~ post_modification_label
      ),
      treatment = stringr::str_replace_all(
        .data$treatment,
        stats::setNames(treatment_info$display_names, treatment_info$treatment)
      ),
      treatment = factor(.data$treatment, levels = treatment_info$display_names)
    ) %>%
    dplyr::group_by(.data$treatment, .data$state, .data$letter, .data$sex) %>%
    dplyr::summarize(mean_PPR = mean(.data$PPR)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$state, y = .data$mean_PPR)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$treatment),
      size = 2,
      position = ggplot2::position_jitter(0.01),
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = .data$treatment, group = .data$letter),
      linewidth = as.numeric(theme_options["connecting_line_width_PPR", "value"]),
      alpha = 0.3
    ) +
    ggsignif::geom_signif(
      comparisons = list(c(
        baseline_label, post_modification_label
      )),
      test = "t.test",
      test.args = list(paired = TRUE),
      map_signif_level = c(
        "***" = 0.001,
        "**" = 0.01,
        "*" = 0.05,
        "ns" = 2
      ),
      vjust = -0.3,
      textsize = 4,
      size = 0.3,
      margin_top = 0.1,
      extend_line = 0.03
    ) +
    ggplot2::facet_wrap(
      ~ .data$treatment,
      ncol = length(treatment_info$treatment),
      strip.position = "bottom"
    ) +
    ggplot2::scale_color_manual(
      breaks = treatment_info$display_names,
      values = treatment_info$colours
    ) +
    patchclampplotteR_theme() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 10),
      strip.placement = "outside",
      panel.spacing.x = grid::unit(2, "mm"),
      legend.position = "none"
    ) +
    ggplot2::stat_summary(
      fun.data = ggplot2::mean_se,
      geom = "pointrange",
      color = theme_options["mean_point_colour", "value"],
      size = as.numeric(theme_options["mean_point_size", "value"]) + 0.25,
      alpha = 0.9
    ) +
    ggplot2::labs(x = NULL, y = "Paired pulse ratio") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = PPR_summary_plot,
      path = here::here("Figures/Evoked-currents/PPR"),
      file = paste0("PPR_Summary_plot", filename_suffix, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(PPR_summary_plot)
}


#' Visually compare spontaneous current parameters
#'
#' [plot_spontaneous_current_parameter_comparison()] is a useful function to see
#' how spontaneous current amplitude or frequency change after adding a hormone.
#' This function produces sina plots (with raw datapoints on top) for the raw
#' amplitude (or raw frequency) for two time intervals: the baseline interval,
#' and a user-specified interval after a hormone or other modification has been
#' applied.
#'
#' @param data Summary data for spontaneous currents generated using [make_summary_EPSC_data()] where `current_type == "sEPSC"`.
#' @param parameter A character value ("raw_amplitude" or "raw_frequency") only.
#'   Normalized amplitude and frequency are not available because all baseline
#'   values are 100.
#' @inheritParams plot_variance_comparison_data
#' @inheritParams plot_baseline_data
#' @inheritParams plot_raw_current_data
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' plot_spontaneous_current_parameter_comparison(
#'   data = sample_summary_sEPSC_df,
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   parameter = "raw_amplitude",
#'   large_axis_text = "no",
#'   hormone_added = "Insulin",
#'   baseline_interval = "t0to5",
#'   post_hormone_interval = "t20to25",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
#'
plot_spontaneous_current_parameter_comparison <-
  function(data,
           plot_category,
           plot_treatment,
           parameter,
           baseline_interval,
           post_hormone_interval,
           large_axis_text = "no",
           hormone_added,
           treatment_colour_theme,
           theme_options,
           save_plot_png) {
    if (is.null(baseline_interval) ||
      !is.character(baseline_interval)) {
      stop("'baseline_interval' must be a character (e.g. \"t0to5\" or \"t0to3\")")
    }

    if (!save_plot_png %in% c("yes", "no")) {
      stop("'save_plot_png' argument must be one of: 'yes' or 'no'")
    }

    if (is.null(post_hormone_interval) ||
      !is.character(post_hormone_interval)) {
      stop("'post_hormone_interval' must be a character (e.g. \"t20to25\")")
    }

    allowed_parameters_list <- "\"raw_amplitude\", or \"raw_frequency\""

    if (!parameter %in% c("raw_amplitude", "raw_frequency")) {
      stop(
        "parameter must be ",
        allowed_parameters_list,
        " because transformed data are all 100 during the baseline."
      )
    }

    plot_colour <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::pull(.data$colours)

    sEPSC_comparison_plot_data <- data %>%
      dplyr::filter(.data$category == plot_category &
        .data$treatment == plot_treatment) %>%
      dplyr::filter(.data$interval == baseline_interval |
        .data$interval == post_hormone_interval)

    if (parameter == "raw_amplitude") {
      y_var <- "mean_raw_amplitude"
      y_title <- "sEPSC Amplitude (pA)"
    }

    if (parameter == "raw_frequency") {
      y_var <- "mean_raw_frequency"
      y_title <- "sEPSC Frequency (Hz)"
    }

    sEPSC_comparison_plot <- sEPSC_comparison_plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$interval, y = .data[[y_var]])) +
      ggplot2::labs(x = NULL, y = y_title) +
      ggplot2::scale_x_discrete(labels = c("Baseline", hormone_added)) +
      ggplot2::geom_violin(
        fill = theme_options["gray_shading_colour", "value"],
        color = NA,
        scale = "width",
        width = 0.2
      ) +
      ggforce::geom_sina(
        bw = 12,
        alpha = 0.8,
        maxwidth = 0.3,
        size = 2,
        color = plot_colour
      ) +
      ggsignif::geom_signif(
        comparisons = list(c(
          baseline_interval, post_hormone_interval
        )),
        test = "wilcox.test",
        test.args = list(paired = TRUE),
        map_signif_level = c(
          "***" = 0.001,
          "**" = 0.01,
          "*" = 0.05,
          "ns" = 2
        ),
        vjust = -0.3,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = 0.4
      ) +
      ggplot2::stat_summary(
        fun.data = ggplot2::mean_se,
        geom = "pointrange",
        color = theme_options["mean_point_colour", "value"],
        size = as.numeric(theme_options["mean_point_size", "value"]) + 0.2,
        alpha = 0.8
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2))) +
      patchclampplotteR_theme()

    if (large_axis_text == "yes") {
      sEPSC_comparison_plot <- sEPSC_comparison_plot +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 10)),
          axis.title.y = ggplot2::element_text(size = 28, face = "plain")
        )
    }

    if (save_plot_png == "yes") {
      ggplot2::ggsave(
        plot = sEPSC_comparison_plot,
        path = here::here(
          "Figures/Spontaneous-currents/Baseline-vs-post-hormone-comparisons"
        ),
        file = paste0(
          "Baseline-vs-",
          hormone_added,
          "-",
          parameter,
          "-",
          plot_treatment,
          ".png"
        ),
        width = 7,
        height = 5,
        units = "in",
        dpi = 300
      )
    }

    sEPSC_comparison_plot
  }



#' Plot a representative spontaneous current trace
#'
#' `plot_spontaneous_current_trace()` generates a plot of raw current amplitude
#' over time for a specified sweep from an ABF file. It requires a dataframe
#' generated from raw .abf data with [import_ABF_file()]. The function returns a
#' ggplot object with an optional scale bar.
#'
#' @param file A dataframe containing at least these columns: `time`,
#'   `episode`, `current`, `voltage`, `time_sec`. An easy way to obtain this is
#'   by importing a raw .abf file through the [import_ABF_file()] function.
#' @param include_scale_bar A character value that determines if a scale bar
#'   will be added to the plot. Allowed values are "yes" and "no".
#' @param plot_episode A character value describing the sweep (e.g. `epi1`) that
#'   will be used for the plot.
#' @param scale_bar_x_start A numeric value describing the x-axis position of
#'   the scale bar.
#' @param scale_bar_x_length A numeric value describing the horizontal span (in
#'   seconds) of the scale bar. This will automatically be converted and
#'   displayed in milliseconds.
#' @param scale_bar_y_start A numeric value describing the y-axis position of
#'   the scale bar.
#' @param scale_bar_y_length A numeric value describing the vertical span (in
#'   pA) of the scale bar.
#' @param plot_colour A character value naming the colour of the plot.
#' @param plot_x_min A numeric value describing the minimum value on the x-axis
#'   (in seconds).
#' @param plot_x_max A numeric value describing the maximum value on the x-axis
#'   (in seconds).
#' @param plot_y_min A numeric value describing the minimum value on the y-axis
#'   (in pA).
#' @param plot_y_max A numeric value describing the maximum value on the y-axis
#'   (in pA).
#' @param save_plot_pngs A character ("yes" or "no") defining if the plot should
#' be saved as a PNG through `ggplot::ggsave()`.
#'
#' @returns A ggplot object. If save_plot_PNGs is defined as "yes", it will also
#'   generate a .png file in the folder
#'   `Figures/Spontaneous-currents/Representative-Traces` relative to the
#'   project directory.
#'
#' @export
#'
#' @examples
#' plot_spontaneous_current_trace(
#'   file = sample_abf_file,
#'   plot_colour = "#6600cc",
#'   include_scale_bar = "yes",
#'   plot_episode = "epi1",
#'   scale_bar_x_length = 1,
#'   scale_bar_y_length = 10,
#'   plot_x_min = 1,
#'   plot_x_max = 3
#' )
#'
plot_spontaneous_current_trace <-
  function(file,
           plot_colour,
           include_scale_bar = "yes",
           plot_episode = "epi1",
           scale_bar_x_start = 1.25,
           scale_bar_x_length = 0.5,
           scale_bar_y_start = 15,
           scale_bar_y_length = 20,
           plot_x_min = 1,
           plot_x_max = 5,
           plot_y_min = -100,
           plot_y_max = 35,
           save_plot_pngs = "no") {
    if (!include_scale_bar %in% c("yes", "no")) {
      stop("'include_scale_bar' argument must be one of: 'yes' or 'no'")
    }

    if (!save_plot_pngs %in% c("yes", "no")) {
      stop("'save_plot_pngs' argument must be one of: 'yes' or 'no'")
    }

    representative_traces_plot <- file %>%
      dplyr::filter(.data$episode == plot_episode) %>%
      dplyr::filter(dplyr::between(.data$time_sec, plot_x_min, plot_x_max)) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$time_sec, y = .data$current)) +
      ggplot2::coord_cartesian(ylim = c(plot_y_min, plot_y_max)) +
      ggplot2::geom_line(color = plot_colour) +
      ggplot2::theme_void()

    scale_bar_x_length_in_ms <- scale_bar_x_length * 1000

    if (include_scale_bar == "yes") {
      representative_traces_plot <- representative_traces_plot +
        ggplot2::annotate(
          "segment",
          x = scale_bar_x_start,
          xend = scale_bar_x_start + scale_bar_x_length,
          y = scale_bar_y_start,
          yend = scale_bar_y_start,
          lwd = 0.4
        ) +
        ggplot2::annotate(
          "segment",
          x = scale_bar_x_start,
          xend = scale_bar_x_start,
          y = scale_bar_y_start,
          yend = scale_bar_y_start + scale_bar_y_length,
          lwd = 0.4
        ) +
        ggplot2::annotate(
          "text",
          x = scale_bar_x_start - (plot_x_max - plot_x_min) / 100,
          y = scale_bar_y_start + 0.5 * scale_bar_y_length,
          label = paste0(scale_bar_y_length, " pA"),
          hjust = 1,
          vjust = 0.5
        ) +
        ggplot2::annotate(
          "text",
          x = scale_bar_x_start + 0.5 * scale_bar_x_length,
          y = scale_bar_y_start - 5,
          label = paste0(scale_bar_x_length_in_ms, " ms"),
          hjust = 0.5
        )
    }

    if (save_plot_pngs == "yes") {
      ggplot2::ggsave(
        plot = representative_traces_plot,
        path = here::here("Figures/Spontaneous-currents/Representative-Traces"),
        file = paste0(substitute(recording_name), ".png"),
        width = 7,
        height = 5,
        units = "in",
        dpi = 600
      )
    }

    representative_traces_plot
  }


#' Make interactive overview table of all recordings
#'
#' This function pulls information from multiple dataframes to display
#' everything about a cell (cell characteristics, evoked current data, and
#' spontaneous current data) in an interactive table. The table is made with
#' `reactable::reactable()`, so it can be filtered, sorted, and rearranged.
#'
#' The table contains sparklines of the evoked current and spontaneous current
#' amplitudes over time, which allows you to visually compare the overall
#' response of a group of cells.
#'
#' The sparklines are colour-coded by treatment, allowing you to quickly
#' identify trends in response to a hormone/protocol for all cells belonging to
#' a particular treatment.
#'
#' @inheritParams plot_baseline_data
#' @param cell_characteristics_dataframe A dataframe containing the cell
#'   characteristics, generated from [import_cell_characteristics_df()].
#' @param pruned_eEPSC_dataframe A dataframe containing pruned evoked current
#'   data, generated from [make_pruned_EPSC_data()], where `current_type ==
#'   "eEPSC"`.
#' @param pruned_sEPSC_dataframe A dataframe containing pruned spontaneous
#'   current data, generated from [make_pruned_EPSC_data()], where `current_type
#'   == "sEPSC"`.
#' @param include_all_categories A character ("yes" or "no") specifying if the
#'   plot will include data from all categories. If "no", you must specify a
#'   list of categories in `list_of_categories`.
#' @param list_of_categories A list of character values describing the
#'   categories that will be in the plot. Defaults to NULL, since
#'   include_all_categories is "yes" by default.
#' @param save_output_as_RDS A character ("yes" or "no") describing if the
#'   resulting object should be saved as an RDS file in the raw data folder.
#'   Note: This is not the interactive table, but it is the raw dataframe that
#'   is later inserted into `reactable::reactable()`. This is useful if you want
#'   to build your own table using a different package, or you want to generate
#'   a customized reactable table yourself.
#'
#' @returns A reactable HTML widget that can be viewed in RStudio or exported in
#'   RMarkdown HTML documents. If `save_output_as_RDS == "yes"`, the raw
#'   dataframe used to create the reactable is also exported as an .rds file
#'   into `Data/Output-Data-from-R/`.
#' @export
#'
#' @examples
#'
#' make_interactive_summary_table(
#'   cell_characteristics_dataframe = sample_cell_characteristics,
#'   pruned_eEPSC_dataframe = sample_pruned_eEPSC_df,
#'   pruned_sEPSC_dataframe = sample_pruned_sEPSC_df,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   include_all_treatments = "yes",
#'   list_of_treatments = NULL,
#'   include_all_categories = "yes",
#'   list_of_categories = NULL,
#'   save_output_as_RDS = "no"
#' )
#'
make_interactive_summary_table <- function(cell_characteristics_dataframe,
                                           pruned_eEPSC_dataframe,
                                           pruned_sEPSC_dataframe,
                                           treatment_colour_theme,
                                           include_all_treatments = "yes",
                                           list_of_treatments = NULL,
                                           include_all_categories = "yes",
                                           list_of_categories = NULL,
                                           save_output_as_RDS = "no") {
  if (!save_output_as_RDS %in% c("yes", "no")) {
    stop("'save_output_as_RDS' argument must be one of: 'yes' or 'no'")
  }

  if (!include_all_treatments %in% c("yes", "no")) {
    stop("'include_all_treatments' argument must be one of: 'yes' or 'no'")
  }

  if (!include_all_categories %in% c("yes", "no")) {
    stop("'include_all_categories' argument must be one of: 'yes' or 'no'")
  }

  table_data <-
    merge(pruned_eEPSC_dataframe$for_table, pruned_sEPSC_dataframe$for_table, by = "letter") %>%
    merge(cell_characteristics_dataframe, by = "letter") %>%
    merge(treatment_colour_theme, by = "treatment") %>%
    dplyr::select(
      c(
        .data$letter,
        .data$display_names,
        .data$treatment,
        .data$synapses,
        .data$sex,
        .data$P1_transformed,
        .data$spont_amplitude_transformed,
        .data$R_a,
        .data$X,
        .data$Y,
        .data$age,
        .data$animal,
        .data$category,
        .data$cell,
        .data$colours
      )
    ) %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::mutate(
      X = round(.data$X, -1),
      Y = round(.data$Y, -1)
    )

  if (include_all_treatments == "yes") {
    if (!is.null(list_of_treatments)) {
      warning(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is NULL.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    if (!is.character(list_of_treatments)) {
      stop(
        "include_all_treatments = \"",
        include_all_treatments,
        "\", but list_of_treatments is not a character object.",
        "\nDid you forget to add a list of treatments?"
      )
    }

    table_data <- table_data %>%
      dplyr::filter(.data$Treatment %in% list_of_treatments)
  }
  # Category filter

  if (include_all_categories == "yes") {
    if (!is.null(list_of_categories)) {
      warning(
        "include_all_categories = \"yes\", but you included a list of categories to filter. All categories will be used."
      )
    }
  } else {
    if (is.null(list_of_categories)) {
      stop(
        "include_all_categories = \"",
        include_all_categories,
        "\", but list_of_categories is NULL.",
        "\nDid you forget to add a list of categories?"
      )
    }

    if (!is.character(list_of_categories)) {
      stop(
        "include_all_categories = \"",
        include_all_categories,
        "\", but list_of_categories is not a character object.",
        "\nDid you forget to add a list of categories?"
      )
    }

    table_data <- table_data %>%
      dplyr::filter(.data$Category %in% list_of_categories)
  }

  if (save_output_as_RDS == "yes") {
    saveRDS(table_data, file = here::here(paste0(
      "Data/Output-Data-from-R/interactive_summary_table_df.RDS"
    )))
  }

  cell_table <- reactable::reactable(
    data = table_data,
    defaultSorted = c("Category", "Treatment", "Animal"),
    filterable = TRUE,
    showPageSizeOptions = TRUE,
    elementId = "cell-table",
    defaultPageSize = 15,
    defaultColDef = reactable::colDef(vAlign = "center", headerVAlign = "center"),
    columns = list(
      Colours = reactable::colDef(show = FALSE),
      Letter = reactable::colDef(
        name = "Letter",
        sticky = "left",
        style = list(borderRight = "1px solid #eee"),
        headerStyle = list(borderRight = "1px solid #eee")
      ),
      Display_names = reactable::colDef(name = "Treatment"),
      Treatment = reactable::colDef(show = FALSE),
      Sex = reactable::colDef(
        name = "Sex",
        filterMethod = reactable::JS(
          "function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return String(row.values[columnId]).toUpperCase() === filterValue.toUpperCase()
        })
      }"
        )
      ),
      P1_transformed = reactable::colDef(
        name = "eEPSC amplitude (pA)",
        filterable = FALSE,
        cell = reactablefmtr::react_sparkline(
          table_data,
          line_color_ref = "Colours",
          show_area = TRUE,
          area_opacity = 1
        )
      ),
      Spont_amplitude_transformed = reactable::colDef(
        name = "sEPSC amplitude (pA)",
        filterable = FALSE,
        cell = reactablefmtr::react_sparkline(
          table_data,
          line_color_ref = "Colours",
          show_area = TRUE,
          area_opacity = 1
        )
      ),
      R_a = reactable::colDef(
        name = "Ra (M\u03a9)",
        filterable = FALSE,
        cell = reactablefmtr::react_sparkline(
          table_data,
          line_color_ref = "Colours",
          labels = c("first", "last"),
          decimals = 1
        )
      )
    )
  )


  return(cell_table)
}
