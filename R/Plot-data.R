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
#' @return A ggplot object
#' @export
#'
#' @examples
#' make_baseline_comparison_plot(
#'  data = sample_summary_eEPSC_df,
#'  treatment_colour_theme = sample_treatment_names_and_colours,
#'  theme_options = sample_theme_options,
#'  include_all_treatments = "yes",
#'  list_of_treatments = NULL,
#'  current_type = "eEPSC",
#'  parameter = "raw_amplitude",
#'  baseline_interval = "t0to5",
#'  large_axis_text = "no",
#'  plot_width = 8
#')
make_baseline_comparison_plot <- function(data,
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
      y_title = "Baseline eEPSC Amplitude (pA)"
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
    ggplot2::ggplot(ggplot2::aes(
      x = .data$treatment,
      y = .data[[y_var]],
      color = .data$treatment,
      shape = .data$sex
    )) +
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
    ggplot2::scale_color_manual(breaks = treatment_info$display_names,
                                values = treatment_info$colours) +
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::theme(legend.position = "right",
                   legend.background = ggplot2::element_rect(fill = NA),
    ) +
    ggplot2::guides(color = "none", shape = ggplot2::guide_legend(reverse = TRUE))

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
#' spontaneous) current frequency over time in minutes. The plot title will be
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
#'   like to plot (e.g. "Control").
#' @param plot_category A numeric value specifying the category, which can be
#'   used to differentiate different protocol types.
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
#' @return A list of ggplot objects, where each list element is a scatterplot of
#'   one recording.
#' @export
#'
#' @examples
#'
#' make_raw_plots(
#'  data = sample_raw_eEPSC_df,
#'  plot_treatment = "Control",
#'  plot_category = 2,
#'  current_type = "eEPSC",
#'  parameter = "P1",
#'  pruned = "no",
#'  hormone_added = "Insulin",
#'  hormone_or_HFS_start_time = 5,
#'  theme_options = sample_theme_options,
#'  treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
#'
make_raw_plots <-
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
      stop("\"hormone_added\" must be a character. Use \"HFS\" for high-frequency stimulation or \"Insulin\", \"CCK\", etc. for any hormones")
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

    if (!is.null(hormone_end_time) & !is.numeric(hormone_end_time)) {
      stop(
        "\"hormone_end_time\" must be numeric
        (e.g. 25 for a hormone ending at 25 minutes)."
      )
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
          stop("pruned = \"yes\"), but parameter = \"P1\". ",
               "\nDid you mean \"mean_P1\"?. ")
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
        list_of_plots[[i]] <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$time, y = .data[[parameter]])) + ggplot2::geom_point(shape = as.numeric(theme_options["male_shape", "value"]))

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
        ggplot2::labs(x = "Time (min)", y = y_title)

      # if (current_type == "sEPSC" &
      #     pruned == "yes" &
      #     parameter == "amplitude") {
      #   list_of_plots[[i]] <- list_of_plots[[i]] +
      #     ggplot2::geom_pointrange(
      #       shape = if (unique(plot_df$sex) == "Male") {
      #         as.numeric(theme_options["male_shape", "value"])
      #       } else {
      #         as.numeric(theme_options["female_shape", "value"])
      #       },
      #       colour = plot_colour,
      #       size = 1,
      #       alpha = 1
      #     )
      # } else {
      #   list_of_plots[[i]] <- list_of_plots[[i]] +
      #     ggplot2::geom_point(
      #       shape = if (unique(plot_df$sex) == "Male") {
      #         as.numeric(theme_options["male_shape", "value"])
      #       } else {
      #         as.numeric(theme_options["female_shape", "value"])
      #       },
      #       colour = plot_colour,
      #       size = if (current_type == "sEPSC" & pruned == "no") {
      #         1
      #       } else {
      #         3.5
      #       },
      #       alpha = if (pruned == "yes") {
      #         1
      #       } else {
      #         0.7
      #       }
      #     )
      # }

      # Get limits of x- and y-axes
      ymax <- ggplot2::layer_scales(list_of_plots[[i]])$y$get_limits()[2]
      xmax <- ggplot2::layer_scales(list_of_plots[[i]])$x$get_limits()[2]
      ymax2 <- ggplot2::layer_scales(list_of_plots[[i]])$y$get_limits()[2]

      # If hormone_added = Insulin, CCK, i.e. anything other than "HFS" (high frequency stimulation),
      # add an annotated line over the application period:

      if (hormone_added != "HFS") {
        list_of_plots[[i]] <-
          list_of_plots[[i]] +
          ggplot2::annotate(
            geom = "segment",
            x = hormone_or_HFS_start_time,
            xend = if (is.null(hormone_end_time)) {xmax} else {hormone_end_time},
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
            hjust = 0,
            family = theme_options["plot_font_family", "value"]
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
            hjust = 0.5,
            family = theme_options["plot_font_family", "value"]
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



# rawplots <- make_raw_plots(
#   data = sample_raw_eEPSC_df,
#   plot_treatment = "Control",
#   plot_category = 2,
#   current_type = "eEPSC",
#   parameter = "P1",
#   pruned = "no",
#   hormone_added = "Insulin",
#   hormone_or_HFS_start_time = 5,
#   hormone_end_time = 25,
#   theme_options = sample_theme_options,
#   treatment_colour_theme = sample_treatment_names_and_colours
# )

#
# data <- dplyr::filter(sample_raw_eEPSC_df, letter == "AO")
#
# ggplot2::ggplot(data = data, ggplot2::aes(x = .data$time, y = .data$P1)) +
#   ggplot2::geom_point() +
#   ggplot2::annotate(
#     geom = "segment",
#     x = 4,
#     xend = 20,
#     y = 60 + 0.1 * 60,
#     yend = 60 + 0.1 * 60,
#     colour = sample_theme_options["line_col", "value"],
#     linewidth = 0.6
#   )

