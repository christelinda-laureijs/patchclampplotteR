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
#' This function creates a scatterplot of `y_variables` such as raw amplitude
#' grouped according to treatment. The data are limited to values from the
#' baseline period, allowing for a quick comparison of baseline parameters
#' across treatments. For evoked currents, only raw amplitude is available for
#' comparison. For spontaneous currents, raw amplitude and raw frequency during
#' the baseline period can be plotted.
#'
#' @inheritParams plot_summary_current_data
#' @param data A dataframe containing the summary data generated from
#'   [make_summary_EPSC_data()]. If `current_type` is "eEPSC", this must be the `$summary_data` element of the list produced by [make_summary_EPSC_data()].
#' @param plot_category A numeric value specifying the category, which can be
#'   used to differentiate different protocol types. In the sample dataset for
#'   this package, `plot_category == 2` represents experiments where insulin was
#'   applied continuously after a 5-minute baseline period.
#' @param treatment_colour_theme A dataframe containing treatment names and
#'   their associated colours as hex values. See
#'   [sample_treatment_names_and_colours] for an example of what this dataframe
#'   should look like.
#' @param theme_options A dataframe containing theme options, defaults to `sample_theme_options`. See
#'   [sample_theme_options] for an example of what this dataframe should
#'   look like and how you can customize these values.
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
#' @param y_variable A character describing the parameter used on the y-axis. If
#'   `current_type == "eEPSC"`, the allowed y_variable is "raw_amplitude". If
#'   `current_type == "sEPSC"`, the allowed y_variables are "raw_amplitude" or
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
#' @param ggplot_theme The name of a ggplot theme or your custom theme. This will be added as a layer to a ggplot object. The default is `patchclampplotteR_theme()`, but other valid entries include `theme_bw()`, `theme_classic()` or the name of a custom ggplot theme stored as an object.
#'
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in `Figures/Evoked-currents/Output-summary-plots` or
#'   `Figures/Spontaneous-currents/Output-summary-plots`. The .png filename will
#'   contain the `y_variable`.
#' @export
#'
#' @examples
#' plot_baseline_data(
#'   data = sample_summary_eEPSC_df$summary_data,
#'   current_type = "eEPSC",
#'   plot_category = 2,
#'   y_variable = "raw_amplitude",
#'   include_all_treatments = "yes",
#'   list_of_treatments = NULL,
#'   baseline_interval = "t0to5",
#'   included_sexes = "both",
#'   large_axis_text = "no",
#'   plot_width = 8,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
plot_baseline_data <- function(data,
                               current_type = "eEPSC",
                               plot_category,
                               y_variable = "raw_amplitude",
                               include_all_treatments = "yes",
                               list_of_treatments = NULL,
                               baseline_interval = "t0to5",
                               filename_suffix = "",
                               large_axis_text = "no",
                               included_sexes = "both",
                               male_label = "Male",
                               female_label = "Female",
                               plot_width = 8,
                               treatment_colour_theme,
                               theme_options,
                               save_plot_png = "no",
                               ggplot_theme = patchclampplotteR_theme()) {
  if (is.null(current_type) ||
    length(current_type) != 1L ||
    !current_type %in% c("eEPSC", "sEPSC")) {
    cli::cli_abort(c("x" = "'current_type' argument must be either 'eEPSC' or 'sEPSC'"))
  }

  if (is.null(baseline_interval) ||
    !is.character(baseline_interval)) {
    cli::cli_abort(c("x" = "'baseline_interval' must be a character (e.g. \"t0to5\", \"t0to3\")"))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (!large_axis_text %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`large_axis_text` argument must be either \"yes\" or \"no\""))
  }

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      cli::cli_alert_info(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is NULL."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    if (!is.character(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }

  if (current_type == "sEPSC") {
    filepath <- "Figures/Spontaneous-currents/Output-summary-plots"

    allowed_y_variables_list <- "\"raw_amplitude\", or \"raw_frequency\""

    if (!y_variable %in% c("raw_amplitude", "raw_frequency")) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for current_type \"",
          current_type,
          "\" \nbecause transformed data are all 100 during the baseline."
        )
      ))
    }

    if (y_variable == "raw_amplitude") {
      y_var <- "mean_raw_amplitude"
      y_title <- "Baseline sEPSC Amplitude (pA)"
    }

    if (y_variable == "raw_frequency") {
      y_var <- "mean_raw_frequency"
      y_title <- "Baseline sEPSC Frequency (Hz)"
    }
  }

  if (current_type == "eEPSC") {
    allowed_y_variables_list <- c("\"raw_amplitude\"")

    if (!y_variable %in% c("raw_amplitude")) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for current_type \"",
          current_type,
          "\" \nbecause transformed data are all 100 during the baseline."
        )
      ))
    }

    filepath <- "Figures/Evoked-currents/Output-summary-plots"

    if (y_variable == "raw_amplitude") {
      y_var <- "mean_P1_raw"
      y_title <- "Baseline eEPSC Amplitude (pA)"
    }
  }

  if (included_sexes == "male") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"

    plot_shape <- as.numeric(theme_options["male_shape", "value"])
  }

  if (included_sexes == "female") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"

    plot_shape <- as.numeric(theme_options["female_shape", "value"])
  }

  if (included_sexes == "both") {
    sex_annotation <- ""
  }

  baseline_comparison_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$interval == baseline_interval) %>%
    dplyr::mutate(
      treatment = stringr::str_replace_all(
        .data$treatment,
        stats::setNames(treatment_info$display_names, treatment_info$treatment)
      ),
      treatment = factor(.data$treatment, levels = treatment_info$display_names)
    )


  if (included_sexes == "both") {
  baseline_comparison_plot <- baseline_comparison_plot %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$treatment,
        y = .data[[y_var]],
        color = .data$treatment,
        shape = .data$sex
      )
    ) +
    ggforce::geom_sina(
      bw = 12,
      alpha = 0.8,
      maxwidth = 0.5,
      size = 2
    ) +
    ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["female_shape", "value"]), as.numeric(theme_options["male_shape", "value"])))
  }


  if (included_sexes != "both") {
    baseline_comparison_plot <- baseline_comparison_plot %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$treatment,
          y = .data[[y_var]],
          color = .data$treatment
        )
      ) +
      ggforce::geom_sina(
        bw = 12,
        alpha = 0.8,
        maxwidth = 0.5,
        size = 2,
        shape = plot_shape
      )
  }

  baseline_comparison_plot <- baseline_comparison_plot +
    ggplot2::labs(x = NULL, y = y_title, shape = "Sex") +
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
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = NA)
    ) +
    ggplot2::guides(
      color = "none",
      shape = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot_theme

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
        y_variable,
        "-comparison-category-",
        plot_category,
        filename_suffix,
        sex_annotation,
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
#' @inheritParams plot_baseline_data
#' @param data A dataframe containing the raw evoked current data generated from
#'   [make_normalized_EPSC_data()]. If `pruned = "yes"` you must use the `$individual_cells` component of a pruned dataset.
#' @param plot_treatment A character value specifying the treatment you would
#'   like to plot (e.g. "Control"). `plot_treatment` represents antagonists that
#'   were present on the brain slice, or the animals were fasted, etc.
#' @param plot_category A numeric value specifying the category, which can be
#'   used to differentiate different protocol types. In the sample dataset for
#'   this package, `plot_category == 2` represents experiments where insulin was
#'   applied continuously after a 5-minute baseline period.
#' @param current_type A character describing the current type. Allowed values
#'   are "eEPSC" or "sEPSC".
#' @param y_variable A character value specifying the variable to be plotted on
#'   the y-axis. For evoked currents (`current_type = "eEPSC"`), the available
#'   y_variables are "P1", "P1_transformed", "mean_P1" and "PPR". *Note*: If you
#'   select "mean_P1", you must set the `pruned` argument to "yes". For
#'   spontaneous currents (`current_type = "sEPSC"`), the available y_variables
#'   are "amplitude" or "frequency".
#' @param pruned A character value ("yes" or "no") specifying if the data are
#'   pruned. The plot will then present the data as means with error bars.
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
#' @returns A list of ggplot objects, where each list element is a scatterplot
#'   of one recording. If `save_plot_png == "yes"`, it will also generate a .png
#'   file from each ggplot element in the list. The figures will be exported to
#'   `Figures/Evoked-currents/Output-individual-plots` or
#'   `Figures/Spontaneous-currents/Output-individual-plots`, depending on the
#'   `current_type`. The .png filename will contain the `letter`. If the data
#'   are pruned, the filename will also include `_pruned` in the filename. If
#'   the data are normalized (`y_variable == "P1_transformed`), the title will
#'   include `_normalized` in the filename. Example filenames include "A.png",
#'   "A_normalized", and "A_pruned.png".
#' @export
#'
#' @examples
#'
#' # Plot raw data
#' plot_raw_current_data(
#'   data = sample_raw_eEPSC_df,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   current_type = "eEPSC",
#'   y_variable = "P1",
#'   pruned = "no",
#'   hormone_added = "Insulin",
#'   hormone_or_HFS_start_time = 5,
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   save_plot_png = "no"
#' )
#'
#'
#' # Plot pruned data
#'
#' # Note that this requires the third element of the list generated with `make_pruned_EPSC_data()`.
#'
#' plot_raw_current_data(
#'   data = sample_pruned_eEPSC_df$individual_cells,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   current_type = "eEPSC",
#'   y_variable = "mean_P1",
#'   pruned = "yes",
#'   hormone_added = "Insulin",
#'   hormone_or_HFS_start_time = 5,
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
plot_raw_current_data <-
  function(data,
           plot_treatment = "Control",
           plot_category = 2,
           current_type = "eEPSC",
           y_variable = "P1",
           pruned = "no",
           hormone_added = "Insulin",
           hormone_or_HFS_start_time = 5,
           hormone_end_time = NULL,
           theme_options,
           treatment_colour_theme,
           save_plot_png = "no",
           ggplot_theme = patchclampplotteR_theme()) {
    list_of_plots <- list()

    if (is.null(current_type) ||
      length(current_type) != 1L ||
      !current_type %in% c("eEPSC", "sEPSC")) {
      cli::cli_abort(c("x" = "'current_type' argument must be either 'eEPSC' or 'sEPSC'"))
    }

    if (is.null(hormone_added) ||
      length(hormone_added) != 1L ||
      !is.character(hormone_added)) {
      cli::cli_abort(c(
        "x" = "`hormone_added` must be a character.",
        "i" = "For example, you could use \"HFS\" for high-frequency stimulation",
        "i" = "If you are adding a hormone, examples include \"Insulin\", \"CCK\", etc.",
        "i" = "`hormone_added` will be the label for the annotation line on the plot."
      ))
    }

    if (!save_plot_png %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
    }


    if (is.null(hormone_or_HFS_start_time) ||
      !is.numeric(hormone_or_HFS_start_time)) {
      cli::cli_abort(c("x" = "`hormone_or_HFS_start_time` must be numeric
      (e.g. 5 for HFS or a hormone applied at five minutes into the recording)."))
    }

    if (!is.null(hormone_end_time) &
      !is.numeric(hormone_end_time)) {
      cli::cli_abort(c("x" = "`hormone_end_time` must be numeric
        (e.g. 25 for a hormone ending at 25 minutes)."))
    }

    df <- data %>%
      dplyr::filter(.data$category == plot_category) %>%
      dplyr::filter(.data$treatment == plot_treatment)


    letters <- as.character(unique(unlist(df$letter)))

    plot_colour <- treatment_colour_theme %>%
      dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
      dplyr::pull(.data$colours)

    treatment_label <- treatment_colour_theme %>%
      dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::pull(.data$treatment)

    if (current_type == "eEPSC") {
      # The plots should go to specific folders depending on current type
      filepath <- "Figures/Evoked-currents/Output-individual-plots"

      allowed_y_variables_list <- "\"P1\", \"P1_transformed\", \"mean_P1\", or \"PPR\""

      if (!y_variable %in% c("P1", "P1_transformed", "mean_P1", "PPR")) {
        cli::cli_abort(c(
          "x" = paste0(
            "`y_variable` must be ",
            allowed_y_variables_list,
            " for current_type \"",
            current_type,
            "\"."
          ),
          "i" = "Check that you have the correct combination of `y_variable`, `current_type` and `data.`"
        ))
      }

      if (y_variable == "P1") {
        y_title <- "eEPSC Amplitude (pA)"

        if (pruned == "yes") {
          cli::cli_abort(c(
            "x" = "`pruned` = \"yes\", but `y_variable` = \"P1\". ",
            "i" = "Did you want to use pruned data? If so, please set `y_variable` to \"mean_P1\" instead of \"P1\"."
          ))
        }
      }

      if (y_variable == "P1_transformed") {
        y_title <- "eEPSC Amplitude (% Baseline)"

        if (pruned == "yes") {
          cli::cli_abort(c(
            "x" = "`pruned` = \"yes\", but `y_variable` = \"P1_transformed\". ",
            "i" = "Did you want to use pruned data? If so, please set `y_variable` to \"mean_P1\" instead of \"P1\"."
          ))
        }
      }

      if (y_variable == "mean_P1") {
        y_title <- "eEPSC Amplitude (pA)"


        if (pruned == "no") {
          cli::cli_abort(c(
            "x" = "`y_variable` = \"mean_P1\", but pruned = \"no\".",
            "i" = "Are you trying to create a pruned plot? If so, change `pruned` to \"yes\" and ensure that you have specified the correct dataframe in the data argument."
          ))
        }
      }

      if (y_variable == "PPR") {
        y_title <- "Paired-pulse Ratio"
        annotation <- "_PPR"
      }
    }


    if (current_type == "sEPSC") {
      filepath <- "Figures/Spontaneous-currents/Output-individual-plots"

      allowed_y_variables_list <- "\"amplitude\" or \"frequency\""

      if (!y_variable %in% c("amplitude", "frequency")) {
        cli::cli_abort(c(
          "x" = paste0(
            "`y_variable` must be ",
            allowed_y_variables_list,
            " for `current_type` \"",
            current_type,
            "\"."
          ),
          "i" = "Check to make sure that you have a logical combination of `y_variable`, `current_type` or `data.`"
        ))
      }


      if (y_variable == "amplitude") {
        y_title <- "sEPSC Amplitude (pA)"
        annotation <- "_amplitude"

        if (pruned == "yes") {
          annotation <- "_amplitude_pruned"
        }
      }

      if (y_variable == "frequency") {
        y_title <- "sEPSC Frequency (Hz)"
        annotation <- "_frequency"
      }
    }

    if (pruned == "yes" & y_variable == "mean_P1") {
      annotation <- "_pruned"
    }

    if (pruned == "no" & y_variable == "P1") {
      annotation <- ""
    }

    if (y_variable == "P1_transformed") {
      annotation <- "_normalized"
    }

    # Pruned sEPSC amplitude plots use mean +/- SE, unlike the other plots


    for (i in letters) {
      plot_df <- df %>% dplyr::filter(.data$letter == i)
      if (current_type == "sEPSC" &
        pruned == "yes" &
        y_variable == "amplitude") {
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
        list_of_plots[[i]] <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$time, y = .data[[y_variable]]))
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
        ggplot_theme

      if (current_type == "sEPSC" &
        pruned == "yes" &
        y_variable == "amplitude") {
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
#' @inheritParams plot_baseline_data
#' @param data A dataframe containing pruned summary data for all cells. This is
#'   the third element of the list generated from [make_pruned_EPSC_data()].
#' @param include_representative_trace A character ("yes" or "no") describing if
#'   a representative trace should be included as an overlay to the plot. This
#'   pulls from a png file stored in `Figures/Representative-Traces/`". Please
#'   specify the file-name in `representative_trace_filename`.
#' @param representative_trace_filename A character value describing the
#'   filename of the representative trace. This should be the name of a .png
#'   file. Use relative paths to specify files. For example, a figure in the Figures/Representative-Traces subfolder would be entered as `representative_trace_filename = "Figures/Representative-Traces/Category-2-Control-Trace.png"`.
#' @param annotation_x_min A numeric value describing the minimum value on the x-axis for the representative trace. Defaults to 1, which will place it at the lower left corner of the plot (when combined with the default value for `annotation_y_min`).
#' @param annotation_x_max A numeric value describing the maximum value on the x-axis for the representative trace. Change this if your representative trace image looks squished or stretched.
#' @param annotation_y_min A numeric value describing the minimum value on the y-axis for the representative trace. Defaults to 0, which will place it at the lower left corner of the plot (when combined with the default value for `annotation_x_min`).
#' @param annotation_y_max A numeric value describing the maximum value on the y-axis for the representative trace. Change this if your representative trace image looks squished or stretched.
#' @param included_sexes A character value ("both", "male" or "female"). Useful if you want to have a plot with data from one sex only. Defaults to "both". If you choose a single sex, the resulting plot will have "-males-only" or "-females-only" in the file name. WARNING!! If you choose "male" or "female", you MUST ensure that the `t_test_df` contains data that has been filtered to only include one sex. Otherwise, the significance stars will represent both sexes and it will be inaccurate.
#' @param male_label A character value used to describe how males are encoded in the `sex` column of the dataframe used in `data`. Examples include "Males", "Male", "male", "males", "M", etc. Defaults to "Male".
#' @param female_label A character value used to describe how females are encoded in the `sex` column of the dataframe used in `data`. Examples include "Females", "Female", "female", "females", "F", etc. Defaults to "Female".
#' @param y_axis_limit A numeric value describing the maximum value on the y-axis.
#' @param geom_signif_family A character value describing the font family used for the p-value annotations used by `ggsignif::geom_signif()`.
#' @param geom_signif_size A numeric value describing the size of the text annotations (significance stars or p-values) on the plot. Defaults to `8`.
#' @param signif_stars A character ("yes" or "no") describing if significance
#'   stars should be included as an overlay in the plot. If "yes", you must
#'   specify a dataframe containing the results of a t-test, which is generated
#'   using [perform_t_tests_for_summary_plot()]. NOTE! If `included_sexes` is "male" or "female", you MUST use a t-test that was performed on data filtered to ONE sex.
#' @param significance_display_method A character value ("stars" or "p-value") describing how significance values should be displayed. These annotations will not appear if `signif_stars` is "no".)
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
#'   "Summary-plot-`plot_treatment`-category-`plot_category`-`file_name_ending`-`text_size`-`sexes`.png".
#'   The `text_size` will only be added on if you are using `large_axis_text`
#'   ("LARGE" will be included in the filename). The `file_name_ending` will be
#'   automatically added on for spontaneous current data to specify what
#'   y_variable is plotted (e.g. "raw_amplitude"). The `sexes` will only be added if you choose to plot a single sex. For example, if you set `included_sexes = "Male"`, the `.png` filename will have "-males-only" included in the filename.
#'
#'   Example auto-generated filenames include:
#'   * "Summary-plot-Control-category-2.png"
#'   * "Summary-plot-Control-category-2-males-only.png"
#'   * "Summary-plot-Control-category-2_raw_amplitude.png" for spontaneous
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
#'   data = sample_pruned_eEPSC_df$all_cells,
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   current_type = "eEPSC",
#'   y_variable = "amplitude",
#'   hormone_added = "Insulin",
#'   hormone_or_HFS_start_time = 5,
#'   included_sexes = "both",
#'   include_representative_trace = "yes",
#'   representative_trace_filename = import_ext_data("Control-trace.png"),
#'   y_axis_limit = 175,
#'   signif_stars = "yes",
#'   t_test_df = sample_eEPSC_t_test_df,
#'   large_axis_text = "no",
#'   shade_intervals = "no",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
plot_summary_current_data <- function(data,
                                      plot_category = 2,
                                      plot_treatment = "Control",
                                      current_type = "eEPSC",
                                      y_variable = "amplitude",
                                      hormone_added = "Insulin",
                                      hormone_or_HFS_start_time = 5,
                                      included_sexes = "both",
                                      male_label = "Male",
                                      female_label = "Female",
                                      include_representative_trace = "no",
                                      representative_trace_filename = NULL,
                                      annotation_x_min = 1,
                                      annotation_x_max = 8,
                                      annotation_y_min = 0,
                                      annotation_y_max = 40,
                                      y_axis_limit,
                                      signif_stars = "no",
                                      significance_display_method = "stars",
                                      geom_signif_size = 8,
                                      geom_signif_family = "",
                                      t_test_df,
                                      large_axis_text = "no",
                                      shade_intervals = "no",
                                      theme_options,
                                      treatment_colour_theme,
                                      save_plot_png = "no",
                                      ggplot_theme = patchclampplotteR_theme()) {
  if (is.null(current_type) ||
    length(current_type) != 1L ||
    !current_type %in% c("eEPSC", "sEPSC")) {
    cli::cli_abort(c("x" = "'current_type' argument must be either 'eEPSC' or 'sEPSC'"))
  }

  if (is.null(hormone_added) ||
    length(hormone_added) != 1L ||
    !is.character(hormone_added)) {
    cli::cli_abort(c(
      "x" = "`hormone_added` must be a character.",
      "i" = "For example, you could use \"HFS\" for high-frequency stimulation",
      "i" = "If you are adding a hormone, examples include \"Insulin\", \"CCK\", etc.",
      "i" = "`hormone_added` will be the label for the annotation line on the plot."
    ))
  }

  if (!significance_display_method %in% c("stars", "p-values")) {
    cli::cli_abort(c("x" = "'significance_display_method' argument must be one of: \"stars\" or \"p-values\"."))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }

  if (!signif_stars %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`signif_stars` argument must be either \"yes\" or \"no\""))
  }

  if (!include_representative_trace %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`include_representative_trace` argument must be either \"yes\" or \"no\""))
  }

  if (include_representative_trace == "yes") {
    if (is.null(representative_trace_filename) ||
      !is.character(representative_trace_filename)) {
      cli::cli_abort(
        c(
          "x" = "`include_representative_trace` is \"yes\" but `representative_trace_filename` is not
           specified or it is not a character value.",
          "i" = "Please define the filename
           as a character in
        `representative_trace_filename`"
        )
      )
    }
  }

  if (is.null(y_axis_limit) ||
    !is.numeric(y_axis_limit)) {
    cli::cli_abort(c("x" = "\"y_axis_limit\" must be numeric, e.g. 175."))
  }


  if (signif_stars == "yes" & is.null(t_test_df)) {
    cli::cli_abort(
      c(
        "x" = "`signif_stars` == \"yes\" but `t_test_df` is NULL.",
        "i" = "This is probably because you did not specify a dataframe
      containing t-test summary data.",
        "i" = "Please generate a t-test dataframe using `patchclampplotteR::perform_t_tests_for_summary_plot()` and insert the resulting dataframe into the `t_test_df` argument."
      )
    )
  }


  if (is.null(hormone_or_HFS_start_time) ||
    !is.numeric(hormone_or_HFS_start_time)) {
    cli::cli_abort(c("x" = "`hormone_or_HFS_start_time` must be numeric
      (e.g. 5 for HFS or a hormone applied at five minutes into the recording)."))
  }

  df <-
    data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment)

  if (included_sexes == "male") {
    df <- df %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"
  }

  if (included_sexes == "female") {
    df <- df %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    sex_annotation <- ""
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  plot_colour_pale <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$very_pale_colours)

  if (current_type == "eEPSC") {
    allowed_y_variables_list <- "\"amplitude\""

    if (!y_variable %in% c("amplitude")) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for `current_type` \"",
          current_type,
          "\"."
        ),
        "i" = "Check to make sure that you have a logical combination of `y_variable`, `current_type` or `data.`"
      ))
    }

    if (y_variable == "amplitude") {
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

    allowed_y_variables_list <- "\"amplitude\", \"raw_amplitude\",
    \"raw_frequency\", or \"frequency\""

    if (!y_variable %in% c(
      "amplitude",
      "raw_amplitude",
      "frequency",
      "raw_frequency"
    )) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for `current_type` \"",
          current_type,
          "\"."
        ),
        "i" = "Check to make sure that you have a logical combination of `y_variable`, `current_type` or `data.`"
      ))
    }


    if (y_variable == "amplitude") {
      y_var <- "mean_all_amplitude"
      se_var <- "se_amplitude"
      file_name_ending <- paste0("_", y_variable)

      if (large_axis_text == "yes") {
        y_title <- "sEPSC Amplitude\n(% Baseline)"
      } else {
        y_title <- "sEPSC Amplitude (% Baseline)"
      }
    }

    if (y_variable == "raw_amplitude") {
      y_var <- "mean_all_raw_amplitude"
      se_var <- "se_raw_amplitude"
      file_name_ending <- paste0("_", y_variable)

      y_title <- "sEPSC Amplitude (pA)"
    }

    if (y_variable == "frequency") {
      y_var <- "mean_all_frequency"
      se_var <- "se_frequency"
      file_name_ending <- paste0("_", y_variable)

      if (large_axis_text == "yes") {
        y_title <- "sEPSC Frequency\n(% Baseline)"
      } else {
        y_title <- "sEPSC Frequency (% Baseline)"
      }
    }

    if (y_variable == "raw_frequency") {
      y_var <- "mean_all_raw_frequency"
      se_var <- "se_frequency"
      file_name_ending <- paste0("_", y_variable)

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
          ymax = y_axis_limit
        ),
        fill = theme_options["rectangle_shading_colour", "value"]
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = 15,
          xmax = 20,
          ymin = -5,
          ymax = y_axis_limit
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
    ggplot2::coord_cartesian(ylim = c(0, y_axis_limit)) +
    ggplot2::labs(
      x = "Time (min)",
      y = y_title,
      shape = "Sex",
      color = "Sex"
    ) +
    ggplot_theme

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
  ymax <- y_axis_limit - 25
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
    if (significance_display_method == "stars") {
      treatment_plot <- treatment_plot +
        ggplot2::geom_text(
          data = t_test_df %>% dplyr::filter(.data$treatment == plot_treatment),
          ggplot2::aes(
            x = .data$asterisk_time,
            y = y_axis_limit - 50,
            label = .data$significance_stars
          ),
          inherit.aes = FALSE,
          size = geom_signif_size,
          family = geom_signif_family
        )
    }

    if (significance_display_method == "p-values") {
      treatment_plot <- treatment_plot +
        ggplot2::geom_text(
          data = t_test_df %>% dplyr::filter(.data$treatment == plot_treatment),
          ggplot2::aes(
            x = .data$asterisk_time,
            y = y_axis_limit - 50,
            label = .data$p_string
          ),
          inherit.aes = FALSE,
          size = geom_signif_size,
          family = geom_signif_family
        )
    }
  }


  if (include_representative_trace == "yes") {
    if (file.exists(here::here(representative_trace_filename))) {
      representative_trace <- png::readPNG(here::here(representative_trace_filename)) %>% grid::rasterGrob()

      treatment_plot <- treatment_plot +
        ggplot2::annotation_custom(
          representative_trace,
          xmin = annotation_x_min,
          xmax = annotation_x_max,
          ymin = annotation_y_min,
          ymax = annotation_y_max
        )
    } else {
      cli::cli_alert_info(paste0(
        "The file ",
        representative_trace_filename,
        " cannot be found. Plotting without a representative trace."
      ))
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
        sex_annotation,
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


#' Plot percent change comparisons
#'
#' This function allows you to visually compare how current amplitudes changed X minutes after a treatment.
#'
#' More documentation will be updated soon.
#' @inheritParams plot_baseline_data
#'
#' @param data A dataframe generated from `make_summary_EPSC_data()`. If `current_type = "eEPSC"`, this must be the percent change dataframe generated from `make_summary_EPSC_data()`. Use `$percent_change` to access this dataframe. If `current_type = "sEPSC"`, this can either be `$percent_change_amplitude` or `$percent_change_frequency`.
#'
#' @param y_variable A character value (must be "amplitude" for `current_type = "eEPSC"`. For `current_type = "sEPSC"`, this must be "amplitude" or "frequency", corresponding to `$percent_change_amplitude` or `$percent_change_frequency`, respectively).
#'
#' @returns
#'
#' A ggplot object
#'
#' @export
#'
#' @examples
#'
#' plot_percent_change_comparisons(
#'   data = sample_summary_eEPSC_df$percent_change_data,
#'   plot_category = 2,
#'   current_type = "eEPSC",
#'   y_variable = "amplitude",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
#'
#' # Spontaneous Current Frequency
#'
#' # (note: this plot only has a few datapoints
#' # because sample_sEPSC_df is intentionally
#' # small to reduce file size.)
#'
#' plot_percent_change_comparisons(
#'   data = sample_summary_sEPSC_df$percent_change_frequency,
#'   plot_category = 2,
#'   current_type = "sEPSC",
#'   y_variable = "frequency",
#'   included_sexes = "both",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
#'
plot_percent_change_comparisons <- function(data,
                                            current_type = "eEPSC",
                                            y_variable,
                                            plot_category,
                                            include_all_treatments = "yes",
                                            list_of_treatments = NULL,
                                            filename_suffix = "",
                                            large_axis_text = "no",
                                            included_sexes = "both",
                                            male_label = "Male",
                                            female_label = "Female",
                                            plot_width = 8,
                                            treatment_colour_theme,
                                            theme_options,
                                            save_plot_png = "no",
                                            ggplot_theme = patchclampplotteR_theme()) {
  if (is.null(current_type) ||
    length(current_type) != 1L ||
    !current_type %in% c("eEPSC", "sEPSC")) {
    cli::cli_abort(c("x" = "'current_type' argument must be either 'eEPSC' or 'sEPSC'"))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (!large_axis_text %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`large_axis_text` argument must be either \"yes\" or \"no\""))
  }

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      cli::cli_alert_info(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is NULL."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    if (!is.character(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }

  if (current_type == "eEPSC") {
    filepath <- "Figures/Evoked-currents/Output-summary-plots"

    allowed_y_variables_list <- "\"amplitude\""

    if (y_variable != "amplitude") {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for current_type \"",
          current_type,
          "."
        )
      ))
    }

    y_title <- "Change in eEPSC Amplitude\n(% Baseline)"
  }

  if (current_type == "sEPSC") {
    filepath <- "Figures/Spontaneous-currents/Output-summary-plots"

    allowed_y_variables_list <- "\"amplitude\", or \"frequency\""

    if (!y_variable %in% c("amplitude", "frequency")) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list,
          " for current_type \"",
          current_type,
          "."
        )
      ))
    }

    if (y_variable == "amplitude") {
      y_title <- "Change in sEPSC Amplitude\n(% Baseline)"
    }

    if (y_variable == "frequency") {
      y_title <- "Change in sEPSC Frequency \n(% Baseline)"
    }
  }

  if (included_sexes == "male") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"

    plot_shape <- as.numeric(theme_options["male_shape", "value"])
  }

  if (included_sexes == "female") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"

    plot_shape <- as.numeric(theme_options["female_shape", "value"])
  }

  if (included_sexes == "both") {
    sex_annotation <- ""

    plot_shape <- as.numeric(theme_options["both_sexes_shape", "value"])
  }

  percent_change_comparison_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::mutate(
      treatment = stringr::str_replace_all(
        .data$treatment,
        stats::setNames(treatment_info$display_names, treatment_info$treatment)
      ),
      treatment = factor(.data$treatment, levels = treatment_info$display_names)
    )


  if (included_sexes == "both") {
    percent_change_comparison_plot <- percent_change_comparison_plot %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$treatment,
      y = .data$percent_change,
      color = .data$treatment,
      shape = .data$sex
    )) +
      ggforce::geom_sina(
        bw = 12,
        alpha = 0.8,
        maxwidth = 0.5,
        size = 2
      ) +
      ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["female_shape", "value"]), as.numeric(theme_options["male_shape", "value"])))

  }

  if (included_sexes != "both") {
    percent_change_comparison_plot <- percent_change_comparison_plot %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$treatment,
        y = .data$percent_change,
        color = .data$treatment
      )) +
      ggforce::geom_sina(
        bw = 12,
        alpha = 0.8,
        maxwidth = 0.5,
        size = 2,
        shape = plot_shape
      )
  }

  percent_change_comparison_plot <- percent_change_comparison_plot +
    ggplot2::scale_color_manual(
      breaks = treatment_info$display_names,
      values = treatment_info$colours
    ) +
    ggplot2::labs(x = NULL, y = y_title, shape = "Sex") +
    ggplot2::stat_summary(
      fun.data = ggplot2::mean_se,
      geom = "pointrange",
      color = theme_options["mean_point_colour", "value"],
      size = as.numeric(theme_options["mean_point_size", "value"]) + 0.02,
      alpha = 0.8
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = NA)
    ) +
    ggplot2::guides(
      color = "none",
      shape = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed") +
    ggplot_theme

  if (large_axis_text == "yes") {
    percent_change_comparison_plot <- percent_change_comparison_plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24, margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(size = 24, face = "plain")
      )
  }

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      percent_change_comparison_plot,
      path = here::here(filepath),
      file = paste0(
        "Treatment-comparison-plot-category-",
        plot_category,
        filename_suffix,
        sex_annotation,
        ".png"
      ),
      width = plot_width,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(percent_change_comparison_plot)
}




#' Plot variance comparison for a treatment
#'
#' `plot_variance_comparison_data()` creates a connected  plot with time as a
#' categorical variable (i.e. baseline/before and after) on the x-axis and the
#' variance measure on the y-axis. There are also lines
#' connecting the "before" data point to the "after" data point for each letter.
#'
#' If you specify a `test_type`, the function will perform a paired t-test or
#' paired wilcox test and add brackets with significance stars through
#' `ggsignif::geom_signif()`.
#'
#' This allows you to visually determine if a change in synaptic plasticity is
#' due to a pre- or post-synaptic mechanism. For more information, please see
#' [Huijstee & Kessels (2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).
#'
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_baseline_data
#' @inheritParams plot_PPR_data_single_treatment
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
#' @param post_hormone_label A character value for x-axis label applied to
#'   the post-hormone or post-protocol state. Defaults to "Post-hormone" but you
#'   will likely change this to the hormone or protocol name.
#' @param geom_signif_size A numeric value describing the size of the geom_signif bracket size. Defaults to 0.4, which is a good thickness for most applications.
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
#'   variance_measure = "cv",
#'   baseline_interval = "t0to5",
#'   post_hormone_interval = "t20to25",
#'   included_sexes = "both",
#'   post_hormone_label = "Insulin",
#'   test_type = "wilcox.test",
#'   large_axis_text = "no",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options
#' )
#'
plot_variance_comparison_data <- function(data,
                                          plot_category,
                                          plot_treatment,
                                          variance_measure = "cv",
                                          baseline_interval = "t0to5",
                                          baseline_label = "Baseline",
                                          post_hormone_interval = "t20to25",
                                          post_hormone_label = "Insulin",
                                          included_sexes = "both",
                                          male_label = "Male",
                                          female_label = "Female",
                                          test_type,
                                          map_signif_level_values = F,
                                          geom_signif_family = "",
                                          large_axis_text = "no",
                                          geom_signif_size = 0.4,
                                          treatment_colour_theme,
                                          theme_options,
                                          save_plot_png = "no",
                                          ggplot_theme = patchclampplotteR_theme()) {
  if (is.null(post_hormone_interval) ||
    !is.character(post_hormone_interval)) {
    cli::cli_abort(c("x" = "`post_hormone_interval` must be a character (e.g. \"t20to25\")"))
  }

  if (!test_type %in% c("wilcox.test", "t.test", "none")) {
    cli::cli_abort(c("x" = "`test_type` argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  variance_comparison_data <- data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment)

  allowed_y_variables_list <- "\"cv\" or \"VMR\""

  if (!variance_measure %in% c("cv", "VMR")) {
    cli::cli_abort(c("x" = paste0("`variance_measure` must be one of ", allowed_y_variables_list)))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }

  if (included_sexes == "male") {
    variance_comparison_data <- variance_comparison_data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"
  }

  if (included_sexes == "female") {
    variance_comparison_data <- variance_comparison_data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    variance_comparison_data <- variance_comparison_data
    sex_annotation <- ""
  }

  if (variance_measure == "cv") {
    variance_comparison_plot <- variance_comparison_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$interval,
        y = .data$cv_inverse_square,
        group = .data$letter
      )) +
      ggplot2::labs(y = "1/CV^2^")
  }

  if (variance_measure == "VMR") {
    variance_comparison_plot <- variance_comparison_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$interval,
        y = .data$VMR,
        group = .data$letter
      )) +
      ggplot2::labs(y = "VMR")
  }

  if (test_type != "none") {
    variance_comparison_plot <- variance_comparison_plot +
      ggsignif::geom_signif(
        comparisons = list(c(
          baseline_interval, post_hormone_interval
        )),
        test = test_type,
        test.args = list(paired = TRUE),
        map_signif_level = map_signif_level_values,
        vjust = -0.3,
        family = geom_signif_family,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = geom_signif_size
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
  }

  variance_comparison_plot <- variance_comparison_plot +
    ggplot2::geom_point(color = theme_options["connecting_line_colour", "value"], size = 1.8) +
    ggplot2::geom_line(color = theme_options["connecting_line_colour", "value"], linewidth = 0.4) +
    ggplot2::labs(x = NULL) +
    ggplot2::scale_x_discrete(labels = c(baseline_label, post_hormone_label)) +
    ggplot_theme +
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
        sex_annotation,
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
#' @inheritParams plot_baseline_data
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
#'   plot_category = 2,
#'   theme_options = sample_theme_options,
#'   included_sexes = "both",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   save_plot_png = "no"
#' )
#'
#' @seealso [plot_variance_comparison_data()] to make plots of inverse
#'   coefficient of variation squared and VMR, which are useful to determine if
#'   a mechanism is pre- or post-synaptic.


plot_cv_data <- function(data,
                         plot_treatment = "Control",
                         plot_category,
                         treatment_colour_theme,
                         included_sexes = "both",
                         male_label = "Male",
                         female_label = "Female",
                         theme_options,
                         save_plot_png = "no",
                         ggplot_theme = patchclampplotteR_theme()) {
  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  if (included_sexes == "male") {
    plot_data <- data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"
  }

  if (included_sexes == "female") {
    plot_data <- data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    plot_data <- data
    sex_annotation <- ""
  }

  cv_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$cv_P1_all_cells)) +
    ggplot2::geom_point(color = plot_colour) +
    ggplot2::labs(x = "Time (min)", y = "CV") +
    ggplot_theme

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = cv_plot,
      path = here::here("Figures/Evoked-currents/CV"),
      file = paste0("CV_plot-category-", plot_category, "-", plot_treatment, sex_annotation, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(cv_plot)
}



#' Make a PPR plot for a single treatment
#'
#' `plot_PPR_data_single_treatment()` creates a categorical scatter plot with
#' experimental state (i.e. baseline/before and after) on the x-axis and the
#' paired-pulse ratio (PPR) on the y-axis. There are also lines connecting the
#' "before" data point to the "after" data point for each letter.
#'
#' If you specify a `test_type`, the function will perform a paired t-test or
#' paired wilcox test and add brackets with significance stars through
#' `ggsignif::geom_signif()`.
#'
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_summary_current_data
#' @inheritParams plot_variance_comparison_data
#' @inheritParams plot_baseline_data
#' @param data Paired pulse ratio data generated from [make_PPR_data()].
#' @param test_type A character (must be "wilcox.test", "t.test" or "none")
#'   describing the statistical model used to create a significance bracket
#'   comparing the pre- and post-hormone groups.
#' @param map_signif_level_values A TRUE/FALSE value or a list of character values for mapping p-values. If TRUE, p-values will be mapped with asterisks (e.g. \* for p < 0.05, for p < 0.01). If FALSE, raw p-values will display. You can also insert a list of custom mappings.
#' @param geom_signif_size A numeric value describing the size of the `geom_signif` bracket size. Defaults to 0.4, which is a good thickness for most applications.
#' @param geom_signif_family A character value describing the font family used for the p-value annotations used by `ggsignif::geom_signif()`.
#' @export
#'
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Evoked-currents/PPR` relative to the
#'   project directory. The treatment will be included in the filename.
#'
#' @examples
#' plot_PPR_data_single_treatment(
#'   data = sample_PPR_df,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   baseline_label = "Baseline",
#'   post_hormone_label = "Insulin",
#'   included_sexes = "both",
#'   test_type = "t.test",
#'   large_axis_text = "no",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
#'
#' @seealso [plot_PPR_data_multiple_treatments()] to plot changes in PPR for multiple treatments.

plot_PPR_data_single_treatment <- function(data,
                                           plot_treatment = "Control",
                                           plot_category = 2,
                                           included_sexes = "both",
                                           male_label = "Male",
                                           female_label = "Female",
                                           baseline_label = "Baseline",
                                           post_hormone_label = "Post-hormone",
                                           test_type,
                                           map_signif_level_values = F,
                                           geom_signif_family = "",
                                           large_axis_text = "no",
                                           geom_signif_size = 0.4,
                                           treatment_colour_theme,
                                           theme_options,
                                           save_plot_png = "no",
                                           ggplot_theme = patchclampplotteR_theme()) {
  if (!large_axis_text %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`large_axis_text` argument must be either \"yes\" or \"no\""))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (!test_type %in% c("wilcox.test", "t.test", "none")) {
    cli::cli_abort(c("x" = "`test_type` argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)

  if (included_sexes == "male") {
    plot_data <- data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"

  }

  if (included_sexes == "female") {
    plot_data <- data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    plot_data <- data
    sex_annotation <- ""
  }


  PPR_one_plot <- plot_data %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::mutate(
      state = dplyr::case_match(
        .data$state,
        "Baseline" ~ baseline_label,
        "Post-modification" ~ post_hormone_label
      )
    ) %>%
    dplyr::group_by(.data$treatment, .data$state, .data$letter, .data$sex) %>%
    dplyr::summarize(mean_PPR_cell = mean(.data$PPR), .groups = "drop")


  if (included_sexes == "both") {
    PPR_one_plot <- PPR_one_plot %>%
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
      ggplot2::scale_shape_manual(values = c(as.numeric(theme_options["female_shape", "value"]), as.numeric(theme_options["male_shape", "value"])))
  }

  if (included_sexes != "both") {
    PPR_one_plot <- PPR_one_plot %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$state,
        y = .data$mean_PPR_cell
      )) +
      ggplot2::geom_point(
        size = 4,
        color = plot_colour,
        position = ggplot2::position_jitter(width = 0.04, height = 0),
        alpha = 0.8,
        shape = if (included_sexes == "male") {as.numeric(theme_options["male_shape", "value"])} else {as.numeric(theme_options["female_shape", "value"])}
      )
  }

  PPR_one_plot <- PPR_one_plot +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$letter),
      color = plot_colour,
      linewidth = as.numeric(theme_options["connecting_line_width", "value"]),
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
    ggplot_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(b = 5, t = 5))) +
    ggplot2::labs(x = NULL, y = "Paired pulse ratio", shape = "Sex")

  if (test_type != "none") {
    PPR_one_plot <- PPR_one_plot +
      ggsignif::geom_signif(
        comparisons = list(c(
          baseline_label, post_hormone_label
        )),
        test = test_type,
        test.args = list(paired = TRUE),
        map_signif_level = map_signif_level_values,
        family = geom_signif_family,
        vjust = -0.3,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = geom_signif_size,
        y_position = 2.5
      )
  }

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
      file = paste0("PPR_comparison-category-", plot_category, "-", plot_treatment, sex_annotation, ".png"),
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
#' letter. It is the same as [plot_PPR_data_single_treatment()] but for more
#' than one treatment.
#'
#' If you specify a `test_type`, the function will perform a paired t-test or
#' paired wilcox test and add brackets with significance stars through
#' `ggsignif::geom_signif()`.
#'
#' @inheritParams plot_baseline_data
#' @inheritParams plot_variance_comparison_data
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_PPR_data_single_treatment

#' @param data Paired pulse ratio data generated from [make_PPR_data()].
#' @param geom_signif_size A numeric value describing the size of the geom_signif bracket size. Defaults to 0.3, which is a good thickness for most applications.
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
#'   post_hormone_label = "I",
#'   included_sexes = "both",
#'   test_type = "t.test",
#'   theme_options = sample_theme_options,
#'   treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
#' @seealso [plot_PPR_data_single_treatment()] to plot changes in PPR for a single treatment.

plot_PPR_data_multiple_treatments <- function(data,
                                              include_all_treatments = "yes",
                                              list_of_treatments = NULL,
                                              plot_category = 2,
                                              included_sexes = "both",
                                              male_label = "Male",
                                              female_label = "Female",
                                              baseline_label = "B",
                                              post_hormone_label = "A",
                                              test_type,
                                              map_signif_level_values = F,
                                              geom_signif_family = "",
                                              treatment_colour_theme,
                                              theme_options,
                                              filename_suffix = "",
                                              geom_signif_size = 0.3,
                                              save_plot_png = "no",
                                              ggplot_theme = patchclampplotteR_theme()) {
  if (!include_all_treatments %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`include_all_treatments` must be either \"yes\" or \"no\"."))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!test_type %in% c("wilcox.test", "t.test", "none")) {
    cli::cli_abort(c("x" = "'test_type' argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      cli::cli_alert_info(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is NULL."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    if (!is.character(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }


  if (included_sexes == "male") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"

    plot_shape <- as.numeric(theme_options["male_shape", "value"])
  }

  if (included_sexes == "female") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"

    plot_shape <- as.numeric(theme_options["female_shape", "value"])
  }

  if (included_sexes == "both") {
    plot_data <- data
    sex_annotation <- ""

    plot_shape <- as.numeric(theme_options["both_sexes_shape", "value"])
  }

  PPR_summary_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::mutate(
      state = dplyr::case_match(
        .data$state,
        "Baseline" ~ baseline_label,
        "Post-modification" ~ post_hormone_label
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
      alpha = 0.9,
      shape = plot_shape
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = .data$treatment, group = .data$letter),
      linewidth = as.numeric(theme_options["connecting_line_width", "value"]),
      alpha = 0.3
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
    ggplot_theme +
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
    ggplot2::labs(x = NULL, y = "Paired pulse ratio")

  if (test_type != "none") {
    PPR_summary_plot <- PPR_summary_plot +
      ggsignif::geom_signif(
        comparisons = list(c(baseline_label, post_hormone_label)),
        test = test_type,
        test.args = list(paired = TRUE),
        map_signif_level = map_signif_level_values,
        family = geom_signif_family,
        vjust = -0.3,
        textsize = 4,
        size = geom_signif_size,
        margin_top = 0.1,
        extend_line = 0.03
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
  }

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = PPR_summary_plot,
      path = here::here("Figures/Evoked-currents/PPR"),
      file = paste0("PPR_Summary_plot-category-", plot_category, filename_suffix, sex_annotation, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(PPR_summary_plot)
}


#' Plot and compare action potential parameters before and after a treatment
#'
#' This function produces a connected line plot which allows you to visually compare action potential parameters such as peak amplitude, after-hyperpolarization amplitude (here, `antipeak_amplitude`), half-width, etc. before and after a treatment has been applied. It requires action potential data from two recordings - one taken during the baseline (`state = "Baseline"`) and one taken after a hormone or high-frequency protocol has been applied (in this example, `state = "Insulin"`).
#'
#' @inheritParams plot_PPR_data_single_treatment
#' @inheritParams plot_AP_frequencies_single_treatment
#'
#' @param data The action potential data generated from `add_new_cells()` with `data_type == "AP"`.
#' @param post_hormone_label A character value that MUST correspond to one of the values in the `State` column. In the sample dataset, this is `"Insulin"`. This is required for the wilcox or t.test comparisons of "Baseline" vs. "Insulin".
#' @param y_variable A character value naming the variable to be plotted on the y-axis. Must be a column present in `data`. Examples include `peak_amplitude`, `time_to_peak`, `antipeak_amplitude` and `half_width`.
#' @param y_axis_title A character value used to define a "pretty" version of `y_variable`. This will become the y-axis label on the ggplot. Examples include "Peak Amplitude (pA)" or "Time to Peak (ms)".
#' @param geom_signif_size A numeric value describing the size of the geom_signif bracket size. Defaults to 0.5, which is a good thickness for most applications.
#'
#' @returns A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Action-potentials` relative to the
#'   project directory. The treatment and y_variable will be included in the filename.
#'
#' @export
#'
#' @examples
#' plot_AP_comparison(
#'   sample_AP_data,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   included_sexes = "both",
#'   y_variable = "peak_amplitude",
#'   y_axis_title = "Peak Amplitude (pA)",
#'   theme_options = sample_theme_options,
#'   baseline_label = "Baseline",
#'   test_type = "wilcox.test",
#'   post_hormone_label = "Insulin",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   save_plot_png = "no"
#' )
plot_AP_comparison <-
  function(data,
           plot_treatment = "Control",
           plot_category = 2,
           included_sexes = "both",
           male_label = "Male",
           female_label = "Female",
           baseline_label = "Baseline",
           post_hormone_label = "Post-hormone",
           y_variable,
           y_axis_title,
           test_type,
           map_signif_level_values = F,
           geom_signif_family = "",
           treatment_colour_theme,
           theme_options,
           baseline_shape = 16,
           post_treatment_shape = 17,
           geom_signif_size = 0.5,
           save_plot_png = "no",
           ggplot_theme = patchclampplotteR_theme()) {
    if (!save_plot_png %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
    }

    if (!test_type %in% c("wilcox.test", "t.test", "none")) {
      cli::cli_abort(c("x" = "'test_type' argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
    }

    if (!included_sexes %in% c("both", "male", "female")) {
      cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
    }


    plot_colour <- treatment_colour_theme %>%
      dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
      dplyr::pull(.data$colours)

    if (included_sexes == "male") {
      plot_data <- data %>%
        dplyr::filter(.data$sex == male_label)

      sex_annotation <- "-males-only"
    }

    if (included_sexes == "female") {
      plot_data <- data %>%
        dplyr::filter(.data$sex == female_label)

      sex_annotation <- "-females-only"
    }

    if (included_sexes == "both") {
      plot_data <- data
      sex_annotation <- ""
    }


    ap_parameter_plot <- plot_data %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::filter(.data$category == plot_category) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$state, y = .data[[y_variable]], color = .data$state, shape = .data$state)) +
      ggplot2::geom_line(
        ggplot2::aes(group = .data$letter),
        linewidth = as.numeric(theme_options["connecting_line_width", "value"]),
        color = theme_options["mean_point_colour", "value"],
        alpha = 0.3
      ) +
      ggplot2::geom_point(
        alpha = 0.9,
        size = 4,
        position = ggplot2::position_jitter(0.04)
      ) +
      ggplot2::scale_color_manual(
        values = c(theme_options["baseline_group_colour", "value"], plot_colour)
      ) +
      ggplot2::scale_shape_manual(
        values = c(baseline_shape, post_treatment_shape)
      ) +
      ggplot2::stat_summary(
        fun.data = ggplot2::mean_se,
        geom = "pointrange",
        color = theme_options["mean_point_colour", "value"],
        size = as.numeric(theme_options["mean_point_size", "value"]) + 0.2,
        alpha = 1,
        position = ggplot2::position_nudge(x = -0.02),
        show.legend = FALSE
      ) +
      ggplot_theme +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = NULL, y = y_axis_title)


    if (test_type != "none") {
      ap_parameter_plot <- ap_parameter_plot + ggsignif::geom_signif(
        comparisons = list(c(baseline_label, post_hormone_label)),
        test = test_type,
        test.args = list(paired = TRUE),
        map_signif_level = map_signif_level_values,
        family = geom_signif_family,
        vjust = -0.3,
        textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
        size = geom_signif_size,
        color = theme_options["baseline_group_colour", "value"]
      ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
    }


    if (save_plot_png == "yes") {
      ggplot2::ggsave(
        plot = ap_parameter_plot,
        path = here::here("Figures/Action-potentials/"),
        file = paste0("AP-", y_variable, "-comparison-category-", plot_category, "-", plot_treatment, sex_annotation, ".png"),
        width = 7,
        height = 5,
        units = "in",
        dpi = 300
      )
    }


    return(ap_parameter_plot)
  }


#' Plot action potential frequency curves for a single treatment
#'
#' This function allows you to generate a plot of action potential frequency (y-axis) for each current injection (x-axis), coloured by state (baseline or post-hormone/treatment).
#'
#'
#' @inheritParams plot_AP_frequencies_multiple_treatments
#' @inheritParams plot_PPR_data_single_treatment
#' @inheritParams plot_spontaneous_current_parameter_comparison
#'
#' @param data Action potential frequency data imported through `add_new_cells()` with `data_type == "AP_count"`
#' @param p_adjust_method This argument is directly related to `p.adjust.method` in `rstatix::t_test`. This is the method used to adjust the p-value in multiple pairwise comparisons. Allowed values include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" (although "none" is not recommended).
#' @param significance_display_method A character value ("stars" or "p-value") describing how significance values should be displayed. These annotations will not appear if `test_type` is "none".)
#' @param geom_signif_y_spacer A numeric value describing the vertical spacing applied to the significance markers. Defaults to 1, but can be set to a higher value if the p-values or significance stars are too close to the error bars.
#' @param geom_signif_size A numeric value describing the size of the text annotations (significance stars or p-values) on the plot. Defaults to `5`.
#' @param save_output_as_RDS A character ("yes" or "no") describing if the
#'   resulting object should be saved as an RDS file in the folder 'Data/Output-Data-from-R'. The function will automatically create this folder if it doesn't already exist.
#' @param baseline_shape A numeric value describing the shape used for the baseline data. Defaults to 16, which is a circle.
#' @param post_treatment_shape A numeric value describing the shape used for the post-treatment/post-protocol data. Defaults to 17, which is a triangle.
#' @returns
#'
#' A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Action-potentials` relative to the
#'   project directory, with the treatment and category included in the filename.
#' @export
#'
#' @examples
#' plot_AP_frequencies_single_treatment(
#'   data = sample_AP_count_data,
#'   plot_treatment = "Control",
#'   plot_category = 2,
#'   baseline_label = "Baseline",
#'   hormone_added = "Insulin",
#'   included_sexes = "both",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   large_axis_text = "no",
#'   test_type = "wilcox.test",
#'   p_adjust_method = "holm",
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
#'
plot_AP_frequencies_single_treatment <- function(data,
                                                 plot_treatment,
                                                 plot_category,
                                                 included_sexes = "both",
                                                 male_label = "Male",
                                                 female_label = "Female",
                                                 large_axis_text = "no",
                                                 baseline_label = "Baseline",
                                                 hormone_added,
                                                 test_type,
                                                 significance_display_method = "p-values",
                                                 geom_signif_size = 5,
                                                 geom_signif_y_spacer = 1,
                                                 geom_signif_family = "",
                                                 p_adjust_method = "holm",
                                                 save_plot_png = "no",
                                                 treatment_colour_theme,
                                                 baseline_shape = 16,
                                                 post_treatment_shape = 17,
                                                 save_output_as_RDS = "no",
                                                 theme_options,
                                                 ggplot_theme = patchclampplotteR_theme()) {
  if (!large_axis_text %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`large_axis_text` argument must be either \"yes\" or \"no\""))
  }

  if (!save_output_as_RDS %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_output_as_RDS` argument must be one of: \"yes\" or \"no\""))
  }

  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (!test_type %in% c("wilcox.test", "t.test", "none")) {
    cli::cli_abort(c("x" = "'test_type' argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
  }

  if (!significance_display_method %in% c("stars", "p-values")) {
    cli::cli_abort(c("x" = "'significance_display_method' argument must be one of: \"stars\" or \"p-values\"."))
  }

  if (!p_adjust_method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) {
    cli::cli_abort(c("x" = "`p_adjust_method` argument must be one of: \"holm\", \"hochberg\", \"hommel\", \"bonferroni\", \"BH\", \"BY\", \"fdr\", or \"none\""))
  }

  plot_colour <- treatment_colour_theme %>%
    dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
    dplyr::pull(.data$colours)



  if (included_sexes == "male") {
    ap_data <- data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"
  }

  if (included_sexes == "female") {
    ap_data <- data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    ap_data <- data
    sex_annotation <- ""
  }

  ap_plot_count_data <- ap_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::filter(.data$treatment == plot_treatment) %>%
    dplyr::group_by(.data$state, .data$current_injection) %>%
    dplyr::summarize(
      mean_AP_frequency = mean(.data$AP_frequency),
      SE = stats::sd(.data$AP_frequency) / sqrt(dplyr::n()),
      n = dplyr::n()
    )

  single_treatment_AP_plot <- ap_plot_count_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$current_injection,
        y = .data$mean_AP_frequency,
        ymin = .data$mean_AP_frequency - .data$SE,
        ymax = .data$mean_AP_frequency + .data$SE,
        color = .data$state,
        shape = .data$state
      )
    ) +
    ggplot2::geom_pointrange(size = 1, linewidth = 0.6) +
    ggplot2::labs(x = "Current Injection (pA)", y = "AP Frequency (Hz)", color = NULL, shape = NULL) +
    ggplot2::scale_color_manual(
      values = c(theme_options["mean_point_colour", "value"], plot_colour),
      labels = c(
        paste0(
          baseline_label,
          ", n = ",
          ap_plot_count_data %>%
            dplyr::filter(.data$state == baseline_label) %>%
            dplyr::pull(.data$n) %>%
            dplyr::first()
        ),
        paste0(
          hormone_added,
          ", n = ",
          ap_plot_count_data %>%
            dplyr::filter(.data$state == hormone_added) %>%
            dplyr::pull(.data$n) %>%
            dplyr::first()
        )
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(baseline_shape, post_treatment_shape),
      labels = c(
        paste0(
          baseline_label,
          ", n = ",
          ap_plot_count_data %>%
            dplyr::filter(.data$state == baseline_label) %>%
            dplyr::pull(.data$n) %>%
            dplyr::first()
        ),
        paste0(
          hormone_added,
          ", n = ",
          ap_plot_count_data %>%
            dplyr::filter(.data$state == hormone_added) %>%
            dplyr::pull(.data$n) %>%
            dplyr::first()
        )
      )
    )

  if (test_type != "none") {
    # Maximum y-axis values required for correct positioning of significance stars over plot
    max_mean_AP_frequencies <- ap_plot_count_data %>%
      dplyr::group_by(.data$current_injection) %>%
      dplyr::summarize(
        max_AP_frequency = max(.data$mean_AP_frequency),
        max_se = max(.data$SE)
      )

    # Group data for comparisons
    frequency_comparison_model <- ap_data %>%
      dplyr::filter(.data$category == plot_category) %>%
      dplyr::filter(.data$treatment == plot_treatment) %>%
      dplyr::group_by(.data$current_injection)


    if (test_type == "t.test") {
      frequency_comparison_test_results <- frequency_comparison_model %>%
        rstatix::pairwise_t_test(
          AP_frequency ~ state,
          ref.group = baseline_label,
          paired = T,
          p.adjust.method = p_adjust_method
        )

      test_annotation <- "t-test"
    }

    if (test_type == "wilcox.test") {
      frequency_comparison_test_results <- frequency_comparison_model %>%
        rstatix::pairwise_wilcox_test(
          AP_frequency ~ state,
          ref.group = baseline_label,
          paired = T,
          p.adjust.method = p_adjust_method
        )

      test_annotation <- "wilcox-test"
    }

    # Generate cleaned results columns
    frequency_comparison_test_results <- frequency_comparison_test_results %>%
      dplyr::mutate(
        statistic = round(.data$statistic, 2),
        p_string = lazyWeave::pvalString(.data$p),
        significance_stars = dplyr::case_when(p.adj.signif == "ns" ~ "", T ~ p.adj.signif)
      )

    frequency_comparison_test_results_final <- merge(frequency_comparison_test_results, max_mean_AP_frequencies, by = "current_injection")

    if (save_output_as_RDS == "yes") {
      RDS_path <- "Data/Output-Data-from-R/"

      if (!dir.exists(RDS_path)) {
        dir.create(RDS_path)
      }

      saveRDS(
        frequency_comparison_test_results_final,
        paste0(
          RDS_path,
          "AP-frequency-comparison-category-",
          plot_category,
          "-",
          plot_treatment,
          "-",
          test_annotation,
          sex_annotation,
          ".rds"
        )
      )
    }

    if (significance_display_method == "stars") {
      single_treatment_AP_plot <- single_treatment_AP_plot +
        ggplot2::geom_text(
          data = frequency_comparison_test_results_final,
          ggplot2::aes(
            x = .data$current_injection,
            y = .data$max_AP_frequency + .data$max_se + geom_signif_y_spacer,
            label = .data$significance_stars
          ),
          inherit.aes = F,
          size = geom_signif_size
        )
    }

    if (significance_display_method == "p-values") {
      single_treatment_AP_plot <- single_treatment_AP_plot +
        ggplot2::geom_text(
          data = frequency_comparison_test_results_final,
          ggplot2::aes(
            x = .data$current_injection,
            y = .data$max_AP_frequency + .data$max_se + geom_signif_y_spacer,
            label = .data$p_string
          ),
          inherit.aes = F,
          size = geom_signif_size,
          family = geom_signif_family
        )
    }
  }


  single_treatment_AP_plot <- single_treatment_AP_plot +
    ggplot_theme +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.14, 0.8),
      legend.key.spacing.y = grid::unit(1.5, "lines"),
      axis.title = ggplot2::element_text(face = "plain")
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, .1)))

  if (large_axis_text == "yes") {
    single_treatment_AP_plot <- single_treatment_AP_plot +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = 28,
          face = "plain",
          margin = ggplot2::margin(t = 10)
        ),
        axis.title.y = ggplot2::element_text(size = 28, face = "plain"),
        legend.text = ggplot2::element_text(size = 18),
        legend.position.inside = c(0.24, 0.8),
        legend.key.spacing.y = grid::unit(0.5, "cm")
      )
  }


  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = single_treatment_AP_plot,
      path = here::here("Figures/Action-potentials"),
      file = paste0("AP-frequency-category-", plot_category, "-", plot_treatment, sex_annotation, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }
  return(single_treatment_AP_plot)
}


#' Plot action potential frequency curves for multiple treatments
#'
#' This function allows you to generate a plot of action potential frequency (y-axis) for each current injection (x-axis), coloured by treatment. The linetype indicates which state the data belong to (a recording taken during the baseline or after a hormone or treatment).
#'
#' @param data Action potential frequency data imported through `add_new_cells()` with `data_type == "AP_count"`
#'
#' @inheritParams plot_PPR_data_multiple_treatments
#'
#' @returns
#'
#' A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Action-potentials` relative to the
#'   project directory.
#' @export
#'
#' @examples
#'
#' plot_AP_frequencies_multiple_treatments(
#'   data = sample_AP_count_data,
#'   include_all_treatments = "yes",
#'   plot_category = 2,
#'   included_sexes = "both",
#'   treatment_colour_theme = sample_treatment_names_and_colours
#' )
#'
plot_AP_frequencies_multiple_treatments <- function(data,
                                                    include_all_treatments = "yes",
                                                    list_of_treatments = NULL,
                                                    plot_category = 2,
                                                    included_sexes = "both",
                                                    male_label = "Male",
                                                    female_label = "Female",
                                                    treatment_colour_theme,
                                                    filename_suffix = "",
                                                    save_plot_png = "no",
                                                    ggplot_theme = patchclampplotteR_theme()) {
  if (!include_all_treatments %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`include_all_treatments` must be either \"yes\" or \"no\"."))
  }

  if (!included_sexes %in% c("both", "male", "female")) {
    cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
  }


  if (!save_plot_png %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
  }

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_colour_theme
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_colour_theme$treatment) %>%
      droplevels()

    if (!is.null(list_of_treatments)) {
      cli::cli_alert_info(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is NULL."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    if (!is.character(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }
  }

  if (include_all_treatments == "no") {
    treatment_info <- treatment_colour_theme %>%
      dplyr::filter(.data$treatment %in% list_of_treatments)
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% list_of_treatments) %>%
      droplevels()
  }

  if (included_sexes == "male") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == male_label)

    sex_annotation <- "-males-only"
  }

  if (included_sexes == "female") {
    plot_data <- plot_data %>%
      dplyr::filter(.data$sex == female_label)

    sex_annotation <- "-females-only"
  }

  if (included_sexes == "both") {
    sex_annotation <- ""
  }

  AP_frequency_plot <- plot_data %>%
    dplyr::filter(.data$category == plot_category) %>%
    dplyr::group_by(.data$treatment, .data$state, .data$current_injection) %>%
    dplyr::summarize(
      mean_AP_frequency = mean(.data$AP_frequency),
      SE = stats::sd(.data$AP_frequency) / sqrt(dplyr::n()),
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$current_injection,
        y = .data$mean_AP_frequency,
        ymin = .data$mean_AP_frequency - .data$SE,
        ymax = .data$mean_AP_frequency + .data$SE,
        color = .data$treatment,
        linetype = .data$state
      )
    ) +
    ggplot2::stat_smooth(geom = "line", lineend = "round") +
    ggplot2::geom_point(alpha = 0) +
    ggplot2::scale_linetype_manual(values = c("solid", "dotted")) +
    ggplot2::scale_color_manual(
      breaks = treatment_info$treatment,
      labels = treatment_info$display_names,
      values = treatment_info$colours
    ) +
    ggplot2::labs(
      x = "Current Injection (pA)",
      y = "AP Frequency (Hz)",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = 0, alpha = 1))) +
    ggplot_theme

  if (save_plot_png == "yes") {
    ggplot2::ggsave(
      plot = AP_frequency_plot,
      path = here::here("Figures/Action-potentials/"),
      file = paste0("AP-frequency-category-", plot_category, filename_suffix, sex_annotation, ".png"),
      width = 7,
      height = 5,
      units = "in",
      dpi = 300
    )
  }

  return(AP_frequency_plot)
}

#' Plot action potential recording
#'
#' This function allows you to plot an `.abf` file of a recording taken in current clamp mode. It is useful if you want to display a representative trace of action potentials or the results of a current injection protocol.
#'
#' @inheritParams plot_baseline_data
#' @inheritParams plot_spontaneous_current_trace
#'
#' @param data A dataframe generated using `import_ABF_file()` with `recording_mode = "current_clamp"`.
#' @param sweeps A character value or list of character values of the sweeps you would like to plot. These correspond to the values in the `sweep1` column of your dataset, and will likely be in the form of "epi1", "epi2", etc.
#' @param line_width A numeric value specifying the width of the lineplot
#' @param trace_colour A hex value of the colour of the lineplot. Use if `colour_scale_option = "single_colour`.
#' @param colour_scale_option A character value ("viridis", "custom" or "single_colour") describing what colour scale should be applied to the trace. If set to "viridis" or "custom", the trace will be coloured by sweep.
#' @param custom_scale_colours A list of character values (can be hex values or named colours) describing the custom theme. Use if `colour_scale_option = "custom"`.
#' @param scale_bar_x_start A numeric value (in milliseconds) describing the x-axis position of
#'   the scale bar (default is 880).
#' @param scale_bar_x_length A numeric value describing the horizontal span (in
#'   milliseconds) of the scale bar (default is 100).
#' @param scale_bar_y_start A numeric value describing the y-axis position (in mV) of
#'   the scale bar (default is -30).
#' @param scale_bar_y_length A numeric value describing the vertical span (in
#'   mV) of the scale bar (default is 40).
#' @param scaling_factor A numeric value describing the scaling factor applied by Clampfit to convert recording time to time in milliseconds. The default is 10, and this value will likely not need to be changed.
#' @param scale_bar_linewidth A numeric value describing the thickness of the scalebar line (default is 0.6).
#' @param ... Additional arguments passed to `viridis::scale_color_viridis` such as `begin`, `end`, `option` and `direction`
#'
#' @returns
#'
#' A ggplot object. If `save_plot_png == "yes"`, it will also generate
#'   a .png file in the folder `Figures/Action-potentials/Representative-traces` relative to the
#'   project directory.
#'
#' @export
#'
#' @examples
#'
#' # Custom colours
#' plot_AP_trace(
#'   data = sample_ap_abf_baseline,
#'   sweeps = as.character(unique(sample_ap_abf_baseline$episode)),
#'   custom_scale_colours = c(
#'     "#edd03a", "#cced34",
#'     "#a3fd3d", "#6bfe64",
#'     "#31f199", "#18dcc3",
#'     "#29bbec", "#4294ff",
#'     "#466be3", "#4040a2"
#'   ),
#'   colour_scale_option = "custom",
#'   plot_category = 2,
#'   plot_treatment = "Control"
#' )
#'
#' # Single colour
#' plot_AP_trace(
#'   data = sample_ap_abf_baseline,
#'   sweeps = as.character(unique(sample_ap_abf_baseline$episode)),
#'   colour_scale_option = "single_colour",
#'   trace_colour = "#4294ff",
#'   plot_category = 2,
#'   plot_treatment = "Control"
#' )
#'
plot_AP_trace <-
  function(data,
           sweeps,
           colour_scale_option,
           custom_scale_colours = NULL,
           trace_colour,
           line_width = 0.7,
           plot_category,
           plot_treatment,
           state,
           include_scale_bar = "yes",
           scale_bar_x_start = 880,
           scale_bar_x_length = 100,
           scaling_factor = 10,
           scale_bar_y_start = -30,
           scale_bar_y_length = 40,
           scale_bar_linewidth = 0.6,
           save_plot_png = "no",
           filename_suffix, ...) {
    if (!save_plot_png %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
    }

    if (!include_scale_bar %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`include_scale_bar` argument must be one of: \"yes\" or \"no\""))
    }

    if (!colour_scale_option %in% c("custom", "viridis", "single_colour")) {
      cli::cli_abort(c("x" = "`colour_scale_option` argument must be one of: \"custom\", \"viridis\" or \"single_colour\""))
    }

    if (colour_scale_option == "single_colour") {
      if (is.null(trace_colour) ||
        !is.character(trace_colour)) {
        cli::cli_abort(c(
          "x" = "You set `colour_scale_option` to `single_colour` but `trace_colour` is blank or not a character.",
          "i" = "Please set `trace_colour` to a hex value (e.g. \"#32a852\" or named colour like \"orange\")"
        ))
      }

      ap_trace <- data %>%
        dplyr::filter(.data$episode %in% sweeps) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$time,
          y = .data$voltage,
          group = .data$episode
        )) +
        ggplot2::geom_line(color = trace_colour, linewidth = line_width) +
        ggplot2::theme_void()
    }

    if (colour_scale_option %in% c("custom", "viridis")) {
      ap_trace <- data %>%
        dplyr::filter(.data$episode %in% sweeps) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$time,
            y = .data$voltage,
            group = .data$episode,
            color = .data$episode
          )
        ) +
        ggplot2::geom_line(linewidth = line_width) +
        ggplot2::theme_void()

      if (colour_scale_option == "viridis") {
        ap_trace <- ap_trace +
          viridis::scale_color_viridis(discrete = T, guide = "none", ...)
      }

      if (colour_scale_option == "custom") {
        if (is.null(custom_scale_colours)) {
          cli::cli_abort(c("x" = "You set `colour_scale_option` to \"custom\" but did not define a custom scale. Please insert a list into `custom_scale_colours`."))
        }
        ap_trace <- ap_trace +
          ggplot2::scale_colour_manual(values = custom_scale_colours, guide = "none")
      }
    }

    if (include_scale_bar == "yes") {
      ap_trace <- ap_trace +
        ggplot2::annotate(
          "segment",
          x = scale_bar_x_start * scaling_factor,
          xend = scale_bar_x_start * scaling_factor + scale_bar_x_length * scaling_factor,
          y = scale_bar_y_start,
          yend = scale_bar_y_start,
          lwd = scale_bar_linewidth
        ) +
        ggplot2::annotate(
          "segment",
          x = scale_bar_x_start * scaling_factor,
          xend = scale_bar_x_start * scaling_factor,
          y = scale_bar_y_start,
          yend = scale_bar_y_start + scale_bar_y_length,
          lwd = scale_bar_linewidth
        ) +
        ggplot2::annotate(
          "text",
          x = scale_bar_x_start * scaling_factor - 100,
          y = scale_bar_y_start + 0.5 * scale_bar_y_length,
          label = paste0(scale_bar_y_length, "mV"),
          hjust = 1,
          vjust = 0.5
        ) +
        ggplot2::annotate(
          "text",
          x = scale_bar_x_start * scaling_factor + 0.5 * scale_bar_x_length *
            scaling_factor,
          y = scale_bar_y_start - 5,
          label = paste0(scale_bar_x_length, "ms"),
          hjust = 0.5,
          vjust = 0.5
        )
    }

    if (save_plot_png == "yes") {
      ggplot2::ggsave(
        plot = ap_trace,
        path = here::here("Figures/Action-potentials/Representative-Traces"),
        file = paste0(
          "Action-potential-trace-category-",
          plot_category,
          "-",
          plot_treatment,
          "-",
          state,
          "-",
          filename_suffix,
          ".png"
        ),
        width = 7,
        height = 5,
        units = "in",
        dpi = 300
      )
    }

    return(ap_trace)
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
#' If you specify a `test_type`, the function will perform a paired t-test or
#' paired wilcox test and add brackets with significance stars through
#' `ggsignif::geom_signif()`.
#'
#' @param data Summary data for spontaneous currents generated using [make_summary_EPSC_data()] where `current_type == "sEPSC"`.
#' @param y_variable A character value ("raw_amplitude" or "raw_frequency") only.
#'   Normalized amplitude and frequency are not available because all baseline
#'   values are 100.
#' @param geom_signif_size A numeric value describing the size of the geom_signif bracket size. Defaults to 0.5, which is a good thickness for most applications.
#'
#' @inheritParams plot_variance_comparison_data
#' @inheritParams plot_baseline_data
#' @inheritParams plot_raw_current_data
#' @inheritParams plot_PPR_data_single_treatment
#' @inheritParams plot_baseline_data
#' @returns A ggplot object
#' @export
#'
#' @examples
#' plot_spontaneous_current_parameter_comparison(
#'   data = sample_summary_sEPSC_df$summary_data,
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   y_variable = "raw_amplitude",
#'   included_sexes = "both",
#'   hormone_added = "Insulin",
#'   baseline_interval = "t0to5",
#'   post_hormone_interval = "t20to25",
#'   test_type = "wilcox.test",
#'   large_axis_text = "no",
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   theme_options = sample_theme_options,
#'   save_plot_png = "no"
#' )
#'
plot_spontaneous_current_parameter_comparison <-
  function(data,
           plot_category = 2,
           plot_treatment = "Control",
           included_sexes = "both",
           male_label = "Male",
           female_label = "Female",
           y_variable = "raw_amplitude",
           hormone_added = "Insulin",
           baseline_interval = "t0to5",
           post_hormone_interval = "t20to25",
           test_type,
           map_signif_level_values = F,
           geom_signif_family = "",
           large_axis_text = "no",
           treatment_colour_theme,
           geom_signif_size = 0.5,
           theme_options,
           save_plot_png,
           ggplot_theme = patchclampplotteR_theme()) {
    if (is.null(baseline_interval) ||
      !is.character(baseline_interval)) {
      cli::cli_abort(c("x" = "`baseline_interval` must be a character (e.g. \"t0to5\" or \"t0to3\")"))
    }

    if (!save_plot_png %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
    }

    if (!included_sexes %in% c("both", "male", "female")) {
      cli::cli_abort(c("x" = "`included_sexes` argument must be one of: \"both\", \"male\" or \"female\""))
    }


    if (is.null(post_hormone_interval) ||
      !is.character(post_hormone_interval)) {
      cli::cli_abort(c("x" = "`post_hormone_interval` must be a character (e.g. \"t20to25\")"))
    }

    if (!test_type %in% c("wilcox.test", "t.test", "none")) {
      cli::cli_abort(c("x" = "`test_type` argument must be one of: \"wilcox.test\", \"t.test\", or \"none\""))
    }

    allowed_y_variables_list <- "\"raw_amplitude\", or \"raw_frequency\""

    if (!y_variable %in% c("raw_amplitude", "raw_frequency")) {
      cli::cli_abort(c(
        "x" = paste0(
          "`y_variable` must be ",
          allowed_y_variables_list
        ),
        "i" = "`y_variable` cannot be baseline transformed data because the graph would not show useful data. During the normalization process (`make_normalized_EPSC_data()`), all baseline values are converted to 100. This plot would therefore show points all at 100."
      ))
    }

    plot_colour <- treatment_colour_theme %>%
      dplyr::filter(.data$category == plot_category & .data$treatment == plot_treatment) %>%
      dplyr::pull(.data$colours)



    sEPSC_comparison_plot_data <- data %>%
      dplyr::filter(.data$category == plot_category &
        .data$treatment == plot_treatment) %>%
      dplyr::filter(.data$interval == baseline_interval |
        .data$interval == post_hormone_interval)

    if (y_variable == "raw_amplitude") {
      y_var <- "mean_raw_amplitude"
      y_title <- "sEPSC Amplitude (pA)"
    }

    if (y_variable == "raw_frequency") {
      y_var <- "mean_raw_frequency"
      y_title <- "sEPSC Frequency (Hz)"
    }

    if (included_sexes == "male") {
      sEPSC_comparison_plot_data <- sEPSC_comparison_plot_data %>%
        dplyr::filter(.data$sex == male_label)

      sex_annotation <- "-males-only"
    }

    if (included_sexes == "female") {
      sEPSC_comparison_plot_data <- sEPSC_comparison_plot_data %>%
        dplyr::filter(.data$sex == female_label)

      sex_annotation <- "-females-only"
    }

    if (included_sexes == "both") {
      sex_annotation <- ""
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
      ggplot2::stat_summary(
        fun.data = ggplot2::mean_se,
        geom = "pointrange",
        color = theme_options["mean_point_colour", "value"],
        size = as.numeric(theme_options["mean_point_size", "value"]) + 0.2,
        alpha = 0.8
      ) +
      ggplot_theme


    if (test_type != "none") {
      sEPSC_comparison_plot <- sEPSC_comparison_plot +
        ggsignif::geom_signif(
          comparisons = list(c(
            baseline_interval, post_hormone_interval
          )),
          test = test_type,
          test.args = list(paired = TRUE),
          map_signif_level = map_signif_level_values,
          family = geom_signif_family,
          vjust = -0.3,
          textsize = as.numeric(theme_options["geom_signif_text_size", "value"]),
          size = geom_signif_size
        ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, .2)))
    }

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
          y_variable,
          "-category-",
          plot_category,
          "-",
          plot_treatment,
          sex_annotation,
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
#' @inheritParams plot_baseline_data
#' @inheritParams plot_raw_current_data
#'
#' @param data A dataframe containing at least these columns: `time`,
#'   `episode`, `current`, `voltage`, `time_sec`. An easy way to obtain this is
#'   by importing a raw .abf file through the [import_ABF_file()] function.
#' @param state A character value describing if the recording was taken during the baseline period or post-treatment/protocol. Examples include "Baseline", "Post-insulin". The `state` will be included in the .png filename if `save_plot_png = "yes"`.
#' @param include_scale_bar A character value that determines if a scale bar
#'   will be added to the plot. Allowed values are "yes" and "no".
#' @param plot_episode A character value describing the sweep (e.g. `epi1`) that
#'   will be used for the plot.
#' @param scale_bar_x_start A numeric value (in seconds) describing the x-axis position of
#'   the scale bar (default is 1.25).
#' @param scale_bar_x_length A numeric value describing the horizontal span (in
#'   seconds) of the scale bar. This will automatically be converted and
#'   displayed in milliseconds (default is 0.5).
#' @param scale_bar_y_start A numeric value describing the y-axis position (in pA) of
#'   the scale bar (default is 15).
#' @param scale_bar_y_length A numeric value describing the vertical span (in
#'   pA) of the scale bar (default is 20).
#' @param scale_bar_linewidth A numeric value describing the thickness of the scalebar line (default is 0.4).
#' @param plot_colour A character value naming the colour of the plot.
#' @param plot_x_min A numeric value describing the minimum value on the x-axis
#'   (in seconds).
#' @param plot_x_max A numeric value describing the maximum value on the x-axis
#'   (in seconds).
#' @param plot_y_min A numeric value describing the minimum value on the y-axis
#'   (in pA).
#' @param plot_y_max A numeric value describing the maximum value on the y-axis
#'   (in pA).
#' @returns A ggplot object. If save_plot_png is defined as "yes", it will also
#'   generate a .png file in the folder
#'   `Figures/Spontaneous-currents/Representative-Traces` relative to the
#'   project directory, with the `plot_category`, `plot_treatment`, and `state` included in the filename.
#'
#' @export
#'
#' @examples
#' plot_spontaneous_current_trace(
#'   data = sample_abf_file,
#'   plot_colour = "#6600cc",
#'   plot_category = 2,
#'   plot_treatment = "Control",
#'   state = "Baseline",
#'   include_scale_bar = "yes",
#'   plot_episode = "epi1",
#'   scale_bar_x_length = 1,
#'   scale_bar_y_length = 10,
#'   plot_x_min = 1,
#'   plot_x_max = 3
#' )
#'
plot_spontaneous_current_trace <-
  function(data,
           plot_colour,
           plot_category,
           plot_treatment,
           state,
           include_scale_bar = "yes",
           plot_episode = "epi1",
           scale_bar_x_start = 1.25,
           scale_bar_x_length = 0.5,
           scale_bar_y_start = 15,
           scale_bar_y_length = 20,
           scale_bar_linewidth = 0.4,
           plot_x_min = 1,
           plot_x_max = 5,
           plot_y_min = -100,
           plot_y_max = 35,
           save_plot_png = "no",
           ggplot_theme = patchclampplotteR_theme()) {
    if (!include_scale_bar %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`include_scale_bar` argument must be one of: \"yes\" or \"no\""))
    }

    if (!save_plot_png %in% c("yes", "no")) {
      cli::cli_abort(c("x" = "`save_plot_png` argument must be either \"yes\" or \"no\""))
    }

    representative_traces_plot <- data %>%
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
          lwd = scale_bar_linewidth
        ) +
        ggplot2::annotate(
          "segment",
          x = scale_bar_x_start,
          xend = scale_bar_x_start,
          y = scale_bar_y_start,
          yend = scale_bar_y_start + scale_bar_y_length,
          lwd = scale_bar_linewidth
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

    if (save_plot_png == "yes") {
      ggplot2::ggsave(
        plot = representative_traces_plot,
        path = here::here("Figures/Spontaneous-currents/Representative-Traces"),
        file = paste0("Spontaneous-current-trace-category-", plot_category, "-", plot_treatment, "-", state, ".png"),
        width = 7,
        height = 5,
        units = "in",
        dpi = 300
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
#'   resulting object should be saved as an RDS file in the folder 'Data/Output-Data-from-R'. The function will automatically create this folder if it doesn't already exist.
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
#' # Note, the number of treatments is limited to "Control" and "PPP" to reduce run-time
#' make_interactive_summary_table(
#'   cell_characteristics_dataframe = sample_cell_characteristics,
#'   pruned_eEPSC_dataframe = sample_pruned_eEPSC_df,
#'   pruned_sEPSC_dataframe = sample_pruned_sEPSC_df,
#'   treatment_colour_theme = sample_treatment_names_and_colours,
#'   include_all_treatments = "no",
#'   list_of_treatments = c("Control", "PPP"),
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
                                           save_output_as_RDS = "no",
                                           ggplot_theme = patchclampplotteR_theme()) {
  if (!save_output_as_RDS %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`save_output_as_RDS` argument must be one of: \"yes\" or \"no\""))
  }

  if (!include_all_treatments %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`include_all_treatments` must be either \"yes\" or \"no\"."))
  }

  if (!include_all_categories %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "`include_all_categories` must be either \"yes\" or \"no\"."))
  }

  table_data <-
    merge(pruned_eEPSC_dataframe$for_table, pruned_sEPSC_dataframe$for_table, by = "letter") %>%
    merge(cell_characteristics_dataframe, by = "letter") %>%
    merge(treatment_colour_theme %>% dplyr::select(-.data$category), by = "treatment") %>%
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
        .data$days_alone,
        .data$animal_or_slicing_problems,
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
      cli::cli_alert_info(
        "include_all_treatments = \"yes\", but you included a list of treatments to filter. All treatments will be used."
      )
    }
  } else {
    if (is.null(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is NULL."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    if (!is.character(list_of_treatments)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_treatments` = \"",
          include_all_treatments,
          "\", but `list_of_treatments` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of treatments?"
      ))
    }

    table_data <- table_data %>%
      dplyr::filter(.data$Treatment %in% list_of_treatments)
  }
  # Category filter

  if (include_all_categories == "yes") {
    if (!is.null(list_of_categories)) {
      cli::cli_alert_info(
        "include_all_categories = \"yes\", but you included a list of categories to filter. All categories will be used."
      )
    }
  } else {
    if (is.null(list_of_categories)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_categories` = \"",
          include_all_categories,
          "\", but `list_of_categories` is NULL."
        ),
        "i" = "Did you forget to add a list of categories?"
      ))
    }

    if (!is.character(list_of_categories)) {
      cli::cli_abort(c(
        "x" = paste0(
          "`include_all_categories` = \"",
          include_all_categories,
          "\", but `list_of_categories` is not a character object or list of characters."
        ),
        "i" = "Did you forget to add a list of categories?"
      ))
    }

    table_data <- table_data %>%
      dplyr::filter(.data$Category %in% list_of_categories)
  }

  if (save_output_as_RDS == "yes") {
    RDS_path <- "Data/Output-Data-from-R/"

    if (!dir.exists(RDS_path)) {
      dir.create(RDS_path)
    }

    saveRDS(table_data, file = here::here(paste0(
      RDS_path,
      "interactive_summary_table_df.rds"
    )))
  }


  cell_table <- suppressWarnings(reactable::reactable(
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
  ))


  return(cell_table)
}
