#' Make baseline comparison plot
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

  treatment_names_and_colours <- treatment_colour_theme

  if (include_all_treatments == "yes") {
    treatment_info <- treatment_names_and_colours
    plot_data <- data %>%
      dplyr::filter(.data$treatment %in% treatment_names_and_colours$treatment) %>%
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

    treatment_info <- treatment_names_and_colours %>%
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
