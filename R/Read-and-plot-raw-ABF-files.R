#' Import raw .abf files as a dataframe
#'
#' `import_ABF_file()` is a wrapper around `abftools::abf2_load()` and
#' `abftools::MeltAbf()`. It converts the array from `abf2_load()` into a
#' dataframe, and it also converts time to minutes
#'
#' @param file_name Filepath to an .abf file (e.g. "Data/23711004.abf")
#'
#' @returns A dataframe
#'
#' @export
#'
#' @examples
#' import_ABF_file(load_ext_data("sample_abf.abf"))

import_ABF_file <-
  function(file_name) {
    abftools::abf2_load(here::here(file_name)) %>%
      abftools::MeltAbf() %>%
      dplyr::rename("current" = .data$chan1, "voltage" = .data$chan2) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::mutate(time_sec = .data$time / 10000) %>%
      invisible()
  }



#' Plot a representative spontaneous current trace
#'
#' `make_representative_sp_trace()` generates a plot of raw current amplitude
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
#' @param plot_font_family A character describing the font family used.
#' @param save_plot_pngs A character ("yes" or "no") defining if the plot should
#' be saved as a PNG through `ggplot::ggsave()`
#'
#' @returns A ggplot object. If save_plot_PNGs is defined as "yes" in the Global
#'   Environment, it will also generate a .png file in the folder
#'   `Figures/Spontaneous-currents/Representative-Traces` relative to the
#'   project directory.
#'
#'   `Figures/Spontaneous-currents/Representative-Traces`
#'
#' @export
#'
#' @examples
#' make_representative_sp_trace(
#'  file = sample_abf,
#'  plot_colour = "#6600cc",
#'  include_scale_bar = "yes",
#'  plot_episode = "epi1",
#'  scale_bar_x_length = 1,
#'  scale_bar_y_length = 10,
#'  plot_x_min = 1,
#'  plot_x_max = 3
#' )
#'

make_representative_sp_trace <-
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
           save_plot_pngs = "no",
           plot_font_family = NULL) {
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

    scale_bar_x_length_in_ms <- scale_bar_x_length*1000

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
          x = scale_bar_x_start - (plot_x_max - plot_x_min)/100,
          y = scale_bar_y_start + 0.5*scale_bar_y_length,
          label = paste0(scale_bar_y_length, " pA"),
          hjust = 1,
          vjust = 0.5,
          family = plot_font_family
        ) +
        ggplot2::annotate(
          "text",
          x = scale_bar_x_start + 0.5*scale_bar_x_length,
          y = scale_bar_y_start - 5,
          label = paste0(scale_bar_x_length_in_ms, " ms"),
          hjust = 0.5,
          family = plot_font_family
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
