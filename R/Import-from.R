# Import from

#' @importFrom magrittr %>%
magrittr::`%>%`


#' @importFrom rlang .data
rlang::.data

#' @importFrom ggplot2 %+replace%
#'
# Required to use dataui package which is suggested by reactablefmtr
# https://r-pkgs.org/dependencies-in-practice.html
# #how-to-not-use-a-package-in-imports
dataui_function <- function() {
  dataui::dui_sparkline()
}

pvalString <- function(p,
                       format = c("default", "exact", "scientific"),
                       digits = 3,
                       ...) {
  format <- match.arg(format, c("default", "exact", "scientific"))
  if (any(p < 0, na.rm = TRUE) | any(p > 1, na.rm = TRUE)) {
    notProb <- which(p < 0 | p > 1)
    stop(paste0(
      "Element(s) ",
      paste(notProb, collapse = ", "),
      " are not valid probabilities"
    ))
  }
  if (format == "default") {
    ps <- ifelse(p > 0.99, "> 0.99", ifelse(
      p > 0.1,
      format(round(p, 2), digits = 2),
      ifelse(p > 0.001, format(round(p, 3), digits = 3), "< 0.001")
    ))
  } else if (format == "exact") {
    ps <- ifelse(
      p < 1 * (10^-digits),
      format(p, scientific = TRUE, digits = digits),
      format(round(p, digits), digits = digits)
    )
  } else if (format == "scientific") {
    ps <- format(p, scientific = TRUE, digits = digits)
  }
  return(ps)
}
