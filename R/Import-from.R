# Import from

#' @importFrom magrittr %>%
magrittr::`%>%`


#' @importFrom rlang .data
rlang::.data


# Required to use dataui package which is suggested by reactablefmtr
# https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
dataui_function <- function() {
  dataui::dui_sparkline()

}
