% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{patchclampplotteR_theme}
\alias{patchclampplotteR_theme}
\title{Add a customized ggplot2 theme}
\usage{
patchclampplotteR_theme()
}
\value{
A ggplot theme
}
\description{
Add a customized ggplot2 theme
}
\examples{

ggplot2::ggplot(cars, ggplot2::aes(x = speed, y = dist)) +
  ggplot2::geom_point() +
  ggplot2::labs(x = "Speed (mph)", y = "Stopping Distance (ft)") +
  patchclampplotteR_theme()

}
