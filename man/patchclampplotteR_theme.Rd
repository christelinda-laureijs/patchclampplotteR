% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{patchclampplotteR_theme}
\alias{patchclampplotteR_theme}
\title{Add a customized ggplot2 theme}
\usage{
patchclampplotteR_theme(font_family = NULL)
}
\arguments{
\item{font_family}{A character value describing the font family used for all text in the ggplot (axis text, axis titles, plot title, etc.). This must be a loaded font in R, which can be facilitated by loading packages like \code{extrafont} at the start of your document.}
}
\value{
A ggplot theme
}
\description{
This is an attractive ggplot2 theme readily available to use with any plots when \code{patchclampplotteR} is loaded. It is built on top of \code{theme_classic()} and features bold and easy to read axis titles, well-spaced margins, and a clean layout.
}
\examples{

ggplot2::ggplot(cars, ggplot2::aes(x = speed, y = dist)) +
  ggplot2::geom_point() +
  ggplot2::labs(x = "Speed (mph)", y = "Stopping Distance (ft)") +
  patchclampplotteR_theme()

}
\seealso{
\code{\link[=patchclampplotteR_facet_theme]{patchclampplotteR_facet_theme()}} to use with \code{\link[=make_facet_plot]{make_facet_plot()}}.
}
