% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_na.R
\name{plot_na}
\alias{plot_na}
\title{Plot NA}
\usage{
plot_na(data, type = c("relative", "absolute")[1])
}
\arguments{
\item{data}{data.frame or tibble}

\item{type}{display the NA rby percentage or absolute}
}
\value{
plot
}
\description{
Easy function that uses \code{na_count()} to plot the number of NAs within your data
}
\examples{
 

data <- airquality

plot_na(data)


}
