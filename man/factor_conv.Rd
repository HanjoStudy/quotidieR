% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/char_to_factor.R
\name{factor_conv}
\alias{factor_conv}
\title{Easy coerse operator for changing character columns to vector for analysis}
\usage{
factor_conv(df, rev = F, to.num = F)
}
\arguments{
\item{df}{data frame that needs to be coersed factors}

\item{rev}{data frame that needs to be coersed to characters}
}
\value{
Coersed data.frame
}
\description{
Takes a data frame as input, checks class of each column and then converts character to factor. The function can also do the reverse - take factor to character
}
\examples{
 df <- iris
 df <- factor_conv(iris, rev = T)
 factor_conv(df)
}
