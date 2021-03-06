% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moveme.R
\name{moveme}
\alias{moveme}
\title{Move column position}
\usage{
moveme(df, movecommand)
}
\arguments{
\item{movecommand}{command that depicts how columns are moved: before, after, first, last}

\item{invec}{names of dataframe that you will be organising}
}
\value{
vector of column names
}
\description{
Uses verb notation to move columns in dataframe
}
\examples{

a <- b <- c <- d <- e <- f <- g <- 1:100
df <- data.frame(a,b,c,d,e,f,g)
df <- df \%>\% tbl_df

# Usage
df \%>\% moveme(., "g first")
df \%>\% moveme(., "g first; a last; e before c")

}
