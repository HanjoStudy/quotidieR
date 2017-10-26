#' @title wrapper for \code{Sys.sleep}
#' @description A wrapper that is useful when you need to sleep in a loop or scrape
#' @param periods controls the min and max for \code{runif} that controls the time for sleep
#' @examples nytnyt(periods = c(1,2))
#' @return \code{Sys.sleep}
#' @export

nytnyt <- function (periods = c(2,3)){
  tictoc <- runif(1, periods[1], periods[2])
  cat("Sleeping for ", round(tictoc, 2), "seconds\n")
  Sys.sleep(tictoc)
}