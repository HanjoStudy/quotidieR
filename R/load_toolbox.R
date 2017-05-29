#' @title Easy loading all analytics packages
#' @description load_toolbox Loads a list of packages that are most often used in our analytics workspace: 
#' dplyr, readr, lubridate, tidyr, openxlsx, ggplot, purrr
#' @param install Will install all packages if not available to load. Default = TRUE
#' @examples load_toolbox()
#' @return Packages loaded into R for analytics
#' @export


load_toolbox <- function(install = T) {
  
  pkg <- c("purrr", "ggplot2", "dplyr", "tidyr", "readr", "lubridate", "openxlsx")
  cat("-------------------------------------------\n")
  cat("Loading the Analytics Toolbox libary\n")
  cat("-------------------------------------------\n")
  suppressPackageStartupMessages(load_pkg(pkg))
  cat(paste0("\n",pkg,"\n"))

}