#' @title Get method
#' @description Get methods of S3 class objects from anywhere
#' @param obj names of obj
#' @param sleep sleep for seconds before display result
#' @examples
#' xx <-  data.frame()
#' get_method(xx)
#' @return vector of column names
#' @export


get_method <- function(obj, sleep = 1){
  message("Object is off class: ", class(obj))
  Sys.sleep(sleep)
  showMethods(class=class(obj), where=search())
}