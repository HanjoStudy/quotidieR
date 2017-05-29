#' @title Operator that produces negative match vector
#' @description Negative match function
#' @param x,y variables to match on
#' @return negative match vector
#' @export
#' @rdname neg_match
#' 

`%!in%` <- function(x,y) !('%in%'(x, y))