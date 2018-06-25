#' @title Move column position
#' @description Uses verb notation to move columns in dataframe
#' @param invec names of dataframe that you will be organising
#' @param movecommand command that depicts how columns are moved: before, after, first, last
#' @examples
#'
#' a <- b <- c <- d <- e <- f <- g <- 1:100
#' df <- data.frame(a,b,c,d,e,f,g)
#' df <- df %>% tbl_df
#' 
#' # Usage
#' df %>% moveme(., "g first")
#' df %>% moveme(., "g first; a last; e before c")
#' 
#' @return vector of column names
#' @export

moveme <- function (df, movecommand) {
  library(dplyr)
  invec <- names(df)
  
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  
  df %>% select(match(myVec, names(.)))
}