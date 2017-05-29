#' @title Easy coerse operator for changing character columns to vector for analysis
#' @description Takes a data frame as input, checks class of each column and then converts character to factor. The function can also do the reverse - take factor to character
#' @param df data frame that needs to be coersed factors
#' @param rev data frame that needs to be coersed to characters
#' @examples 
#'  df <- iris
#'  df <- factor_conv(iris, rev = T)
#'  factor_conv(df)
#' @return Coersed data.frame
#' @export

factor_conv <- function(df, rev = F, to.num = F){
  load_pkg("dplyr")
  if(rev == F){
    col_num <- which(sapply(df, class) == "character")
    if(length(col_num) == 0) return("No character columns found!!")
  } else {
    col_num <- which(sapply(df, class) == "factor")
    if(length(col_num) == 0) return("No character columns found!!")
  }
  
  cat("The following columns were identified and will be converted:\n")
  for(i in 1:length(col_num)) cat(paste0(i, ") ", names(col_num[i]), "\n"))
  
  col_num <- col_num %>% as.numeric
  
  if (rev == F){
    for(i in col_num) df[,i] <- as.factor(df[,i])
  } else {
    for(i in col_num) df[,i] <- as.character(df[,i])
    if (to.num){
      for(i in col_num) df[,i] <- as.numeric(df[,i])
    }
  }
  
  return(df)
}