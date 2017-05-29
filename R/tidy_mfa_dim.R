#' @title Tidy MFA global PCA dim ("coord", "cor", "cos2", "contrib") results
#' @description Takes an MFA objects and tidy the data into a much easier working format for evaluation
#' @param mfa_res object of class MFA
#' @param n number of dimension to tidy
#' @param type metric to tidy
#' @param top_bottom_n decide on top n variables in dimension
#' @examples 
#'  library(FactoMineR)
#'
#'  data(wine)
#'  res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'             ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'             num.group.sup=c(1,6))
#'  
#'  tidy_mfa_dim(res)
#'  
#'  data (poison)
#'  MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'      name.group=c("desc","desc2","symptom","eat"),
#'      num.group.sup=1:2)
#'  
#'  tidy_mfa_dim(res)
#' 
#' @return Tidy data frame for MFA global PCA results
#' @export

tidy_mfa_dim <- function(mfa_res, n = 5, type = c("coord", "cor", "cos2", "contrib")[4], top_bottom_n = 5){
  
  load_pkg(c('tibble','dplyr', 'tidyr'))
  
  if(class(mfa_res)[1] != "MFA") stop("Not a MFA object")
  if(!(type %in% c("coord", "cor", "cos2", "contrib"))) stop("Type not correctly specified")
     
  res <- 
    mfa_res$global.pca$var[[type]][,1:n] %>% 
    data.frame %>% 
    tibble::rownames_to_column(var = "variable") %>% 
    gather(., dim, type, 2:(n+1), -variable) %>% 
    arrange(dim, type) %>%
    group_by(dim) %>% 
    filter(row_number() %in% 1:top_bottom_n | row_number() %in% c(n():(n() - top_bottom_n + 1))) %>% 
    ungroup 
  
  names(res)[3] <- type
  
  return(res)
}