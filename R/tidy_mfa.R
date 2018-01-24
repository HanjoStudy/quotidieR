#' @title Tidy MFA individual results
#' @description Takes an MFA objects and tidy the data into a much easier working format for evaluation
#' @param mfa_res object of class MFA
#' @param type which data class to tidy (default all)
#' @examples 
#' library(FactoMineR)
#' 
#' # Quanti variables only
#' data(wine)
#' mfa_res <- MFA(wine[,c(1, 2)], group=c(1,1), type=c(rep("n",2)),
#'                ncp=5, name.group=c("olf","vis"))
#' 
#' tidy_mfa(mfa_res)
#' 
#' # Quali variables only
#' mfa_res <- MFA(iris[,-5], group=c(2,2), type=c(rep("s",2)),
#'                ncp=5, name.group=c("sepal", "petal"))
#' 
#' tidy_mfa(mfa_res)
#' 
#' # Quanti & quali variables
#' data(wine)
#' mfa_res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'                ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'                num.group.sup=c(1,6))
#' 
#' tidy_mfa(mfa_res)
#' 
#' data (poison)
#' mfa_res <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2)
#' 
#' tidy_mfa(mfa_res)
#' @return Tidy data frame for MFA results
#' @export

tidy_mfa <- function(mfa_res, type = c("all", "quanti", "quali", "category")[1]){
  
  load_pkg(c('purrr','tibble','dplyr', 'tidyr'))
  
  if(class(mfa_res)[1] != "MFA") stop("Not a MFA object")
  
  mfa_construct <- unique(mfa_res$call$type)
  
  mfa_res <- dimdesc(mfa_res)
  
  dim_len <- length(mfa_res)
  
  if(length(mfa_construct) > 1){
    mfa_types <- 
      list(lapply(mfa_res, function(x) x[['quanti']]),
           lapply(mfa_res, function(x) x[['quali']]),
           lapply(mfa_res, function(x) x[['category']]))
    
    mfa_types <- 
      mfa_types %>% map(~.x %>% map(data.frame) %>% 
                          map(tibble::rownames_to_column) %>% 
                          do.call(rbind,.) %>% 
                          tibble::rownames_to_column(., var = "dim")) %>% 
      Map(cbind, type = c("quanti", "quali", "category") %>% as.list(), .) %>% 
      map(~.x %>% mutate(dim = gsub("^(Dim.[0-9]).*", "\\1", dim))) %>% 
      map(as_data_frame)
  } else if(mfa_construct == "c")
  {
    mfa_types <- 
      list(lapply(mfa_res, function(x) x[['quanti']]))
    
    mfa_types <- 
      mfa_types %>% map(~.x %>% map(data.frame) %>% 
                          map(tibble::rownames_to_column) %>% 
                          map(as_data_frame) %>% 
                          do.call(rbind,.) %>% 
                          tibble::rownames_to_column(., var = "dim")) %>% 
      Map(cbind, type = c("quanti") %>% as.list(), .) %>% 
      map(~.x %>% mutate(dim = gsub("^(Dim.[0-9]).*", "\\1", dim))) %>% 
      map(as_data_frame)
  } else if(mfa_construct == "n")
  {
    mfa_types <- 
      list(lapply(mfa_res, function(x) x[['quali']]),
           lapply(mfa_res, function(x) x[['category']]))
    
    mfa_types <- 
      mfa_types %>% map(~.x %>% map(data.frame) %>% 
                          map(tibble::rownames_to_column) %>% 
                          map(as_data_frame) %>% 
                          do.call(rbind,.) %>% 
                          tibble::rownames_to_column(., var = "dim")) %>% 
      Map(cbind, type = c("quali", "category") %>% as.list(), .) %>% 
      map(~.x %>% mutate(dim = gsub("^(Dim.[0-9]).*", "\\1", dim))) %>% 
      map(as_data_frame)
  }
  
  names(mfa_types) <- mfa_types %>%
    map(~.x %>% select(type) %>% distinct %>% unlist %>% as.character) %>% 
    reduce(c)
  
  return(mfa_types)
}
