#' @title Tidy Catdesc
#' @description Uses FactoMineR::catdesc function to describe the categories of one factor by categorical variables and/or by quantitative variables
#' @param df data frame to analyse
#' @param cluster cluster column name
#' 
#' @examples 
#' 
#' library(FactoMineR)
#' data(iris)
#' # Principal Component Analysis:
#' res.pca <- PCA(iris[,1:4], graph=FALSE)
#' # Clustering, auto nb of clusters:
#' hc <- HCPC(res.pca, nb.clust=-1)
#' 
#' hc$data.clust %>% tidy_catdesc(., clust)
#' hc %>% tidy_catdesc()
#' @return list object
#' @export

tidy_catdesc <- function(df, cluster){
  options(stringsAsFactors = F)
  load_pkg(c("purrr", "dplyr", 'FactoMineR'))
  
  if(missing(cluster)){
    cluster_col <- NULL
  } else {
    cluster_col <- enquo(cluster) 
  }
  
  if(class(df)[1] %in% c("HCPC")){
    df <- df$data.clust %>% 
      data.frame %>% 
      select(clust, everything()) 
  } else {
    if(is.null(cluster_col)) stop("Please provide cluster column name for analysis")
    
    df <- df %>% 
      select(!!cluster_col, everything())
  }
  df[,1] <- factor(df %>% pull(1))
  df <- df %>% data.frame
  
  res_catdes <- catdes(df,  1)
  
  quali <- res_catdes['category'][[1]]
  quanti <- res_catdes['quanti'][[1]]
  
  if(!is.null(quali))
  {
    quali <- quali %>% 
      purrr::compact() %>% 
      Map(data.frame, cluster = names(.), .) %>% 
      map(~.x %>% data.frame %>% tibble::rownames_to_column("variable")) %>%  
      reduce(rbind) %>% 
      mutate(type_variable = "qualitative")
    
  } else { quali <- data.frame(Message = "No qualitative variables were used")}
  
  if(!is.null(quanti)){
    quanti <- quanti %>% 
      purrr::compact() %>% 
      Map(data.frame, cluster = names(.), .) %>% 
      map(~.x %>% data.frame %>% tibble::rownames_to_column("variable")) %>% 
      reduce(rbind) %>% 
      mutate(type_variable = "quantitative")
  } else { quanti <- data.frame(Message = "No quantitative variables were used")}
  
  list(quali, quanti)
}