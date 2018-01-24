#' @title Tidy wrapper for HCPC results
#' @description Uses HCPC class as inputs and tidy's the quali and quanti results of the most important variable in a cluster
#' @param hcpc_res object of class HCPC from \link[FactoMineR]{HCPC}
#' @examples
#' 
#'  library(FactoMineR)
#'  
#'  data(wine)
#'  mfa_res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'                 ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'                 num.group.sup=c(1,6))
#'  
#'  hcpc_res <- HCPC(res, nb.clust = 3)
#'  
#'  tidy_hcpc(hcpc_res)
#' 
#' 
#' @return list object containing most important quali and quanti variables in a clust
#' @export

tidy_hcpc <- function(hcpc_res){
  
  load_pkg(c("purrr", "dplyr"))
  
  hcpc_names <- names(hcpc_res$desc.var)
  
  hcpc_desc <- list()
  if("category" %in% hcpc_names){
    cat_res <- hcpc_res$desc.var$category
    
    null_res <- which(sapply(cat_res, is.null))
    
    cat_res[null_res] <- NULL
    
    cat_res <- Map(data.frame, type = as.list(rep("categorical", length(cat_res))), 
                   clust = as.list(names(cat_res)),  
                   variables = lapply(cat_res, rownames), 
                   cat_res) %>% 
      reduce(rbind) %>% tbl_df
    hcpc_desc[["categorical"]] <- cat_res
  }
  
  if("quanti" %in% hcpc_names){
    quant_res <- hcpc_res$desc.var$quanti
    
    null_res <- which(sapply(quant_res, is.null))
    
    quant_res[null_res] <- NULL
    
    quant_res <- Map(data.frame, type = as.list(rep("quantitative", length(quant_res))), 
                     clust = as.list(names(quant_res)),  
                     variables = lapply(quant_res, rownames), 
                     quant_res) %>% 
      reduce(rbind) %>% tbl_df
    hcpc_desc[["quantitative"]] <- quant_res
  }
  
  return(hcpc_desc)
}