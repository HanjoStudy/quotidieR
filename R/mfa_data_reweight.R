#' @title Reweigh dataset based on MFA results
#' @description uses the results from the multiple factor analysis to reweigh all data based on sample results
#' @param dataset the dataset that needs to be reweighted
#' @param mfa_res a MFA object that contains the dimension reduction results 
#' @param index if index is provided it will be binded to dataframe in the end
#' @param dim number of dimensions to keep
#' @param plot_scree plots the eigenvalues against the factors for interpretation
#' @examples 
#' 
#' load_toolbox()
#' 
#' data(sales)
#' 
#' dataset <- sales
#' dataset[,1] <- factor(dataset[,1])
#' 
#' # Samle data and run MFA on the sample
#' sample <- sample_n(v, 1000)
#'  
#' load_pkg("FactoMineR")
#' mfa_res <- parMFA(sample,
#'                    group=c(1, 4, 12), 
#'                    type = c('n','s','s'), 
#'                    ncp=12, 
#'                    name.group=c('factor', 'purchases','seasonality'),
#'                    graph=FALSE, cores = 3)
#'                    
#' # The dimension needs to be specified
#' mfa_data_reweight(dataset, mfa_res)
#' 
#' mfa_data_reweight(dataset, mfa_res, dim = 12)
#'  
#' @return MFA Object
#' @export

mfa_data_reweight<- function(dataset, mfa_res, index, dim, plot = T)
{
  
  if(missing(dim))
  {
    mar.default = c(5.1,4.1,4.1,2.1) + c(0, 0.3, 0, 0)
    par(mfrow = c(1,2), mar = mar.default)
    plot(mfa_res$eig$eigenvalue, 
         type = "b", 
         xlab = "Dimensions",
         ylab = "Eigenvalues", 
         main = "Scree plot", 
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
         col = "black",
         bg = "black",
         pch = 21,
         lwd = 2)
    
    plot(mfa_res$eig$`cumulative percentage of variance`, 
         type = "b", 
         xlab = "Dimensions",
         ylab = "Cumulative Variance", 
         main = "Cumulative percentage of variance", 
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, 
         col = "black",
         bg = "black",
         pch = 21,
         lwd = 2)
    
    return("Please choose number of dimensions from scree plot. Look for the elbow as a guide")
  }
  
  dataset <- as.data.frame(dataset)
  check_class <- which(sapply(dataset, class) %in% c("factor", "character"))
  for(i in check_class) 
  {
    dataset[ ,i] <- factor(dataset[ ,i], exclude = NULL)
    levels(dataset[ ,i]) <- paste0(levels(dataset[,i]))
  }
  
  
  dataset <- as.data.frame(dataset)
  factor_checks <- lapply(dataset,  levels)
  
  dup_fac <- which(duplicated(factor_checks) & !sapply(factor_checks, is.null))
  if(length(dup_fac) != 0)
  {
    for(i in 1:length(dup_fac)){
      levels(dataset[,dup_fac[i]]) <- paste(levels(dataset[,dup_fac[i]]), i, sep="_")
    }
  }
  
  Nr_groups <- length(mfa_res$separate.analyses)
  
  Count_in_group <- mfa_res$call$group
  Group_type <- mfa_res$call$type
  
  end_ind <- cumsum(Count_in_group)
  begin_ind <- c(1, end_ind[-length(end_ind)] +1)
  
  new_data <-list()
  for(i in 1:length(Count_in_group))
  {
    if(Group_type[i] == "c"){
    new_data[[i]] <- scale(dataset[ ,begin_ind[i]:end_ind[i]],
                           center = mfa_res$separate.analyses[[i]]$call$centre,
                           scale = mfa_res$separate.analyses[[i]]$call$ecart.type)
    
    } else if (Group_type[i] == "n"){
      
      disjoint_tbl <- tab.disjonctif(dataset[ ,begin_ind[i]:end_ind[i]])
      
      col_names <- colnames(colnames(disjoint_tbl))

      disjoint_tbl <- scale(disjoint_tbl,
                            center= apply(disjoint_tbl, 2, mean),
                            scale = apply(disjoint_tbl, 2, sd)*sqrt((nrow(disjoint_tbl)-1)/nrow(disjoint_tbl)))
  
      
      new_data[[i]] <- disjoint_tbl
      
    } else {
      return("Variable type has not been incorporated into this scaling function. Please talk to Vuli or Hanjo")
    }
  }
  
  new_data <- do.call(cbind, new_data)
  
  col_order <- rownames(mfa_res$global.pca$var$coord[,1:dim])
  new_data <- new_data[ ,col_order]
  
  new_data <- new_data %*% 
    diag(c(mfa_res$call$col.w)) %*%
    mfa_res$global.pca$var$coord[,1:dim] %*%
    diag(1/mfa_res$global.pca$svd$vs[1:dim])
  
  if(plot == T){
    par(mfrow = c(1,2))
    plot(mfa_res$eig$eigenvalue, 
         type = "b", 
         xlab = "Factors",
         ylab = "Eigenvalues", 
         main = "Scree plot")
    
    plot(mfa_res$eig$`cumulative percentage of variance`, 
         type = "b", 
         xlab = "Factors",
         ylab = "Cumulative Variance", 
         main = "Cumulative percentage of variance")
  }

  if(!missing(index)){
    return(data.frame(index, new_data, stringsAsFactors = F))
  } else
  {
    return(data.frame(new_data, stringsAsFactors = F))
  }

}
