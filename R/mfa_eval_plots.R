#' @title Wrapper function for factorextra plotting functions
#' @description Leverages factoextras plotting functions to plot MFA results. Mostly the global.pca object
#' @param mfa_res an MFA object
#' @param indi also plotting the individual results - avoid if data is large!
#' @examples 
#' library(FactoMineR)
#'
#'  data(wine)
#'  res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'             ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'             num.group.sup=c(1,6))
#'  
#'  mfa_eval_plots(res, indi = T)
#' @return Plots of MFA evaluations
#' @export

mfa_eval_plots <- function(mfa_res, indi = F){
  
  load_pkg(c("dplyr","factoextra", "gridExtra","ggrepel"))
  
  message("Please be patient, you are trying to plot a lot of plots ~30s\n")
  
  plot1 <- fviz_screeplot(mfa_res, ncp=10)
  
  plot2 <- fviz_pca_var(mfa_res$global.pca)
  
  plot3 <- fviz_pca_var(mfa_res$global.pca, col.var="cos2") +
    scale_color_gradient2(low = "white", mid = "blue", 
                          high = "red", midpoint = 0.5) + theme_minimal()
  
  p1 <- fviz_contrib(mfa_res$global.pca, choice = "var", axes = 1, top = 5)
  p2 <- fviz_contrib(mfa_res$global.pca, choice = "var", axes = 2, top = 5)
  p3 <- fviz_contrib(mfa_res$global.pca, choice = "var", axes = 3, top = 5)
  p4 <- fviz_contrib(mfa_res$global.pca, choice = "var", axes = 4, top = 5)
  
  plot4 <- arrangeGrob(p1, p2, p3, p4, ncol = 2, nrow = 2)
  
  plot5 <- fviz_contrib(mfa_res$global.pca, choice = "var", axes = 1:5, top = 20)
  
  obs <- length(mfa_res$call$row.w)
  
  if(obs > 100){
    message("-----------------------------------\n You are trying to plot individual analysis plots which contains more than 100 observations.\n Are your sure you want to do this (Y/N)?\n-----------------------------------\n")
    dec <- readline(">>>> ")
    
    dec <- ifelse(dec %in% c("Y", "y"), 1, 0)
  }
  
  if(indi == T & dec == 1) 
  {
    message("If your individual plots only have numbers as labels, add the labels\n as rownames to your dataset before conducting the MFA")
    plot6 <- fviz_pca_ind(mfa_res$global.pca, repel = T)
  
    p1 <- fviz_pca_ind(mfa_res$global.pca, col.ind = "cos2") +
      scale_color_gradient2(
        low = "white",
        mid = "blue",
        high = "red",
        midpoint = 0.50
      ) + theme_minimal()
    
    p2 <- fviz_pca_ind(mfa_res$global.pca, col.ind = "contrib") +
      scale_color_gradient2(
        low = "white",
        mid = "blue",
        high = "red",
        midpoint = 0.50
      ) + theme_minimal()
    
    plot7 <- arrangeGrob(p1, p2, ncol = 2)
    
    p1 <- fviz_contrib(mfa_res$global.pca, choice = "ind", axes = 1, top = 10)
    p2 <- fviz_contrib(mfa_res$global.pca, choice = "ind", axes = 2, top = 10)
    p3 <- fviz_contrib(mfa_res$global.pca, choice = "ind", axes = 3, top = 10)
    p4 <- fviz_contrib(mfa_res$global.pca, choice = "ind", axes = 4, top = 10)
    
    plot8 <- arrangeGrob(p1, p2, p3, p4, ncol = 2, nrow = 2)
    
    plot9 <- fviz_pca_biplot(mfa_res$global.pca, axes = c(1, 2), top = 10, repel = T)
    
    plots_ind <- list(plot6, plot7, plot8, plot9)
  }
  
  options(warn = -1)
  plots_all <- list(plot1, plot2, plot3, plot4, plot5)
  sink(file = tempfile())
  for(i in 1:5){
    if(i == 4){
      grid.arrange(plots_all[[i]])
    }else{
      print(plots_all[[i]])
    }
    dev.new()
  }
  
  Sys.sleep(1)
  
  if(indi == T){
    for(j in 1:4){
      if(j %in% c(2,3)){
        grid.arrange(plots_ind[[j]])
      }else{
        print(plots_ind[[j]])
      }
      dev.new()
    }
  }
  sink()
  options(warn = 0)
  
}