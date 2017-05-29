#' @title Plot dimension description visually
#' @description Function takes MFA object and plots either the quali, quanti or combined metrics of the dimension description
#' @param mfa MFA object from FactoMineR
#' @param dim number of dimensions to plot
#' @param type type of plot to return
#' @param grid grid to use for plotting (default is c(2,3))
#' @param specific whether you are plotting specific dimensions and not the range of dimensions
#' @examples  
#' load_pkg("FactoMineR")
#'
#' data(wine)
#'
#' mfa <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'           ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'           num.group.sup=c(1,6), graph = F)
#'
#' splitplot(mfa, dim = 5, type = "quanti")
#' 
#' # Wine example does not contain any qualitative examples
#' #splitplot(mfa, dim = 5, type = "quali")
#'
#' splitplot(mfa, dim = 5, type = "combined")
#'
#' splitplot(mfa, dim = c(1:2), type = "combined", grid = c(1,2), specific = T)
#' 
#' @return Plot containing correlations and v.tests
#' @export
#'

splitplot <- function(mfa, dim = 6, type = c("combined","quali","quanti")[3], grid = c(2,3), specific = F){
  
  if(!(type %in% c("combined","quali","quanti"))) return("ERROR: Misspecified type")
  if(!"MFA" %in% class(mfa)) return("ERROR: Not a MFA object")
  
  if(specific == F){
    dim <- seq(1, dim, 1)
  }
  
  if(mfa$call$ncp < max(dim))
  {
    cat("Specified too many dimension, defauling to max\n")
    Sys.sleep(1)
    dim <- mfa$call$ncp
    dim <- seq(1, dim, 1)
  }
  
  par(mfrow = grid)
  palette(rev(colorspace::rainbow_hcl(4, l = 70, c = 50)))
  colors_x <- colorspace::rainbow_hcl(length(dim), l = 70, c = 50)
  
  mar.default <- c(5,5.5,4,2) + 0.2
  par(mar = mar.default + c(0, 5, -1, 0))
  
  
  if(type == "combined")
  {
    # Quati
    y <- mfa$quanti.var$cor
    
    # Quali
    z <- mfa$quali.var$v.test
    
    if(nrow(y) ==0 || nrow(z) == 0) return("ERROR: Cannot perform combined analysis as data does not contain both quantitative and qualitative variables")
    
    col_count <- 1
    for(i in dim){
      options(warn = -1)
      x <- y[,i]
      
      x_norm <- x[x >= 0]
      
      # normalise_between <- c(0, 1)
      # normalized_quanti_pos = (normalise_between[2] - normalise_between[1])*((x_norm-min(x_norm))/(max(x_norm)-min(x_norm))) + (normalise_between[1])
      normalized_quanti_pos <- x_norm/max(abs(x_norm))
      
      
      x_norm <- x[x < 0]
      
      # normalise_between <- c(-1, 0)
      # normalized_quanti_neg = (normalise_between[2] - normalise_between[1])*((x_norm-min(x_norm))/(max(x_norm)-min(x_norm))) + (normalise_between[1])
      normalized_quanti_neg <- x_norm/max(abs(x_norm))
      
      
      normalised_quanti <- c(normalized_quanti_neg, normalized_quanti_pos)
      
      x <- z[,i]
      x_norm <- x[x >= 0]
      
      # normalise_between <- c(0, 1)
      # normalized_quali_pos = (normalise_between[2] - normalise_between[1])*((x_norm-min(x_norm))/(max(x_norm)-min(x_norm))) + (normalise_between[1])
      normalized_quali_pos <- x_norm/max(abs(x_norm))
      
      x_norm <- x[x < 0]
      
      # normalise_between <- c(-1, 0)
      # normalized_quali_neg = (normalise_between[2] - normalise_between[1])*((x_norm-min(x_norm))/(max(x_norm)-min(x_norm))) + (normalise_between[1])
      
      normalized_quali_neg <- x_norm/max(abs(x_norm))
      
      normalised_quali <- c(normalized_quali_pos, normalized_quali_neg)
      
      normalized <- c(normalised_quanti, normalised_quali)
      
      options(warn = 0)
      
      plot_col <- as.factor(names(sort(normalized)) %in% names(normalised_quali))
      
      barplot(sort(normalized), 
              main=paste0("Splitplot: dim",i), 
              horiz=TRUE, 
              las = 2, 
              col = plot_col,
              xpd = F,
              names.arg = names(sort(normalized)),
              xlab = "normalised metric", 
              cex.lab=1.5, cex.main=1.5)
      legend("topleft", col = plot_col, lty = 1, legend = c("Quanti", "Quali"), cex = 0.5)
      col_count <- col_count + 1
    }
  }
  
  if(type == "quali" || type == "quanti")
  {
    
    metric  <- ifelse(type == "quali", 9, 8)
    
    x <- mfa[[metric]][[4]]
    
    xlab  <- ifelse(type == "quali", "v.test", "corr")
    
    col_count <- 1
    for(i in dim)
    {
      barplot(sort(x[,i]), main=paste0("Splitplot: dim",i) , horiz=TRUE,
              names.arg=names(sort(x[,i])), las = 2,  xpd = F, col = colors_x[i],
              xlab = xlab,
              cex.lab=1.5, cex.main=1.5)
      col_count <- col_count + 1
    }
  }
}