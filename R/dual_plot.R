#' @title Plotting of 2 series with a secondary Y axis
#' @description dual_plot will take 2 series and plot the second series on the second Y-axis
#' @param x1 x-axis object
#' @param y1,y2 objects to be plotted
#' @param col1,col2 colors for the 2 lines
#' @param lwd line width
#' @param colgrid colour of gridlines; if NULL there are no gridlines
#' @param mar margins for plotting area see  (see ?par)
#' @param ylab1,ylab2 labels for the two y axes
#' @param nbreaks Number of breaks in horizontal axis
#' @param yleg1,yleg2 legend of the 2 objects
#' @param legx,legy position of the legend
#' @examples 
#' data("forex")
#' 
#' with(forex, dual_plot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, lwd = 1.2, colgrid = "grey90", 
#'     main = "NZ dollar exchange rate & trade-weighted index",
#'     ylab1 = "NZ dollars for one US dollar",
#'     ylab2 = "Index",
#'     yleg1 = "NZD / USD exchange rate (left axis)",
#'     yleg2 = "Trade-weighted index (right axis)",
#'     mar = c(5,6,3,6)))
#'     
#' mtext(side = 1, "Data from RBNZ", 
#'           adj = 1, cex = 0.8, line = 3)
#'           
#' @return Plot of 2 series on different Y-axis
#' @export
#'
#'  
dual_plot <- function(x1 = time(y1), y1, y2, x2 = x1, col1 = "#C54E6D", col2 = "#009380",
                     lwd = 1, colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = substitute(y1), ylab2 = substitute(y2), nxbreaks = 5,
                     yleg1 = paste(ylab1, "(LHS)"), yleg2 = paste(ylab2, "(RHS)"),
                     ylim1 = NULL, ylim2 = NULL, ylim.ref = 1,
                     xlab = "", main = NULL, legx = "topleft", legy = NULL, ...){
  
  # Original: Peter Ellis, 16 August 2016, GNU GPL-3
  # http://ellisp.github.io/blog/2016/08/18/dualaxes
  # Note that default colours were chosen by colorspace::rainbow_hcl(2, c = 80, l = 50)
  
  oldpar <- par(mar = mar)
  xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
  
  # unless ylim1 or ylim2 were, set, we set them to levels that make it equivalent
  # to a graphic drawn of indexed series
  if(is.null(ylim1) & is.null(ylim2)){
    if(min(c(y1, y2)) < 0){
      stop("Sorry, with negative values you need to specify ylim1 or ylim2")
    }
    
    if((length(y1) != length(y2)) & ylim.ref != 1){
      warning("y1 and y2 are different lengths.  Forcing ylim.ref to be 1.")
      ylim.ref <- 1
    }
    if(ylim.ref > min(length(y1), length(y2))){
      stop("ylim.ref must be a number shorter than the length of the shorter series.")
    }
    ind1 <- y1 / y1[ylim.ref]
    ind2 <- y2 / y2[ylim.ref]
    indlimits <- range(c(ind1, ind2))
    ylim1 = indlimits * y1[ylim.ref]
    ylim2 = indlimits * y2[ylim.ref]
  }
  
  
  # draw first series - with no axes.
  plot(x1, y1, type = "l", axes = FALSE, lwd = lwd,
       xlab = xlab, ylab = "", col = col1, main = main, 
       xlim = range(xbreaks), ylim = ylim1)
  
  # add in the gridlines if wanted:
  if(!is.null(colgrid)){
    grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
    abline(v = xbreaks, col = colgrid)
  }
  
  # add in the left hand vertical axis and its label
  axis(2, col = col1, col.axis= col1, las=1 )  ## las=1 makes horizontal labels
  mtext(paste0("\n", ylab1, "\n"), side = 2, col = col1, line = 1.5) 
  
  # Allow a second plot on the same graph
  par(new=TRUE)
  
  # Plot the second series:
  plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd,
       col = col2, xlim = range(xbreaks), ylim = ylim2)
  
  ## add second vertical axis (on right) and its label
  mtext(paste0("\n", ylab2, "\n"), side = 4, col = col2, line = 4.5) 
  axis(4,  col = col2, col.axis = col2, las=1)
  
  # Draw the horizontal time axis
  axis(1, at = xbreaks)
  
  # Add Legend
  legend(x = legx, y = legy, legend=c(yleg1, yleg2),
         text.col = c(col1, col2), lty = c(1, 1), col = c(col1, col2),
         bty = "n", ...)
  
  par(oldpar)
}