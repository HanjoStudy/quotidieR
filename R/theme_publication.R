#' @title Theme to be used in journal publications
#' @description Theme for use in journals
#' @param base_size size of text in the graph
#' @param base_family font of text in graph
#' @examples
#'
#'library(ggplot2)
#'library(gridExtra)
#'library(reshape2)
#'
#'Scatter <-
#'  ggplot(mtcars, aes(mpg, disp, color = factor(carb))) + 
#'  geom_point(size = 3) + 
#'  labs(title = "Scatter Plot")
#'
#'grid.arrange(Scatter,
#'             (Scatter + scale_colour_Publication() + theme_Publication()), 
#'             nrow = 1)
#'
#'Bar <- ggplot(mtcars, aes(factor(carb), fill = factor(carb))) +
#'  geom_bar(alpha = 0.7) +
#'  labs(title = "Bar Plot")
#'
#'grid.arrange(Bar,
#'             (Bar + scale_fill_Publication() + theme_Publication()),
#'             nrow = 1)
#'
#'Bubble <- ggplot(mtcars, aes(mpg, disp, color = factor(carb), size = hp)) +
#'  geom_point(alpha = 0.7) +
#'  labs(title = "Bubble Plot") +
#'  scale_size_continuous(range = c(3, 10))
#'
#'grid.arrange(Bubble,
#'             (Bubble + scale_colour_Publication() + theme_Publication()),
#'             nrow = 1)
#'
#'
#'mtcars$Index <- 1:nrow(mtcars)
#'dat <-
#'  melt(mtcars,
#'       id.vars = c("Index"),
#'       measure.vars = c("drat", "wt"))
#'Line <- ggplot(dat, aes(Index, value, colour = variable)) +
#'  geom_line(size = 1.3) +
#'  labs(title = "Line Plot")
#'grid.arrange(Line,
#'             (Line + scale_colour_Publication() + theme_Publication()),
#'             nrow = 1)
#'
#' 
#' @return NA
#' @export

theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
  
}


