#' @title Time Series Calendar Heatmap plot
#' @description Utilizes ggplot's tile geom to construct heatmaps for calendar dates
#' @param data data containing the values you want to plot
#' @param date name of date column
#' @param n name of column with the numerical amount to grade plot
#' @param low color on low gradient
#' @param high color of high gradient
#' @examples
#' 
#' library(quantmod)
#' library(dplyr)
#' library(tbl2xts)
#' 
#' getSymbols("JSE:WHL", src = "google")
#' 
#' stock <- `JSE:WHL`
#' 
#' stock <- xts_tbl(stock)
#'   
#' stock %>% filter(date > "2016-01-01") %>% 
#'   calendar_plot(date, JSE.WHL.Volume)
#' 
#' stock %>% filter(date > "2016-01-01") %>% 
#'   calendar_plot(date, JSE.WHL.Volume, low = "#a7b5c3")
#' 
#' 
#' @return calendar heatmap plot
#' @export

calendar_plot <- function(data, date, n, low = "#a7b5c3", high = "red"){
  
  load_pkg(c("dplyr", "lubridate", "ggplot2"))
  
  enquo_date <- enquo(date)
  enquo_n <- quo_name(enquo(n))
  
  cal_data <- data %>% 
    mutate(year_date = year(!!enquo_date),
           month_date = factor(month(!!enquo_date), levels = as.character(1:12), 
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered=TRUE),
           wkday = factor(wday(!!enquo_date), levels = rev(1:7), 
                          labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")), ordered=TRUE), 
           weekmonths = ceiling(as.numeric(format(as.Date(!!enquo_date), "%d")) / 7))
  
  ggplot(cal_data, aes_string("weekmonths", "wkday", fill = enquo_n)) + 
    geom_tile(colour = "white") + 
    facet_grid(year_date ~ month_date) + 
    scale_fill_gradient(low = low, high = high) +
    labs(title = "Time-Series Calendar Heatmap") +  
    xlab("Week of Month") + ylab("")
}
