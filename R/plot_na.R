#' @title Plot NA
#' @description Easy function that uses \code{na_count()} to plot the number of NAs within your data
#' @param data data.frame or tibble
#' @param type display the NA rby percentage or absolute
#' @examples  
#' 
#' data <- airquality
#' 
#' plot_na(data)
#' 
#' 
#' @return plot
#' @export

plot_na <- function(data, type = c("relative", "absolute")[1])
{
  
  load_pkg(c("ggplot2", "dplyr"))
  
  if(!is.data.frame(data)) message("Not a data frame object")
  
  if(type == "relative")
  {
  na_action(data) %>%
    data.frame(val = .) %>%
    mutate(columns = row.names(.), na_count = round(val / nrow(data), 2)) %>%
    ggplot(., aes(x = reorder(columns,-val), y = na_count)) +
    geom_bar(stat = "identity", fill = "#4982c3")  +
    labs(x = "Variables", y = ifelse(type == "relative", "NA count (%)", "NA count"), title = "Missing data") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.grid.major = element_line(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5)
    )
  } else {
    na_action(data) %>%
      data.frame(val = .) %>%
      mutate(columns = row.names(.), na_count = val) %>%
      ggplot(., aes(x = reorder(columns,-val), y = na_count)) +
      geom_bar(stat = "identity", fill = "#4982c3")  +
      labs(x = "Variables", y = ifelse(type == "relative", "NA count (%)", "NA count"), title = "Missing data") +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)
      )
  }
}