#' Gives a proportional count of variables supplied
#'
#' @param df a \code{tbl} of data
#' @param ... variables to group by
#' 
#' @examples
#' dplyr::starwars %>% prop_count(homeworld, species)
#' @export

prop_count <- function(df, ...){
  vars <- quos(...)
  
  df %>% 
    count(!!!vars, sort = T) %>% 
    mutate(prop_count = prop.table(n)) %>% 
    mutate(cumsum_count = cumsum(prop_count))
    
}
