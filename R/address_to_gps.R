#' @title Google api call for turning address to gps coordinates
#' @description Uses google's api from \code{ggmap} to convert address to gps
#' @param address string with address to convert
#' @examples
#'  address <- "31 Pickwick street Salt River Western Cape South Africa"
#'  address_to_gps(address)
#' @return data.frame with GPS coordinates
#' @export

address_to_gps <- function(address){   
  load_pkg("ggmap")
  
  #use the gecode function to query google servers
  geo_reply <- geocode(address, 
                       output = 'all', 
                       messaging = TRUE, 
                       override_limit = TRUE)
  
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat = NA, 
                       long = NA, 
                       accuracy = NA, 
                       formatted_address = NA, 
                       address_type = NA, status = NA)
  
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    
    message("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    message(as.character(time))
    Sys.sleep(60 * 60)
    geo_reply <- geocode(address, 
                         output='all', 
                         messaging = TRUE, 
                         override_limit = TRUE)
    
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}