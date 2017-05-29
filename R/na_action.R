#' @title Counting/NAs of NA's per column or row vector
#' @description na_action Looks at your data.frame() or vector and counts the NAs. Option to remove is also built in
#' @param df object to be checked
#' @param margin Margin sets whether you count are counting columns, margin = 2, or rows ,margin =1 (Default = 2)
#' @param drop If this is TRUE, then all rows with NAs will be removed
#' @param cutoff Cutoff is a parameter where the exact NA cutoff per row must be when filtering the dataset (Default = 1)
#' @examples 
#' x <- data.frame(NR = c(1,2,3,5,NA,6,7,NA),Letters = c(rep(LETTERS)[1:7],NA), stringsAsFactors = F)
#' # Get the count per columns
#' na_action(x, 2)
#' 
#' # Get the count per row
#' na_action(x, 1)
#' 
#' # Returns NA count in the vector
#' na_action(c(1,2,3,NA,4,NA,5))
#' 
#' # Replace all NA with 0
#' 
#' na_action(df, replace = 0)
#' 
#' # Drop all rows with NA
#' na_action(x, drop =T)
#' 
#' # Specify cutoff for dropoff. 
#' # Specifying cutoff as 2 will only drop rows where there are more than >=2 NAs in the row
#' na_action(x, drop = T, cutoff = 2)
#' 
#' # Cutoffs can also be proportional
#' na_action(x, drop = T, cutoff = 0.6)
#'  
#' @return An count of NAs
#' @export

# Add parameters to do the following:
# 1) Should I remove the NA and create new dataset (@param drop)? If so, should there be a cutoff specify?
# 2) Add if statement for when cutoff does nothing

na_action <- function(df, margin = 2, replace, drop = F, cutoff = 1) {
  
  if(!missing(replace))
  {
    df[is.na(df)] <- replace
    return(df)
  }
  
  if(drop == T & missing(cutoff))
  {
    margin <- 1
    if(margin == 1)
    {
      x <- df[complete.cases(df),]
      return(x)
    }else{
      return("Error occured in drop")
    }
  }
  
  if(drop == T & !missing(cutoff))
  {
    margin <- 1
    if(margin == 1)
    {
      if(cutoff >= 1)
      {

        x <- apply(df, 1, function(z) sum(is.na(z)))
        cut <- which(x >= cutoff)
        if(length(cut) > 0)
        {
          x <- df[-cut, ]
          return(x)
        }
        if(length(cut) == 0)
        {
          return(df)
        }
      }else if(cutoff < 1)
      {
        x <- apply(df, 1, function(z) sum(is.na(z))/ncol(df))
        cut <- which(x >= cutoff)
        if(length(cut) > 0)
        {
          x <- df[-cut, ]
          return(x)
        } else if(length(cut) == 0)
        {
          return(df)
        }
      }else{
      cat("Cutoff value not specified correctly")
      }
    }
  }
  
  #Information on class assignment: http://stackoverflow.com/questions/13056392/print-if-not-assigned
  if(is.vector(df)){
    x<- sum(is.na(df))
    return(x)
  }
  
  if(is.matrix(df) | is.data.frame(df))
  {
    if(margin == 2){
      x <- sapply(df, function(y) sum(is.na(y)))
      return(x)
    } 
    if(margin == 1){
        x <- apply(df, 1, function(z) sum(is.na(z)))
        names(x) <- row.names(df)
        return(x)
    }
  }
    
}
