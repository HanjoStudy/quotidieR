#' @title Parallel implementation of Kmeans
#' @description Parallel implementation of Kmeans to test for global minimum
#' @param dataset the dataset upon which the kmeans algoritm will be run
#' @param seeds the number of seed you would like to test for a global min
#' @param centers vector of k centers to try
#' @param cores number of cores to run in parallel, default is 10
#' @examples 
#' 
#' data("sales")
#' 
#' # remove NAs
#' seeds <- 50
#' centers <- c(5:15)
#' 
#' best_fit <- parKmeans(sales, seeds = seeds,  centers = centers)
#' best_fit
#' 
#' # plot runs
#' load_pkg("ggplot2")
#' 
#' best_fit %>% select(clusters, withinss, seed) %>% mutate(seed = factor(seed)) %>%
#'   ggplot(aes(x = clusters, y = withinss, color = seed)) +
#'   geom_line(show.legend=FALSE) +
#'   geom_smooth(se = F, color = "black", size = 1, span = 0.6)  + 
#'   ylab(label="WithinSS") + 
#'   xlab("Clusters") + 
#'   theme_bw() +
#'   scale_colour_manual(values=c(rep("#8599bc", seeds)))
#' 
#' @return Data frame with each of the runs' metrics and seed number
#' @export

parKmeans <- function(dataset, seeds, centers, cores = 10, 
                      algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")[1]){
  
  load_pkg(c("doParallel", "dplyr"))
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  
  seeds <- seq(1, seeds,1)
  NAs <- na_action(dataset)[1]
  
  dataset <- na_action(dataset, drop = T)
  
  best_fit <- foreach(i = 1:length(seeds)) %dopar% {
    set.seed(i)
    runs <- list()
    count <- 1
    
    for (j in centers){
      ans.Kmeans <- kmeans(dataset, centers = j, iter.max = 20, algorithm = algorithm)
      withinss <- sum(ans.Kmeans$withinss)
      fit <- ans.Kmeans$betweenss/withinss
      
      r <-data.frame(seed = i, withinss = withinss, fit = fit, clusters = j)
      
      runs[[count]] <- r
      count <- count + 1
    }
    do.call(rbind, runs)
  }
  best_fit <- do.call(rbind, best_fit)
  
  best_fit %>% ungroup %>% arrange(desc(withinss), desc(fit)) 
  
  rownames(best_fit) <- NULL
  
  return(best_fit)
}
  