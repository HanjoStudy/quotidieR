#' @title Plot Random Forest trees
#' @description Plots least and most node trees from Random Forest object
#' @param final_model object of class randomForest from \link[randomForest]{caret}
#' @param tree_num tree number you want to plot
#' @examples
#' 
#' library(caret)
#' 
#' 
#' set.seed(42)
#' index <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
#' train_data <- iris[index, ]
#' test_data  <- iris[-index, ]
#' 
#' fitControl <- trainControl(method = "repeatedcv", 
#'              number = 5, 
#'              repeats = 2,
#'              sampling = "smote",
#'              savePredictions = TRUE, 
#'              verboseIter = FALSE)
#' 
#' rfGrid <-  expand.grid(mtry = c(2: sqrt(ncol(train_data))))
#' rfGrid <-  expand.grid(mtry = c(2: 4))
#' 
#' # run model
#' set.seed(42)
#' model_rf <- train(Species ~ .,
#'                          data = train_data,
#'                          method = "rf",
#'                          preProcess = c("scale", "center"),
#'                          fitControl = fitControl,
#'                          tuneGrid = rfGrid)
#' 
#' plot(model_rf)
#' ggplot(model_rf)
#' plot(model_rf$finalModel)
#' 
#' plot_rf_tree(model_rf, "most")
#' plot_rf_tree(model_rf, "least")
#'
#' 
#' 
#' @return ggplot object of a random forest containing least or most nodes
#' @export
plot_rf_tree <- function(model_rf, spec = c("least", "most")[1]) {
  
  load_pkg(c("tibble", "ggraph", "igraph", "dplyr"))
  
  if(spec == "least"){
    tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))
  } else if(spec == "most"){
    tree_num <- which(model_rf$finalModel$forest$ndbigtree == max(model_rf$finalModel$forest$ndbigtree))
  }
  
  if(length(tree_num) > 1){
    message("Trees with same depth found, sampling 1")
    tree_num <- sample(tree_num, 1)
  }
  
  # get tree by index
  tree <- randomForest::getTree(model_rf$finalModel, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}