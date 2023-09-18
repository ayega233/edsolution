#' Shortest path Algorithm
#'
#' @description Return the shortest path from \code{init.node} to each nodes in a \code{graph}, using weight vector in graph data
#'          frame find shortest distance and return single vector as a result with distance to each nodes.
#' @usage dijkstra(graph, init.node )
#' @param graph  data frame with three variables \code{(v1,v2,w)} and variables should be edges of the graph with the weight of edge
#' @param init.node numeric scale exist in the graph
#'
#' @return shortest distance vector from \code{init.node} to each nodes in \code{graph}
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra27s_algorithm}
#'
#' @export dijkstra
#' @examples
#'  sample_graph <-  data.frame(
#'   v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#'
#'  dijkstra(sample_graph, 1)
#'
#'  dijkstra(sample_graph, 3)



dijkstra <-function(graph,init.node){
  graph_column_names<- c("v1", "v2", "w")
  supplied_column_names <- colnames(graph)
  stopifnot("column names are not maching expected values are v1,v2 and w"= identical(supplied_column_names,graph_column_names))

  unvisited_set <- unique(graph$v1)
  no_of_nodes <- length(unvisited_set)
  distance_matrix <- matrix(NA,no_of_nodes ,no_of_nodes)
  for (i in 1:length(graph$v1)) {
    distance_matrix[graph$v1[i],graph$v2[i]]<-graph$w[i]
  }
  tentative_distance <-rep(c(Inf),each=no_of_nodes)
  tentative_distance[init.node]<-0
  current_node = init.node;
  while (length(unvisited_set) > 1) {
    for (i in unvisited_set) {
      #cat("distance",current_node,",",i,",",distance_matrix[current_node,i],"\n")
      if(i!=current_node && !is.na(distance_matrix[current_node,i])){
        new_distance <-distance_matrix[current_node,i]+tentative_distance[[current_node]]
        #cat("distance",current_node,",",i,",",new_distance,",",tentative_distance[i],"\n")
        if(new_distance<tentative_distance[[i]]){
          tentative_distance[[i]] <- new_distance
        }
      }
    }
    unvisited_set <- unvisited_set[!unvisited_set==current_node]
    min_value <- min(tentative_distance[unvisited_set]);
    current_node <- intersect(unvisited_set,which(tentative_distance==min_value))[1]

    #cat("after ",unvisited_set,",",current_node,",",tentative_distance,"\n")
  }
  return(tentative_distance)
}
