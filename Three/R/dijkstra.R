#' Dijkstra's Algorithm for Shortest Paths
#'
#' This function implements Dijkstra's algorithm to find the shortest paths from a source node to all other nodes in a weighted graph.
#'
#' @param graph A data.frame representing the adjacency matrix or edge list of the graph. The graph should contain edge weights.
#' @param init_node The node from which to calculate the shortest paths. It should be a valid node in the graph.
#'
#' @return A list where each element represents the shortest distance from the start node to the corresponding node in the graph. The list may also include the actual paths.
#' 
#' @details Dijkstra's algorithm is a greedy algorithm that calculates the shortest path between nodes in a graph, 
#' which may represent, for example, road networks. The algorithm starts from the source node and explores the nearest neighbors with the smallest weights, updating the shortest path as it goes.
#'
#' @references \href(https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm){Dijkstra's Algorithm - Wikipedia}
#' 
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'     v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'     w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' )
#' dijkstra(graph, init_node)
#' 
#' @export
dijkstra <- function(graph, init_node) {
  # Dijkstra's algorithm implementation
}


dijkstra <- function(graph, init_node){
  ##create graph
  node <- unique(graph[[1]])
  node_num <- length(node)
  graph_matrix <- matrix(c(Inf), nrow = node_num, ncol = node_num)
  for (node_index in seq_along(node)) {
    graph_matrix[node_index,node_index] <- 0
  }
  for(edge_index in seq_along(graph[[3]])) {
    graph_matrix[graph[[1]][edge_index],graph[[2]][edge_index]] <- graph[[3]][edge_index]
  }
  
  
  ## new queue
  to_be_visited <- numeric(length(node))
  start <- 1
  end <- 1
  
  ##initialize
  current_dis <- graph_matrix[, init_node]
  shortest_dis <- graph_matrix[, init_node]
  
  
  ## push element
  to_be_visited[end] <- init_node
  end <- end + 1
  
  ##Not Empty
  while (start != end) {
    ## pop element
    current_node <- to_be_visited[start]
    start <- start + 1
    
    ##the distance from current node to next node
    for (next_node_index in seq_along(node)){
      current_dis[next_node_index] <- graph_matrix[current_node, next_node_index] + shortest_dis[current_node]
    }
    ##add new node
    for (current_dis_node in seq_along(current_dis)) {
      ##Not equal to 99999 to join the queue and nodes are not added repeatedly
      if(current_dis[current_dis_node] != Inf && !(current_dis_node %in% to_be_visited)){
        to_be_visited[end] <- current_dis_node
        end <- end + 1
      }
    }
    ##Compare and update the shortest distance
    for (index in seq_along(current_dis)) {
      if(current_dis[index] < shortest_dis[index]){
        shortest_dis[index] <- current_dis[index] 
      }
    }
  }
  
  return(shortest_dis)
}