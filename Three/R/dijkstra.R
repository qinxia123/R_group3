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