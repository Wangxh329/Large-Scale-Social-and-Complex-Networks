library('igraph')

#Question 16. List of nodes with degree 24.
fb_net = as.matrix(read.table('facebook_combined.txt', header = FALSE, sep = " ")) + 1
fb_graph = graph_from_edgelist(fb_net, directed = FALSE)
neigh415 = as_ids(neighbors(fb_graph, 415, mode = "all"))
neigh415 = append(neigh415, 415)
graph415 = induced_subgraph(fb_graph, neigh415)
degree_list = degree(graph415, mode = "all")
degree24 = which(degree_list == 24)
Nr = length(degree24)

#Question 17 - Common Neighbors measure
mean_accuracy = c()
for(nodei in degree24){
  accuracy = c()
  for (step in c(1:10)){
    current_graph = graph415
    R = c()
    neigh_nodei = as_ids(neighbors(current_graph, nodei, mode = "all"))
    for (node in neigh_nodei){
      if (runif(1, 0, 1) <= 0.25){
        current_graph = delete_edges(current_graph, edge(node, nodei))
        R = append(R, node)
      }
    }
    new_neigh_nodei = setdiff(neigh_nodei, R)
    new_neigh_nodei = append(new_neigh_nodei, nodei)
    non_neigh_nodei = setdiff(V(current_graph), new_neigh_nodei)
    common_neighbors_count = c()
    for (nodej in non_neigh_nodei){
      neigh_nodej = as_ids(neighbors(current_graph, nodej, mode = "all"))
      common_neighbors = intersect(new_neigh_nodei, neigh_nodej)
      common_neighbors_count = c(common_neighbors_count, length(common_neighbors))
    }
    index = sort(common_neighbors_count,decreasing=TRUE, index.return=TRUE)$ix
    P = non_neigh_nodei[index[1:length(R)]]
    intersect_R_P = intersect(R, P)
    accuracy = append(accuracy, length(intersect_R_P)/length(R))
  }
  mean_accuracy = append(mean_accuracy,mean(accuracy))
}
accuracy_common_neighbours_measure = mean(mean_accuracy)


#Question 17 - Jaccard Measure
mean_accuracy = c()
for(nodei in degree24){
  accuracy = c()
  for (step in c(1:10)){
    current_graph = graph415
    R = c()
    neigh_nodei = as_ids(neighbors(current_graph, nodei, mode = "all"))
    for (node in neigh_nodei){
      if (runif(1, 0, 1) <= 0.25){
        current_graph = delete_edges(current_graph, edge(node, nodei))
        R = append(R, node)
      }
    }
    new_neigh_nodei = setdiff(neigh_nodei, R)
    new_neigh_nodei = append(new_neigh_nodei, nodei)
    non_neigh_nodei = setdiff(V(current_graph), new_neigh_nodei)
    jaccard_count = c()
    for (nodej in non_neigh_nodei){
      neigh_nodej = as_ids(neighbors(current_graph, nodej, mode = "all"))
      common_neighbors = intersect(new_neigh_nodei, neigh_nodej)
      union_common_neighbours = union(new_neigh_nodei, neigh_nodei)
      jaccard_count = c(jaccard_count, length(common_neighbors)/length(union_common_neighbours))
    }
    index = sort(jaccard_count,decreasing=TRUE, index.return=TRUE)$ix
    P = non_neigh_nodei[index[1:length(R)]]
    intersect_R_P = intersect(R, P)
    accuracy = append(accuracy, length(intersect_R_P)/length(R))
  }
  mean_accuracy = append(mean_accuracy,mean(accuracy))
}
accuracy_jaccard_measure = mean(mean_accuracy)


#Question 17 - Adamic Adar Measure
mean_accuracy = c()
for(nodei in degree24){
  accuracy = c()
  for (step in c(1:10)){
    current_graph = graph415
    R = c()
    neigh_nodei = as_ids(neighbors(current_graph, nodei, mode = "all"))
    for (node in neigh_nodei){
      if (runif(1, 0, 1) <= 0.25){
        current_graph = delete_edges(current_graph, edge(node, nodei))
        R = append(R, node)
      }
    }
    new_neigh_nodei = setdiff(neigh_nodei, R)
    new_neigh_nodei = append(new_neigh_nodei, nodei)
    non_neigh_nodei = setdiff(V(current_graph), new_neigh_nodei)
    adamic_adar_count = c()
    for (nodej in non_neigh_nodei){
      neigh_nodej = as_ids(neighbors(current_graph, nodej, mode = "all"))
      common_neighbors = intersect(new_neigh_nodei, neigh_nodej)
      record = 0
      for (neighbour in common_neighbors){
        neighbour_size = length(neighbors(current_graph, neighbour, mode = "all"))
        record = record + 1/log2(neighbour_size)
      }
      adamic_adar_count = c(adamic_adar_count, record)
    }
    index = sort(adamic_adar_count,decreasing=TRUE, index.return=TRUE)$ix
    P = non_neigh_nodei[index[1:length(R)]]
    intersect_R_P = intersect(R, P)
    accuracy = append(accuracy, length(intersect_R_P)/length(R))
  }
  mean_accuracy = append(mean_accuracy,mean(accuracy))
}
accuracy_adamic_adar_measure = mean(mean_accuracy)