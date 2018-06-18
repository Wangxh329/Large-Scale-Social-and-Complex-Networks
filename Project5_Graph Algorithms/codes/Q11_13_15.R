###############################################################################
library(igraph)
#Q11
setwd('/Users/Ray/Desktop/ECE232_HW5')
g10 <- read.graph('sidDidTime_only.txt', format="ncol", directed=TRUE)
#the graph would rename the node by ID
idx2nodeID = vertex_attr(g10)[[1]]
class(idx2nodeID) <- "numeric"

edge_wTimeDist <- read.table('sidDidTime_dist.txt')
edgeNotime <- read.table('edgeNotime.txt')
#to calculate edgeNotime distance and time
time_dist <- matrix(-1, nrow = dim(edgeNotime)[1], ncol = 2)
edgeNotime_extend <- cbind(data.matrix(edgeNotime), time_dist)

for (i in c(1:dim(edgeNotime)[1])){
  NodeID = edgeNotime[i,1:2]
  idx_graph = match(NodeID, idx2nodeID) #from nodeID to index on graph
  if ((is.na(idx_graph[1]) | is.na(idx_graph[2])) ==1 ){
    next
  }
  min_time = distances(g10, idx_graph[1], to = idx_graph[2], mode = 'out',
                       weights = E(g10)$weight)
  if (min_time < 100000){
    edgeNotime_extend[i,3] = min_time #record time
    #do path distance calculation
    path_short = all_shortest_paths(g10, from = idx_graph[1], to = idx_graph[2], mode = 'out',
                                    weights = E(g10)$weight)$res[[1]] 
    cnt_dist = 0
    for(j in c(1:(length(path_short) - 1))){
      temppair <- path_short[j:(j+1)]
      temppair <- as.numeric(temppair) #index (need to change to nodeID)
      nodeIDpair <- idx2nodeID[temppair] #turn idx to nodeID
      for (k in c(1: dim(edge_wTimeDist)[1]))
        if (all(nodeIDpair == c(edge_wTimeDist[k,1], edge_wTimeDist[k,2]))){
          cnt_dist = cnt_dist + edge_wTimeDist[k,4]
          #print(k)
          break
        }
    }
    edgeNotime_extend[i,4] = cnt_dist #record distance
  }
  print(i)
}

i_s =c()
for (i in c(1:dim(edgeNotime_extend)[1])){
  if (edgeNotime_extend[i,3] > 0){
  i_s <- c(i_s, i)
  }
}
df_temp <- edgeNotime_extend[i_s, ]
edge_wTimeDist_new <- rbind(edge_wTimeDist, df_temp)
edge_wTimeOnly_new <- edge_wTimeDist_new[,1:3]

#write.table(edge_wTimeOnly_new, "edge_wTimeOnly_11.txt",sep="\t",row.names=FALSE)
g_11 <- read.graph('edge_wTimeOnly_11.txt', format="ncol", directed=TRUE)
idx2nodeID2 = vertex_attr(g_11)[[1]]
class(idx2nodeID2) <- "numeric"
idx_graph <- match(2607, idx2nodeID2)
nei <- neighbors(g_11, idx_graph, mode = c("out"))
idx2nodeID2[nei]

#Q13
table_temp <- read.table('Q12flow.txt')
colnames(table_temp) <- c("from", "to", "capacity")
g12 <- graph_from_data_frame(as.data.frame(table_temp))
idx2nodeID3 = vertex_attr(g12)[[1]]
class(idx2nodeID3) <- "numeric"
idx_graph1 <- match(2607, idx2nodeID3) #454
idx_graph2 <- match(1968, idx2nodeID3) #594
max_car = max_flow(g12, source=idx_graph1, target=idx_graph2, capacity = NULL)$value
plot(make_ego_graph(g12, 1, nodes = 454, mode = 'out',mindist = 0)[[1]])
plot(make_ego_graph(g12, 1, nodes = 594, mode = 'in',mindist = 0)[[1]])

#Q15
table_temp1 <- read.table('Q12flow_pick.txt')
colnames(table_temp1) <- c("from", "to", "capacity")
g15 <- graph_from_data_frame(as.data.frame(table_temp1))
idx2nodeID4 = vertex_attr(g15)[[1]]
class(idx2nodeID4) <- "numeric"
idx_graph1 <- match(2607, idx2nodeID4) #440
idx_graph2 <- match(1968, idx2nodeID4) #578
max_car = max_flow(g15, source=idx_graph1, target=idx_graph2, capacity = NULL)$value #12062
plot(make_ego_graph(g15, 1, nodes = idx_graph1, mode = 'all', mindist = 0)[[1]])
plot(make_ego_graph(g15, 1, nodes = idx_graph2, mode = 'all', mindist = 0)[[1]])

