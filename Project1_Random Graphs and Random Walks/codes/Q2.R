library('igraph')
library('Matrix')
library('pracma')
library('resample')

#2-1(a) Create random network with 1000 nodes, the probability of connecting is 0.01
g1 = random.graph.game(1000, 0.01, directed=F)


#2-1(b) Plot <s(t)> v.s. t and variance v.s. t
create_transition_matrix = function (g){
  # WARNING: make sure your graph is connected (you might input GCC of your graph)
  vs = V(g)
  n = vcount(g)
  adj = as_adjacency_matrix(g)
  adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with isolated nodes by creating self-edges
  z = matrix(rowSums(adj, , 1))
  
  transition_matrix = adj / repmat(z, 1, n)  # normalize to get probabilities
  
  return(transition_matrix)
}


random_walk = function (g, num_steps, start_node, transition_matrix = NULL){
  # The input must be connected graph
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  
  end_nodes = numeric(length=num_steps)
  v = start_node
  for(i in 1:num_steps){
    PMF = transition_matrix[v, ]
    v = sample(1:vcount(g), 1, prob = PMF)
    end_nodes[i] = v
  }
  
  return(end_nodes)
}

plotgraph_with_lastnodes = function (g, steps = 100, times = 1000){
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))
  tm = create_transition_matrix(g.giant)
  set.seed(1)
  nodes = matrix(NA, nrow=times, ncol=steps)
  last_nodes = array(1:times)
  for(i in 1:times){
    start = sample(1:vcount(g.giant), 1)
    row_nodes = random_walk(g.giant, steps, start, tm)
    last_nodes[i] = row_nodes[steps]
    nodes[i, ] = shortest.paths(g.giant, row_nodes, start)
  }
  means = colMeans(nodes)
  vars = colVars(nodes)
  plot(means, main = "<s(t)> v.s. t", xlab = "t", ylab = "<s(t)>")
  plot(vars, main = "standard deviation v.s. t", xlab = "t", ylab = "sigma(t)^2")
  return(last_nodes)
}

last_nodes = plotgraph_with_lastnodes(g1)



#2-1(c) Degree distribution after 1000 times random 1000-walk
degreesVector <- degree(g1)
hist(degreesVector, main = "Degree Distribution of Graph")

degrees_lastnodes <- degree(g1, last_nodes)
hist(degrees_lastnodes, main = "Degree Distribution of Last Nodes in Graph")



#2-1(d) node_num = 100, 10000
g2 = random.graph.game(100, 0.01, directed=F)
g3 = random.graph.game(10000, 0.01, directed=F)
g2_last_nodes = plotgraph_with_lastnodes(g2,500,1000)
g3_last_nodes = plotgraph_with_lastnodes(g3,100,1000)


#2-2 same as 2-1 for preferential attachment network
g4 <- barabasi.game(1000, directed=F, m=1)
last_nodes2 = plotgraph_with_lastnodes(g4,500,1000)

degreesVector <- degree(g4)
hist(degreesVector, main = "Degree Distribution of Graph")
degrees_lastnodes <- degree(g4, last_nodes)
hist(degrees_lastnodes, main = "Degree Distribution of Last Nodes in Graph")

g5 = barabasi.game(100, directed=F, m=1)
g6 = barabasi.game(10000, directed=F, m=1)
g5_last_nodes = plotgraph_with_lastnodes(g5,500,1000)
g6_last_nodes = plotgraph_with_lastnodes(g6,500,1000)


#3(a) without teleportation
g7 <- barabasi.game(1000, directed = T, m = 4)
steps = 500
times = 1000
tm = create_transition_matrix(g7)
nodes_pagerank = rep(0, 1000)
for(i in 1:times){
  start = sample(1:vcount(g7), 1)
  row_nodes = random_walk(g7, steps, start, tm)
  nodes_pagerank[row_nodes[steps]] = nodes_pagerank[row_nodes[steps]] + 1
}
plot(nodes_pagerank, main = "Probability of walker visiting each node")

degreesVector <- degree(g7, mode = "all")
plot(degreesVector, main = "Degree Distribution of Graph")



#3(b) with teleportation probability 1/N
random_walk_with_teleport = function (g, num_steps, start_node, transition_matrix = NULL, probability = NULL){
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  if(is.null(probability))
    probability = rep(1/vcount(g), vcount(g))
  
  row_nodes = numeric(length=num_steps)
  v = start_node
  for(i in 1:num_steps){
    PMF = transition_matrix[v, ]
    v1 = sample(1:vcount(g), 1, prob = probability)
    v2 = sample(1:vcount(g), 1, prob = PMF)
    random = sample(1:100, 1)
    if(random <= 15 || degree(g, v, mode = "out") == 0)
      v = v1
    else
      v = v2
    row_nodes[i] = v
  }
  
  return(row_nodes)
}


nodes = rep(0, 1000)
for(i in 1:times){
  start = sample(1:vcount(g7), 1)
  row_nodes = random_walk_with_teleport(g7, steps, start, tm)
  nodes[row_nodes[steps]] = nodes[row_nodes[steps]] + 1
}
plot(nodes)




#4(a) with teleportation probability proportional to PageRank
nodes2 = rep(0, 1000)
for(i in 1:times){
  start = sample(1:vcount(g7), 1)
  row_nodes = random_walk_with_teleport(g7, steps, start, tm, nodes_pagerank)
  nodes2[row_nodes[steps]] = nodes2[row_nodes[steps]] + 1
}
plot(nodes2)




#4(b) teleportations land on two median PageRanks nodes
ix <- sort(nodes_pagerank, index.return=T)[2]$ix
median_nodes1 = ix[times/2]
median_nodes2 = ix[times/2+1]
nodes_with_median_prob = rep(0,1000)
nodes_with_median_prob[median_nodes1] = 1/2
nodes_with_median_prob[median_nodes2] = 1/2
nodes3 = rep(0, 1000)
for(i in 1:times){
  start = sample(1:vcount(g7), 1)
  row_nodes = random_walk_with_teleport(g7, steps, start, tm, nodes_with_median_prob)
  nodes3[row_nodes[steps]] = nodes3[row_nodes[steps]] + 1
}
plot(nodes3)




#4(c) with influences from both 4(a) and 4(b)
nodes4 = rep(0, 1000)
beta = 0.8
combine_prob = nodes_pagerank * (1-beta) / 1000
combine_prob[median_nodes1] = combine_prob[median_nodes1] + beta/2
combine_prob[median_nodes2] = combine_prob[median_nodes2] + beta/2
for(i in 1:times){
  start = sample(1:vcount(g7), 1)
  row_nodes = random_walk_with_teleport(g7, steps, start, tm, combine_prob)
  nodes4[row_nodes[steps]] = nodes4[row_nodes[steps]] + 1
}
plot(nodes4)
