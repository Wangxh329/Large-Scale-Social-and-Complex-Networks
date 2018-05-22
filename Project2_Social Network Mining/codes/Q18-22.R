library('igraph')
library('entropy')
library('infotheo')

#Q18
myfiles = list.files(path="gplus", pattern="*.circles")
count = 0
for (file in myfiles){
  file = paste("gplus", file, sep="/")
  if (length(readLines(file)) > 2){
    count = count + 1
  }
}
print(count)

#Q19 - indegree and outdegree plot for three nodes
node1 = "109327480479767108490"
node2 = "115625564993990145546"
node3 = "101373961279443806744"

options(scipen=999)
file1 = paste("gplus/", node1, ".edges", sep = "")
gplus_graph1 = read_graph(file1, format="ncol", directed=TRUE)
gplus_graph1 = gplus_graph1 + vertex(node1)
for (i in seq(1, vcount(gplus_graph1)-1,1)){
  gplus_graph1 = gplus_graph1 + edge(vcount(gplus_graph1), i)
}
InDegree1 = degree(gplus_graph1, mode = "in")
plot(InDegree1, main = "In-Degree Distribution of Graph")
OutDegree1 = degree(gplus_graph1, mode = "out")
plot(OutDegree1, main = "Out-Degree Distribution of Graph")

options(scipen=999)
file2 = paste("gplus/", node2, ".edges", sep = "")
gplus_graph2 = read_graph(file2, format="ncol", directed=TRUE)
gplus_graph2 = gplus_graph2 + vertex(node2)
for (i in seq(1, vcount(gplus_graph2)-1,1)){
  gplus_graph2 = gplus_graph2 + edge(vcount(gplus_graph2), i)
}
InDegree2 = degree(gplus_graph2, mode = "in")
plot(InDegree2, main = "In-Degree Distribution of Graph")
OutDegree2 = degree(gplus_graph2, mode = "out")
plot(OutDegree2, main = "Out-Degree Distribution of Graph")

options(scipen=999)
file3 = paste("gplus/", node3, ".edges", sep = "")
gplus_graph3 = read_graph(file3, format="ncol", directed=TRUE)
gplus_graph3 = gplus_graph3 + vertex(node3)
for (i in seq(1, vcount(gplus_graph3)-1,1)){
  gplus_graph3 = gplus_graph3 + edge(vcount(gplus_graph3), i)
}
InDegree3 = degree(gplus_graph3, mode = "in")
plot(InDegree3, main = "In-Degree Distribution of Graph")
OutDegree3 = degree(gplus_graph3, mode = "out")
plot(OutDegree3, main = "Out-Degree Distribution of Graph")



#Q20. Compare Modularity Scords & Plot Communities
wtc1 = cluster_walktrap(gplus_graph1)
modul1 = modularity(wtc1)
colors1 = rainbow(max(membership(wtc1)))
plot(wtc1, gplus_graph1, vertex.colors = colors1[membership(wtc1)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)


wtc2 = cluster_walktrap(gplus_graph2)
modul2 = modularity(wtc2)
colors2 = rainbow(max(membership(wtc2)))
plot(wtc2, gplus_graph2, vertex.colors = colors1[membership(wtc2)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)

wtc3 = cluster_walktrap(gplus_graph3)
modul3 = modularity(wtc3)
colors3 = rainbow(max(membership(wtc3)))
plot(wtc3, gplus_graph3, vertex.colors = colors1[membership(wtc3)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)



#Q22
file4 = paste("gplus/", node1, ".circles", sep = "")
circle_nodes1 = c()
circle1 = c()
read_lines = readLines(file4)
for (i in c(1:length(read_lines))) {
  lines = strsplit(read_lines[i], "\t")
  lines = tail(lines[[1]], -1)
  circle_nodes1 = c(circle_nodes1, lines)
  circle1 = c(circle1, rep(i,length(lines)))
}

community1 = c()
for (circle in circle_nodes1){
  for (node in circle){
    index = membership(wtc1)[[node]]
    community1 = c(community1, index)
  }
}

HC1 = entropy(circle1)
HK1 = entropy(community1)
HCK1 = condentropy(circle1, community1)
HKC1 = condentropy(community1, circle1)
h1 = 1 - HCK1 / HC1
c1 = 1 - HKC1 / HK1





file5 = paste("gplus/", node2, ".circles", sep = "")
circle_nodes2 = c()
circle2 = c()
read_lines = readLines(file5)
for (i in c(1:length(read_lines))) {
  lines = strsplit(read_lines[i], "\t")
  lines = tail(lines[[1]], -1)
  circle_nodes2 = c(circle_nodes2, lines)
  circle2 = c(circle2, rep(i,length(lines)))
}

community2 = c()
for (circle in circle_nodes2){
  for (node in circle){
    index = membership(wtc2)[[node]]
    community2 = c(community2, index)
  }
}

HC2 = entropy(circle2)
HK2 = entropy(community2)
HCK2 = condentropy(circle2, community2)
HKC2 = condentropy(community2, circle2)
h2 = 1 - HCK2 / HC2
c2 = 1 - HKC2 / HK2





file6 = paste("gplus/", node3, ".circles", sep = "")
circle_nodes3 = c()
circle3 = c()
read_lines = readLines(file6)
for (i in c(1:length(read_lines))) {
  lines = strsplit(read_lines[i], "\t")
  lines = tail(lines[[1]], -1)
  circle_nodes3 = c(circle_nodes3, lines)
  circle3 = c(circle3, rep(i,length(lines)))
}

community3 = c()
for (circle in circle_nodes3){
  for (node in circle){
    index = membership(wtc3)[[node]]
    community3 = c(community3, index)
  }
}

HC3 = entropy(circle3)
HK3 = entropy(community3)
HCK3 = condentropy(circle3, community3)
HKC3 = condentropy(community3, circle3)
h3 = 1 - HCK3 / HC3
c3 = 1 - HKC3 / HK3