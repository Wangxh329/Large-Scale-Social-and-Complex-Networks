#Q1
#install.packages("igraph")
#library(igraph)
setwd("~/Desktop/ECE232e")
mydata = read.table("facebook_combined.txt")
mydata = as.matrix(mydata)#igraph need to be matrix format
mydata[,1] = as.character(mydata[,1])#change to character to draw graph
mydata[,2] = as.character(mydata[,2])

g = graph_from_edgelist(el, directed = F)
g = graph.edgelist(mydata[,1:2], directed = F)
plot(g)
is.connected(g)#connected

#Q2
diameter(g, directed = F) #8

#Q3
plot(degree.distribution(g),main="Degree distribution
     ",xlab="Degree",ylab="Frequency", pch=16, cex=0.5)
mean(degree(g)) #43.69

#Q4
plot(degree.distribution(g), log="xy", main="Degree distribution(log scale)
     ",xlab="Degree(log)",ylab="Frequency(log)")
x1 <- seq(15,200,by=1)
y1 <- x1^(-1.35) * (1)
lines(x1, y1, col = 'red')

#Q5
g_1 <- induced_subgraph(g, c(1, neighbors(g,1)))
plot(g_1)
vcount(g_1) #348
ecount(g_1) #2866
