library(igraph)
library(hash)
setwd('/Users/Ray/Desktop/ECE232e/Project4/partIIdata')
practice = hash()
f1 <- file("movVact_mod.txt", "r")
line = readLines(f1)
arr = strsplit(line, "[ ]")
movie<-c()
actor<-c()
for (i in c(1:length(arr))){
  movie[[i]] = arr[[i]][1]
  actor[[i]] = arr[[i]][2]
  print(i)
}

g_practice <- read.graph("movVact_mod.txt", format="ncol", directed=FALSE)
print(sum(degree(g_practice))/2)#the edge number

plot(degree.distribution(g_practice),
     main="Degree distribution of bipartite graph of actor set and movie set",
     xlab="Degree",ylab="probability",  
     pch=16, cex=1)
plot(degree.distribution(g_practice),
     main="Degree distribution of bipartite graph of actor set and movie set (zoom)",
     xlab="Degree",ylab="probability", xlim = range(0,100), 
     pch=16, cex=1)
