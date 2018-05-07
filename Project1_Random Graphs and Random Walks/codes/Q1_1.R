install.packages("igraph")
library(igraph)
#1a
p <- 0.1 
g1 <- erdos.renyi.game(1000, p=0.003, directed=F)
x1 <- seq(0,150,by=1)
y1 <- dbinom(x,1000,0.003)

g2 <- erdos.renyi.game(1000, p=0.004, directed=F)
x2 <- seq(0,150,by=1)
y2 <- dbinom(x,1000-1,0.004)

g3 <- erdos.renyi.game(1000, p=0.010, directed=F)
x3 <- seq(0,150,by=1)
y3 <- dbinom(x,1000-1,0.010)

g4 <- erdos.renyi.game(1000, p=0.050, directed=F)
x4 <- seq(0,150,by=1)
y4 <- dbinom(x,1000-1,0.050)

g5 <- erdos.renyi.game(1000, p=0.100, directed=F)
x5 <- seq(0,150,by=1)
y5 <- dbinom(x,1000-1,0.100)
plot(degree.distribution(g1),main="Degree distribution with p=0.003",xlab="Degree",ylab="Frequency")
lines(x1,y1, col = 'red')
plot(degree.distribution(g2),main="Degree distribution with p=0.004",xlab="Degree",ylab="Frequency")
lines(x2,y2, col = 'red')
plot(degree.distribution(g3),main="Degree distribution with p=0.010",xlab="Degree",ylab="Frequency")
lines(x3,y3, col = 'red')
plot(degree.distribution(g4),main="Degree distribution with p=0.05",xlab="Degree",ylab="Frequency")
lines(x4,y4, col = 'red')
plot(degree.distribution(g5),main="Degree distribution with p=0.1",xlab="Degree",ylab="Frequency")
lines(x5,y5, col="red")
#find mean and var of degree distribution for each graph
mean(degree(g1))
var(degree(g1))
mean(degree(g2))
var(degree(g2))
mean(degree(g3))
var(degree(g3))
mean(degree(g4))
var(degree(g4))
mean(degree(g5))
var(degree(g5))

#1b
#connected
v <- rep(0, 100)
for (i in 1:100){
  #g <- erdos.renyi.game(1000, p=0.003, directed=F)
  #g <- erdos.renyi.game(1000, p=0.004, directed=F)
  #g <- erdos.renyi.game(1000, p=0.010, directed=F)
  #g <- erdos.renyi.game(1000, p=0.050, directed=F)
  g <- erdos.renyi.game(1000, p=0.100, directed=F)
  #v[i] = is.connected(g)
  if (is.connected(g) == 0) {
    break
  }
}
g.components <- clusters(g)
print(g.components$csize)
ix <- which.max(g.components$csize)
# get the subgraph correspondent to just the giant component
g.giant <- induced.subgraph(g, which(g.components$membership == ix))
diameter(g.giant, directed = F)

v <- rep(0, 100)
for (i in 1:100){
  #g <- erdos.renyi.game(1000, p=0.003, directed=F)
  g <- erdos.renyi.game(1000, p=0.004, directed=F)
  #g <- erdos.renyi.game(1000, p=0.010, directed=F)
  #g <- erdos.renyi.game(1000, p=0.050, directed=F)
  #g <- erdos.renyi.game(1000, p=0.100, directed=F)
  v[i] = is.connected(g)
}
sum(v)/100

#1c
k = 1
gccfrac = rep(0,98)
for(p1 in seq(from=0.003, to=0.100, by=0.001)){
  
  v <- rep(0, 100)
  for (i in 1:100){
    g <- erdos.renyi.game(1000, p=p1, directed=F)
    g.components <- clusters(g)
    v[i] = max(g.components$csize)
  }

  gccfrac[k] = mean(v)/1000
  #try
  # g <- erdos.renyi.game(1000, p=p1, directed=F)
  # g.components <- clusters(g)
  # gccfrac[k]= g.components$csize[1]
  k=k+1
  print(k)
}
x1<-seq(from=0.003, to=0.100, by=0.001)
plot(x1, gccfrac
     , main="GCC fraction vs p",xlab="p",ylab="GCC sizes",
     pch=16, cex=0.5)
# to focus on the ln(n)/n part
k = 1
gccfrac = rep(0,121)
for(p1 in seq(from=0.003, to=0.015, by=0.0001)){
  
  v <- rep(0, 100)
  for (i in 1:100){
    g <- erdos.renyi.game(1000, p=p1, directed=F)
    g.components <- clusters(g)
    v[i] = max(g.components$csize)
  }

  gccfrac[k] = mean(v)/1000
  #try
  # g <- erdos.renyi.game(1000, p=p1, directed=F)
  # g.components <- clusters(g)
  # gccfrac[k]= g.components$csize[1]
  k=k+1
  print(k)
}
x1<-seq(from=0.003, to=0.015, by=0.0001)
plot(x1, gccfrac
     , main="GCC fraction vs p",xlab="p",ylab="GCC sizes",
     pch=16, cex=0.5)
# p=0.007 turn to linear(appoaches to GCC size = 1)

#1d
#average degree of nodes
gccfrac1 <- matrix(rep(0,9901*5),nrow=9901, ncol=5)
j <- 1
for (cs in c(0.5, 1, 1.1, 1.2, 1.3)){
  c <- cs
  k <- 1
  for(n1 in seq(from=100, to=10000, by=1)){
    v <- rep(0, 100)
    for (i in 1:100){
      g <- erdos.renyi.game(n=n1, p=c/n1, directed=F)
      g.components <- clusters(g)
      v[i] = max(g.components$csize)
    }
    gccfrac1[k, j] = mean(v)#####
    #try
    # g <- erdos.renyi.game(n=n1, p=c/n1, directed=F)
    # g.components <- clusters(g)
    # gccfrac1[k, j] <- max(g.components$csize)
    k <- k+1
    print(k)
  }
  j <- j+1
}
#(i)
x1<-seq(from=100, to=10000, by=1)
plot(x1, gccfrac1[,1]
     , main="GCC size vs n with c=0.5",xlab="n",ylab="GCC sizes",
    ylim=range(1, 25),
     pch=16, cex=0.5)
#(ii)
plot(x1, gccfrac1[,2]
     , main="GCC size vs n with c=1.0",xlab="n",ylab="GCC sizes",
     ylim=range(1, 600),
     pch=16, cex=0.5)
#(iii)
plot(x1, gccfrac1[,3]
     , main="GCC size vs n with c",xlab="n",ylab="GCC sizes",
     ylim=range(1, 4500),
     pch=16, cex=0.5)
points(x1, gccfrac1[,4],col='red',
     pch=16, cex=0.5)
points(x1, gccfrac1[,5],col='blue',
       pch=16, cex=0.5)
op <- par(cex = 1)
legend("topleft", c("c=1.1","c=1.2", "c=1.3"),
       col=c("black","red","blue"), pch=c(16,16,16), pt.cex=1.0, cex=0.8)


