library("igraph")
options(scipen=999)

#read data
g = read.graph("weight2.txt", format="ncol")

sectorData = readLines("finance_data/finance_data/Name_sector.csv", skip=1)
sectorData = strsplit(sectorData, ",")
names(sectorData) = sapply(sectorData, "[[", 1)
sectorData = lapply(sectorData, "[", -1)

colors = list("Health Care"="red", "Industrials"="green", "Consumer Discretionary"="blue",
              "Information Technology"="yellow", "Consumer Staples"="orange", "Utilities"="purple",
              "Financials"="blueviolet", "Real Estate"="gold", "Materials"="deeppink",
              "Energy"="chartreuse", "Telecommunication Services"="azure")

#1.2
deg = degree(g)
hist(c(492,494,deg), xlab="degrees", main="Histogram of degrees")
weigts = edge.attributes(g)[['weight']]
hist(weigts, breaks=400)

#1.3
sectors = unlist(sectorData[V(g)$name], use.names=FALSE)
V(g)$color = unlist(colors[sectors], use.names=FALSE)
mst = mst(g, weights=weigts)
plot(mst, vertex.size=4, vertex.label=NA)

v = length(V(g))
s = list("Health Care"=0, "Industrials"=0, "Consumer Discretionary"=0,
         "Information Technology"=0, "Consumer Staples"=0, "Utilities"=0,
         "Financials"=0, "Real Estate"=0, "Materials"=0,
         "Energy"=0, "Telecommunication Services"=0)
for (sector in sectors){
  s[[sector]] = s[[sector]] + 1
}
q = c()
n = c()
p1 = c()
p2 = c()
for (node in V(g)){
  qi = 0
  ni = 0
  sector = sectorData[[node]]
  for (neighbor in neighbors(g, node)){
    if (sectorData[[neighbor]] == sector){
      qi = qi + 1
    }
    ni = ni + 1
  }
  q = c(q, qi)
  n = c(n, ni)
  p1 = c(p1, qi / ni)
  p2 = c(p2, s[[sector]] / v)
}
alpha1 = sum(p1) / v
alpha2 = sum(p2) / v
