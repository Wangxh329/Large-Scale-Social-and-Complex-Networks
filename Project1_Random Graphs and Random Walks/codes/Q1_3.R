library(igraph)
library(ggplot2)

# ========================== (a) ============================
# create a new network
g <- sample_pa_age(1000, pa.exp=1, aging.exp=-1, m=1, directed = F,
  zero.deg.appeal = 1, zero.age.appeal = 0, deg.coef = 1, age.coef = 1)
plot(g, edge.arrow.size=.5, vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="") 

# plot degree distribution
degree_distribution_orig = function(graph) {
    # count the frequency of each degree
    freq <- sort(degree(graph))
    df <- data.frame(degree=c(NA), freq=c(NA))
    df <- df[-1, ]
    curDegree <- freq[1]
    count <- 1
    for (i in 2:length(freq)) {
        if (freq[i] != curDegree) {
            df[nrow(df)+1, ] <- c(curDegree, count)
            curDegree <- freq[i]
            count <- 1
        } else {
            count <- count + 1
        }
    }
    df[nrow(df)+1, ] <- c(curDegree, count)
    
    # plot original degree distribution histogram
    ggplot(df, aes(x=degree, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    geom_text(aes(x=degree,y=freq+12,label=freq),color="hotpink",size=5,show.legend = T)+
    labs(title="Degree Distribution of the Network (PA + age, n=1000, m=1)", x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

degree_distribution_log = function(graph) {
    # plot degree distribution in log-log scale 
    degree <- log2(c(1:length(degree.distribution(graph))))
    distr <- log2(degree.distribution(graph))
    df <- data.frame(degree, distr)
    print(df)
    plot(degree, distr, type="o", col="hotpink", main="Degree Distribution in Log Scale (PA + age, n=1000, m=1)",xlab="log(degree)",ylab="log(probability)")
    lines(seq(1,4,0.03), seq(-0.75,-10,-0.0925),col="deepskyblue",lty=2)
}

degree_distribution_orig(g)
degree_distribution_log(g)

# ========================== (b) ============================
# community detection and measure modularity
communities <- cluster_fast_greedy(g)
modularity(communities)
print(communities)
plot(g, mark.groups=groups(communities), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="")

# plot community structure
index <- c(1:length(communities))
size <- as.vector(sizes(communities))
comm_df <- data.frame(index, size)

ggplot(comm_df, aes(x=index, y=size))+ 
geom_bar(stat = "identity", fill="hotpink")+
geom_text(aes(x=index,y=size+1,label=size),color="hotpink",show.legend = T)+
labs(title="Community Structure (PA + age)", x="Community", y="Size")+
theme(plot.title = element_text(hjust = 0.5))
