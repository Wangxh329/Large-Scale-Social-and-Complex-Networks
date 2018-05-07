library(igraph)
library(ggplot2)

# ========================== (a) ============================
# create random graph with 1000 nodes and each time step attached 1 edge
connected_time <- 0
for (i in 1:100) {
    g1 <- barabasi.game(1000, m=1, directed=F)
    if (is.connected(g1)) {
        connected_time <- connected_time + 1
    }
}
plot(g1, edge.arrow.size=.5, vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="") 
connected_time

# ========================== (b) ============================
# community detection and measure modularity
communities1 <- cluster_fast_greedy(g1)
modularity(communities1)
print(communities1)
plot(g1, mark.groups=groups(communities1), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main="n = 1000, m = 1")

# plot community structure
index <- c(1:length(communities1))
c1_size <- as.vector(sizes(communities1))
comm_df <- data.frame(index, c1_size)

ggplot(comm_df, aes(x=index, y=c1_size))+ 
geom_bar(stat = "identity", fill="hotpink")+
geom_text(aes(x=index,y=c1_size+2,label=c1_size),color="hotpink",show.legend = T)+
labs(title="Community Structure (n=1000, m=1)", x="Community", y="Size")+
theme(plot.title = element_text(hjust = 0.5))

# ========================== (c) ============================
# generate a network with 10000 nodes, m=1
g2 <- barabasi.game(10000, m=1, directed=F)
plot(g2, edge.arrow.size=.5, vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="") 

# community detection and measure modularity
communities2 <- cluster_fast_greedy(g2)
modularity(communities2)
print(communities2)
plot(g2, mark.groups=groups(communities2), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main="n = 10000, m = 1")

# plot community structure
index2 <- c(1:length(communities2))
c2_size <- as.vector(sizes(communities2))
comm_df2 <- data.frame(index2, c2_size)

ggplot(comm_df2, aes(x=index2, y=c2_size))+ 
geom_bar(stat = "identity", fill="hotpink", width=.3)+
labs(title="Community Structure (n=10000, m=1)", x="Community", y="Size")+
theme(plot.title = element_text(hjust = 0.5))

# ========================== (d) ============================
degree_distribution_orig = function(graph, n) {
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
    text_size <- 0
    dist <- 0
    if (n == 1000) {
        text_size <- 4
        dist <- 12
    } else {
        text_size <- 2
        dist <- 70
    }
    
    ggplot(df, aes(x=degree, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    geom_text(aes(x=degree,y=freq+dist,label=freq),color="hotpink",size=text_size,show.legend = T)+
    labs(title=paste("Degree Distribution of the Network (n=",n,", m=1)",collapse=""), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

degree_distribution_log = function(graph, n) {
    # plot degree distribution in log-log scale 
    degree <- log2(c(1:length(degree.distribution(graph))))
    distr <- log2(degree.distribution(graph))
    df <- data.frame(degree, distr)
    print(df)
    plot(degree, distr, type="o", col="hotpink", main=paste("Degree Distribution of the Network in Log Scale (n=",n,", m=1)",collapse=""),xlab="log(degree)",ylab="log(possibility)")
    if (n == 1000) {
        lines(seq(1,4,0.03), seq(-0.75,-10,-0.0925),col="deepskyblue",lty=2)
    } else {
        lines(seq(1,5.5,0.045), seq(-0.75,-13.55,-0.128),col="deepskyblue",lty=2)
    }
}

# plot n=1000
print("=================================== n = 1000 =====================================")
degree_distribution_orig(g1, 1000)
degree_distribution_log(g1, 1000)
print("==================================================================================")

# plot n=10000
print("=================================== n = 10000 ====================================")
degree_distribution_orig(g2, 10000)
degree_distribution_log(g2, 10000)
print("==================================================================================")


# ========================== (e) ============================
sample_count_degree = function(graph) {
    # sample vcount nodes, then sample their 1 neighbor for each, and get the degree
    degrees = array(0, vcount(graph))
    for (i in 1:vcount(graph)) {
        node <- sample(vcount(graph), 1)
        neighbors <- neighbors(graph, node, mode="total")
        if (length(neighbors) > 0) {
            neighbor = sample(length(neighbors), 1)
            degrees[i] <- degree(graph, neighbors[neighbor])
        }
    }
    
    # count the frequency of each degree
    freq <- sort(degrees)
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
    return (df)
}

random_degree_distribution_orig = function(df, n) {
    # plot original degree distribution histogram
    text_size <- 0
    dist <- 0
    if (n == 1000) {
        text_size <- 4
        dist <- 5
    } else {
        text_size <- 2
        dist <- 30
    }
    
    ggplot(df, aes(x=degree, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    geom_text(aes(x=degree,y=freq+dist,label=freq),color="hotpink",size=text_size,show.legend = T)+
    labs(title=paste("Random Degree Distribution of the Network (n=",n,", m=1)",collapse=""), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

random_degree_distribution_log = function(df, n) {
    # calculate P for each degree
    degree <- log2(df["degree"])
    distr <- log2(df["freq"]/n)
    dataframe <- data.frame(degree, distr)
    print(dataframe)
    
    plot(dataframe, type="o", col="hotpink", main=paste("Random Degree Distribution in Log Scale (n=",n,", m=1)",collapse=""),xlab="log(degree)",ylab="log(possibility)")
    if (n == 1000) {
        lines(seq(1,4.5,0.035), seq(-2,-8,-0.06),col="deepskyblue",lty=2)
    } else {
        lines(seq(1,5.5,0.045), seq(-2.2,-9.4,-0.072),col="deepskyblue",lty=2)
    }
}

# plot n=1000
print("=================================== n = 1000 =====================================")
df <- sample_count_degree(g1)
random_degree_distribution_orig(df, 1000)
random_degree_distribution_log(df, 1000)
print("==================================================================================")

# plot n=10000
print("=================================== n = 10000 ====================================")
df <- sample_count_degree(g2)
random_degree_distribution_orig(df, 10000)
random_degree_distribution_log(df, 10000)
print("==================================================================================")


# ========================== (f) ============================
# iterate 1000 times, and calculate the mean degree for each age
degrees <- array(0, 1000)
for (i in 1:1000) {
    g <- barabasi.game(1000,m=1,directed=F)
    degrees <- degrees+degree(g)
}
degrees <- degrees/1000
age <- c(999:0)
age_degree_df <- data.frame(age, degrees)
age_degree_df
sum(degrees)

# plot original expected degree vs age 
plot(age_degree_df, type="l", col="hotpink", main="Expected Degree of Nodes vs Age of Nodes (n=1000, m=1)",xlab="Age",ylab="Expected Degree")
# fitting the curve
x <- c(0:999)
y <- 100/(1000-x) + 0.001*x + 1
lines(x, y, col="deepskyblue", lty=2)
legend(70,19,c("original data","fitted curve"),col=c("hotpink","deepskyblue"),text.col=c("hotpink","deepskyblue"),lty=c(1,2))

# ========================== (g) ============================
generate_network_connectivity = function(n, m) {
    connected_time <- 0
    for (i in 1:100) {
        graph <- barabasi.game(n, m=m, directed=F)
        if (is.connected(graph)) {
            connected_time <- connected_time + 1
        }
    }
    print(paste("n = ", n, ", m = ", m, collapse=""))
    plot(g1, edge.arrow.size=.5, vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="",
        main=paste("n = ", n, ", m = ", m, collapse="")) 
    print(connected_time)
    return(graph)
}

g3 <- generate_network_connectivity(1000, 2)
g4 <- generate_network_connectivity(10000, 2)
g5 <- generate_network_connectivity(1000, 5)
g6 <- generate_network_connectivity(10000, 5)


community_structure_modularity = function(graph, n, m) {
    # community detection and measure modularity
    communities <- cluster_fast_greedy(graph)
    print(paste("n = ", n, ", m = ", m, collapse=""))
    print(modularity(communities))
    print(communities)
    plot(graph, mark.groups=groups(communities), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main=paste("n = ",n,", m = ",m, collapse=""))

    # plot community structure
    index <- c(1:length(communities))
    size <- as.vector(sizes(communities))
    df <- data.frame(index, size)

    ggplot(df, aes(x=index, y=size))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    labs(title=paste("Community Structure (n=", n, ", m=", m, ")", collapse=""), x="Community", y="Size")+
    theme(plot.title = element_text(hjust = 0.5))
}

community_structure_modularity(g3, 1000, 2)
community_structure_modularity(g4, 10000, 2)
community_structure_modularity(g5, 1000, 5)
community_structure_modularity(g6, 10000, 5)


new_degree_distribution_orig = function(graph, n, m) {
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
    labs(title=paste("Degree Distribution of the Network (n=",n,", m=",m,")",collapse=""), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

new_degree_distribution_log = function(graph, n, m) {
    # plot degree distribution in log-log scale 
    degree <- log2(c(1:length(degree.distribution(graph))))
    distr <- log2(degree.distribution(graph))
    df <- data.frame(degree, distr)
    print(df)
    plot(degree, distr, type="o", col="hotpink", main=paste("Degree Distribution of the Network in Log Scale (n=",n,", m=",m,")",collapse=""),xlab="log(degree)",ylab="log(probability)")
    if (n == 1000 && m == 2) {
        lines(seq(1.6,4.4,0.028), seq(-1.2,-10,-0.088),col="deepskyblue",lty=2)
    } else if (n == 10000 && m == 2) {
        lines(seq(1.6,5.7,0.041), seq(-1.2,-13.3,-0.121),col="deepskyblue",lty=2)
    } else if (n == 1000 && m == 5) {
        lines(seq(2.6,5.4,0.028), seq(-2,-10,-0.08),col="deepskyblue",lty=2)
    } else {
        lines(seq(2.6,6.4,0.038), seq(-2,-13.3,-0.113),col="deepskyblue",lty=2)
    }
}

# plot n=1000, m=2
print("============================ n = 1000, m = 2 =====================================")
new_degree_distribution_orig(g3, 1000, 2)
new_degree_distribution_log(g3, 1000, 2)
print("==================================================================================")

# plot n=10000, m=2
print("============================ n = 10000, m = 2 ====================================")
new_degree_distribution_orig(g4, 10000, 2)
new_degree_distribution_log(g4, 10000, 2)
print("==================================================================================")

# plot n=1000, m=5
print("============================ n = 1000, m = 5 =====================================")
new_degree_distribution_orig(g5, 1000, 5)
new_degree_distribution_log(g5, 1000, 5)
print("==================================================================================")

# plot n=10000, m=5
print("============================ n = 10000, m = 5 ====================================")
new_degree_distribution_orig(g6, 10000, 5)
new_degree_distribution_log(g6, 10000, 5)
print("==================================================================================")


new_sample_count_degree = function(graph) {
    # sample vcount nodes, then sample their 1 neighbor for each, and get the degree
    degrees = array(0, vcount(graph))
    for (i in 1:vcount(graph)) {
        node <- sample(vcount(graph), 1)
        neighbors <- neighbors(graph, node, mode="total")
        if (length(neighbors) > 0) {
            neighbor = sample(length(neighbors), 1)
            degrees[i] <- degree(graph, neighbors[neighbor])
        }
    }
    
    # count the frequency of each degree
    freq <- sort(degrees)
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
    return (df)
}

new_random_degree_distribution_orig = function(df, n, m) {
    # plot original degree distribution histogram
    ggplot(df, aes(x=degree, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    labs(title=paste("Random Degree Distribution of the Network (n=",n,", m=",m,")",collapse=""), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

new_random_degree_distribution_log = function(df, n, m) {
    # calculate P for each degree
    degree <- log2(df["degree"])
    distr <- log2(df["freq"]/n)
    dataframe <- data.frame(degree, distr)
    print(dataframe)
    
    plot(dataframe, type="o", col="hotpink", main=paste("Random Degree Distribution in Log Scale (n=",n,", m=",m,")",collapse=""),xlab="log(degree)",ylab="log(probability)")
    if (n == 1000 && m == 2) {
        lines(seq(1.6,4.6,0.03), seq(-2.6,-8,-0.054),col="deepskyblue",lty=2)
    } else if (n == 10000 && m == 2) {
        lines(seq(1.6,6,0.044), seq(-2.7,-9.5,-0.068),col="deepskyblue",lty=2)
    } else if (n == 1000 && m == 5) {
        lines(seq(2.6,5.6,0.03), seq(-3.3,-9,-0.057),col="deepskyblue",lty=2)
    } else {
        lines(seq(2.3,7,0.047), seq(-3.2,-10.1,-0.069),col="deepskyblue",lty=2)
    }
}


# plot n=1000, m=2
print("============================ n = 1000, m = 2 =====================================")
df <- sample_count_degree(g3)
new_random_degree_distribution_orig(df, 1000, 2)
new_random_degree_distribution_log(df, 1000, 2)
print("==================================================================================")

# plot n=10000, m=2
print("============================ n = 10000, m = 2 ====================================")
df <- sample_count_degree(g4)
new_random_degree_distribution_orig(df, 10000, 2)
new_random_degree_distribution_log(df, 10000, 2)
print("==================================================================================")

# plot n=1000, m=5
print("============================ n = 1000, m = 5 =====================================")
df <- sample_count_degree(g5)
new_random_degree_distribution_orig(df, 1000, 5)
new_random_degree_distribution_log(df, 1000, 5)
print("==================================================================================")

# plot n=10000, m=5
print("============================ n = 10000, m = 5 ====================================")
df <- sample_count_degree(g6)
new_random_degree_distribution_orig(df, 10000, 5)
new_random_degree_distribution_log(df, 10000, 5)
print("==================================================================================")


age_degree = function(m) {
    # iterate 1000 times, and calculate the mean degree for each age
    degrees <- array(0, 1000)
    for (i in 1:1000) {
        g <- barabasi.game(1000,m=m,directed=F)
        degrees <- degrees+degree(g)
    }
    degrees <- degrees/1000
    age <- c(999:0)
    age_degree_df <- data.frame(age, degrees)
    age_degree_df
    sum(degrees)

    # plot original expected degree vs age 
    plot(age_degree_df, type="l", col="hotpink", main=paste("Expected Degree of Nodes vs Age of Nodes (n=1000, m=",m,")", collapse=""),xlab="Age",ylab="Expected Degree")
    # fitting the curve
    x <- c(0:999)
    y <- 0
    xlab <- 50
    ylab <- 0
    if (m == 2) {
        y <- 100/(1000-x) + 0.002*x + 2
        ylab <- 38
    } else {
        y <- 200/(1000-x) + 0.004*x + 5
        ylab <- 78
    }
    
    lines(x, y, col="deepskyblue", lty=2)
    legend(xlab,ylab,c("original data","fitted curve"),col=c("hotpink","deepskyblue"),text.col=c("hotpink","deepskyblue"),lty=c(1,2))
}

# n=1000, m=2
age_degree(2)
# n=1000, m=5
age_degree(5)

# ========================== (h) ============================
# generate a new network with n=1000, m=1
g <- barabasi.game(1000, m=1, directed=F)
communities1 <- cluster_fast_greedy(g)
print(paste("Modularity of original network: ",modularity(communities1),collapse=""))
print(paste("Number of communities of original network: ",length(communities1),collapse=""))
plot(g, mark.groups=groups(communities1), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main="Original Network") 

# generate using degree sequence method
# 1) use "vl"
degseq_graph1 <- sample_degseq(degree(g), method="vl")
communities2 <- cluster_fast_greedy(degseq_graph1)
print(paste("Modularity of degree sequence network (vl method): ",modularity(communities2),collapse=""))
print(paste("Number of communities of degree sequence network (vl method): ",length(communities2),collapse=""))
plot(degseq_graph1, mark.groups=groups(communities2), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main="Degree Sequence + vl") 
print(paste("vl method: ",is.connected(degseq_graph1)))

# 2) use "simple.no.multiple"
degseq_graph2 <- sample_degseq(degree(g), method="simple.no.multiple")
communities3 <- cluster_fast_greedy(degseq_graph2)
print(paste("Modularity of degree sequence network (simple.no.multiple method): ",modularity(communities3),collapse=""))
print(paste("Number of communities of degree sequence network (simple.no.multiple method): ",length(communities3),collapse=""))
plot(degseq_graph2, mark.groups=groups(communities3), edge.arrow.size=.5, 
     vertex.color="gold", vertex.size=6, vertex.frame.color="gray", vertex.label="", 
    main="Degree Sequence + simple.no.multiple") 
print(paste("simple.no.multiple method: ",is.connected(degseq_graph2)))

