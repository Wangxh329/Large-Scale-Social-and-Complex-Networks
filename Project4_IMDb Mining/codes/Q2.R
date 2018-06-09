
# =================== Q2 ====================

library(igraph)
library(ggplot2)

in_degree_distribution_orig = function(graph) {
    # count the frequency of each degree
    freq <- sort(degree(graph, mode="in"))
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
    labs(title="In-degree Distribution of the Actor/Actress Network (frequency)", x="In-degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

in_degree_distribution_prob = function(graph) {
    plot(degree.distribution(graph, mode="in"), type="p", col="hotpink", main="In-degree Distribution of the Actor/Actress Network (probability)",xlab="In-degree",ylab="Probability")
}

# create weighted directed network
g1 <- read.graph("preprocess_data/edge_weight_list",format="ncol",directed=TRUE)

# plot in-degree distribution (frequency and probability)
in_degree_distribution_orig(g1)
in_degree_distribution_prob(g1)

