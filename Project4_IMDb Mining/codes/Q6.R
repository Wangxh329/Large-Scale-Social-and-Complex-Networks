
# =================== Q6 ====================

library(igraph)
library(ggplot2)

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
    labs(title="Degree Distribution of the Movie Network (frequency)", x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5))
}

degree_distribution_prob = function(graph) {
    plot(degree.distribution(graph), type="p", col="hotpink", main="Degree Distribution of the Movie Network (probability)",xlab="Degree",ylab="Probability")
}

# create weighted undirected network
g2 <- read.graph("preprocess_data/edge_weight2", format="ncol", directed=FALSE)

# plot degree distribution (frequency and probability)
degree_distribution_orig(g2)
degree_distribution_prob(g2)

