# =================== Q7 ====================

library(igraph)
library(ggplot2)
library(hash)
library(plyr)

g <- read.graph("preprocess_data/edge_weight2", format="ncol", directed=FALSE)
print('graph loaded!')
print('clustering...')
fg = cluster_fast_greedy(g)
print('clustering finished')
print(fg)

print('init 2 dicts...')
idx_to_mov = hash()
f1 <- file("preprocess_data/index2movie", "r")
lines = readLines(f1)
for (line in lines) {
    if (nchar(line) > 1) {
        arr = strsplit(line, "[ ]")
        .set(idx_to_mov, arr[[1]][1], substr(line, start = 11, stop = nchar(line)))
    }
}

mov_to_gen = hash()
f2 <- file("preprocess_data/movie_genre_utf-8", "r")
lines = readLines(f2)
for (line in lines) {
    if (nchar(line) > 1) {
        arr = strsplit(line, "[\t\t]")
        .set(mov_to_gen, arr[[1]][1], arr[[1]][3])
    }
}
print('dicts initialized')

for (i in c(1:10)) {
    print(paste('=======', i, '======'))
    ci = fg[[i]]
    genre_count = hash()
    for (m_idx in ci) {
        movie = idx_to_mov[[as.character(m_idx)]]
        genre = mov_to_gen[[movie]]
        if (is.null(genre)) {
            if (is.null(genre_count[['other']])) {
                .set(genre_count, 'other', 0)
            }
            .set(genre_count, 'other', genre_count[['other']] + 1)
        } else {
            if (is.null(genre_count[[genre]])) {
                .set(genre_count, genre, 0)
            }
            .set(genre_count, genre, genre_count[[genre]] + 1)
        }

    }
    
    key_list = c()
    val_list = c()
    for (k in ls(genre_count)) {
        key_list = c(key_list, k)
        val_list = c(val_list, genre_count[[k]])
    }
    
    df <- data.frame(genre=key_list, freq=val_list)
    # plot genre distribution histogram
    dis_plot <- ggplot(df, aes(x=genre, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    labs(title=paste("Distribution of the Genres of the Movies (Community ", i, ", ", length(fg[[i]]), " movies)", collapse=""), x="Genre", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    print(dis_plot)
}

