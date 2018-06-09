
# =================== Q4 & Q5 ====================

library(igraph)
library(ggplot2)
library(hash)
library(plyr)

g<-read.graph("preprocess_data/edge_weight_list",format="ncol",directed=TRUE)


idx_to_name = hash()
f1 <- file("preprocess_data/index_name_list", "r")
index = 0
line = readLines(f1, n = 1)
while (length(line) != 0) {
    .set(idx_to_name, index, substr(line, start = 11, stop = nchar(line)))
    line = readLines(f1, n = 1)
    index = index + 1
}

ori_to_gra = hash()
gra_to_ori = hash()
f2 <- file("preprocess_data/idx_ori2gra", "r")
line = readLines(f2, n = 1)
while (length(line) != 0) {
    arr = strsplit(line, "[ ]")
    .set(ori_to_gra, arr[[1]][1], arr[[1]][2])
    .set(gra_to_ori, arr[[1]][2], arr[[1]][1])
    line = readLines(f2, n = 1)
}


rank = page_rank(g, algo = c("prpack"), vids = V(g),
  directed = TRUE, damping = 0.85, personalized = NULL, weights = NULL,
  options = NULL)

pr = rank$vector
df <- data.frame(Object=1:length(pr),PageRank=pr)
top10 = arrange(df,desc(PageRank))


# Q4
for (i in c(1:10)) {
    top10_gra_idx = top10[i,1]
    top10_ori_idx = gra_to_ori[[as.character(top10_gra_idx)]]
    print(idx_to_name[[top10_ori_idx]])
    indeg = degree(g, v = top10_gra_idx, mode = c("in"), loops = TRUE, normalized = FALSE)
    outdeg = degree(g, v = top10_gra_idx, mode = c("out"), loops = TRUE, normalized = FALSE)
    print(paste('  in degree:', indeg[[1]]))
}


# Q5
top10map = hash()
for (i in c(1:length(top10[, 1]))) {
    object = top10[i,1]
    pagerank = top10[i, 2]
    .set(top10map, object, pagerank)
}


actors = c('Tom Cruise', 'Emma Watson (II)', 'George Clooney', 'Tom Hanks', 'Dwayne Johnson (I)', 'Johnny Depp', 'Will Smith (I)', 'Meryl Streep', 'Leonardo DiCaprio', 'Brad Pitt')
indices = c('14499', '111286', '12808', '27252', '32383', '16873', '62765', '107820', '17280', '53241')

for (index in indices) {
    print(idx_to_name[[index]])
    print(paste('pagerank:', top10map[[as.character(ori_to_gra[[as.character(index)]])]]))
    deg = degree(g, v = ori_to_gra[[as.character(index)]], mode = c("in"), loops = TRUE, normalized = FALSE)
    print(paste('  in degree:', deg[[1]]))
    print("")
}
