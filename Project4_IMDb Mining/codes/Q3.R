
# =================== Q3 ====================

library(igraph)
library(ggplot2)
library(hash)


g <- read.graph("preprocess_data/edge_weight_list",format="ncol",directed=TRUE)


find_pair = function(i) {
    print(paste('=======', i, '======='))
    print(actors[i])
    ori_idx = indices[i]
    gra_idx = ori_to_gra[[ori_idx]]
    neis = neighbors(g, as.numeric(gra_idx), mode = c("out"))
    print(paste('neighbors: ', length(neis)))
    max_weight = 0
    res_gra_idx = -1
    for (nei in neis) {
        edge_id = get.edge.ids(g, c(as.numeric(gra_idx), nei), directed = TRUE, error = TRUE, multi = TRUE)
        curWei = edge_attr(g, "weight", index = edge_id)
        if (curWei > max_weight) {
            max_weight = curWei
            res_gra_idx = nei
        }
    }
    res_ori_idx = gra_to_ori[[as.character(res_gra_idx)]]
    print(idx_to_name[[res_ori_idx]])
    print(paste('weight: ', max_weight))
    print('')
}


actors = c('Tom Cruise', 'Emma Watson (II)', 'George Clooney', 'Tom Hanks', 'Dwayne Johnson (I)', 'Johnny Depp', 'Will Smith (I)', 'Meryl Streep', 'Leonardo DiCaprio', 'Brad Pitt')
indices = c('14499', '111286', '12808', '27252', '32383', '16873', '62765', '107820', '17280', '53241')

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


for (i in c(1:length(actors))) {
    find_pair(i)
}

