
# =================== Q11 ====================

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

print('dicts initialized')



movies = c('Batman v Superman: Dawn of Justice (2016)', 'Mission: Impossible - Rogue Nation (2015)', 'Minions (2015)')
indices = c('10114', '38485', '77924')

idx_to_movie = hash()
f1 <- file("preprocess_data/index2movie", "r")
index = 0
line = readLines(f1, n = 1)
while (length(line) != 0) {
    .set(idx_to_movie, index, substr(line, start = 11, stop = nchar(line)))
    line = readLines(f1, n = 1)
    index = index + 1
}

ori_to_gra = hash()
gra_to_ori = hash()
f2 <- file("preprocess_data/movie_idx_ori2gra", "r")
line = readLines(f2, n = 1)
while (length(line) != 0) {
    arr = strsplit(line, "[ ]")
    .set(ori_to_gra, arr[[1]][1], arr[[1]][2])
    .set(gra_to_ori, arr[[1]][2], arr[[1]][1])
    line = readLines(f2, n = 1)
}


find_neighbor = function(i) {
    print(paste('=======', i, '======='))
    print(movies[i])
    ori_idx = indices[i]
    gra_idx = ori_to_gra[[ori_idx]]
    neis = neighbors(g, as.numeric(gra_idx), mode = c("out"))
    print(paste('neighbors: ', length(neis)))
    
    pre_max_weight = Inf
    res_gra_idx = c()
    max_weights = c()
    
    for (i in c(1:5)) {
        cur_max_weight = 0
        cur_nei = -1
        for (nei in neis) {
            edge_id = get.edge.ids(g, c(as.numeric(gra_idx), nei), directed = FALSE, error = TRUE, multi = TRUE)
            curWei = edge_attr(g, "weight", index = edge_id)
            if ((curWei > cur_max_weight) & (curWei <= pre_max_weight) & (!(nei %in% res_gra_idx))) {
                cur_max_weight = curWei
                cur_nei = nei
            }
        }
        res_gra_idx = c(res_gra_idx, cur_nei)
        max_weights = c(max_weights, cur_max_weight)
        pre_max_weight = cur_max_weight
    }

    res_ori_idx = c()
    for (graidx in res_gra_idx) {
        res_ori_idx = c(res_ori_idx, gra_to_ori[[as.character(graidx)]])
    }
    for (oriidx in res_ori_idx) {
        print(idx_to_mov[[oriidx]])
    }
    return (res_ori_idx)
}



movies = c('Batman v Superman: Dawn of Justice (2016)', 'Mission: Impossible - Rogue Nation (2015)', 'Minions (2015)')
indices = c('10114', '38485', '77924')
for (i in c(1:3)) {
    res_ori_idx = find_neighbor(i)
    for (oriidx in res_ori_idx) {
        cur_communityId = -1
        for (communityIndex in 1:length(fg)){
            if (oriidx %in% fg[[communityIndex]]){
              cur_communityId = communityIndex
              break
            }
        }
        print(paste('movie:', idx_to_mov[[oriidx]], 'communityID:', cur_communityId))
        print('')
    }
}


