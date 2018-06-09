library("igraph")
options(scipen=999)

#Read data
edgelist = read.csv("edge_weight2", sep=" ")

index2movie_data = readLines("index2movie")
index2movie = list()
for (line in index2movie_data){
  if (line != ""){
    index = toString(as.numeric(substring(line, 1, 10)))
    movie = substring(line, 11)
    index2movie[[index]] = movie
  }
}

movie_idx_ori2gra = readLines("movie_idx_ori2gra")
movie_idx_ori2gra = strsplit(movie_idx_ori2gra, " ")
names(movie_idx_ori2gra) = sapply(movie_idx_ori2gra, function(x) as.numeric(x[[1]]))
movie_idx_ori2gra = lapply(movie_idx_ori2gra, function(x) as.numeric(x[-1]))

trim = readLines("trim")
trim = gsub("\t\t", "\t", trim)
trim = strsplit(trim, "\t")
names(trim) = sapply(trim, "[[", 1)
trim = lapply(trim, "[", -1)

movie2actor = list()
for (actor in ls(trim)){
  for (movie in trim[[actor]]){
    if (movie %in% names(movie2actor)){
      movie2actor[[movie]] = c(movie2actor[[movie]], actor)
    }
    else{
      movie2actor[[movie]] = c(actor)
    }
  }
}

genreData = readLines("movie_genre_utf-8")
genreData = gsub("\t\t", "\t", genreData)
genreData = strsplit(genreData, "\t")
names(genreData) = sapply(genreData, "[[", 1)
genreData = lapply(genreData, "[", -1)

ratingData = readLines("movie_rating.txt")
ratingData = gsub("\t\t", "\t", ratingData)
ratingData = strsplit(ratingData, "\t")
names(ratingData) = sapply(ratingData, "[[", 1)
ratingData = lapply(ratingData, function(x) as.double(x[-1]))

#Create communities
g = read.graph("edge_weight2", format="ncol", directed=FALSE)
communities = cluster_fast_greedy(g)

#Obtain genre fractions of whole data
fracWhole = list()
for (movie in ls(genreData)){
  genre = genreData[[movie]]
  if (genre %in% names(fracWhole)) fracWhole[[genre]] = fracWhole[[genre]] + 1
  else fracWhole[[genre]] = 1
}

#8(a) & 8(b)
failed = 0
dominantGenres = vector()
dominantGenresModified = vector()
for (communityIndex in 1:length(communities)){
  communityGenre = list()
  for (vertexIndex in communities[[communityIndex]]){
    movie = index2movie[[vertexIndex]]
    if (movie %in% names(genreData)){
      genre = genreData[[movie]]
    }
    else{
      failed = failed + 1
      genre = 1
    }
    if (genre %in% names(communityGenre)){
      communityGenre[[genre]] = communityGenre[[genre]] + 1
    }sizes
    else{
      communityGenre[[genre]] = 1
    }
  }
  dominantGenres = c(dominantGenres, names(communityGenre)[order(-unlist(communityGenre))[1]])
  genreScore = list()
  for (genre in names(communityGenre)){
    if (genre != "") genreScore[[genre]] = log(communityGenre[[genre]]) * (communityGenre[[genre]] / length(communities[[communityIndex]])) / fracWhole[[genre]]
  }
  dominantGenresModified = c(dominantGenresModified, names(genreScore)[order(-unlist(genreScore))[1]])
} 

#8(c)
communites8cIndex = 21
communites8c = communities[[communites8cIndex]]
for (communityIndex in 1:length(communities)){
  if (length(communities[[communityIndex]]) >= 10 && length(communities[[communityIndex]]) <= 20){
    communites8c = communities[[communityIndex]]
    communites8cIndex = communityIndex
    break
  }
}
bipartiteMovies = c()
bipartiteActors = c("Dasz, Steven", "Joiner, Craig", "McKay, Reuben", "Moir, Shaun", "Noble, Graeme",
                    "Noble, John-William", "Sandison, Martin", "Taylor, Stuart (X)", "Chan, Juju", "Kilpatrick, Kayleigh",
                    "Marshall, Scarlett", "McKay, Hannah", "Hislop, Tom", "Simpson, Julia (II)")
bipartiteEdgeList = c(1, 19, 1, 20, 1, 21, 1, 22, 1, 23, 1, 24, 1, 25, 1, 26, 1, 27, 1, 28, 1, 29, 1, 30,
                      2, 31, 2, 21, 2, 24, 2, 28, 2, 30, 3, 31, 3, 20, 3, 21, 3, 22, 3, 23, 3, 24, 3, 25,
                      3, 26, 3, 30, 3, 32, 4, 31, 4, 20, 4, 21, 4, 22, 4, 23, 4, 24, 4, 25, 4, 26, 4, 28,
                      4, 30, 5, 31, 5, 21, 5, 23, 5, 28, 5, 30, 5, 32, 6, 31, 6, 20, 6, 21, 6, 23, 6, 24,
                      6, 26, 6, 28, 6, 30, 6, 32, 7, 31, 7, 21, 7, 24, 7, 26, 7, 28, 7, 30, 8, 31, 8, 21,
                      8, 22, 8, 23, 8, 24, 8, 25, 8, 26, 8, 28, 8, 30, 8, 32, 9, 31, 9, 20, 9, 21, 9, 23,
                      9, 24, 9, 25, 9, 26, 9, 28, 9, 30, 9, 32, 10, 20, 10, 21, 10, 23, 10, 24, 10 ,25, 10, 26,
                      11, 20, 11, 21, 11, 22, 11, 23, 11, 24, 11, 32, 12, 20, 12, 21, 12, 22, 12, 23, 12, 24, 12, 25,
                      12, 26, 12, 30, 13, 20, 13, 21, 13, 22, 13, 24, 13, 25, 13, 28, 13, 32, 14, 20, 14, 21, 14, 23,
                      14, 24, 14, 28, 14, 32, 15, 20, 15, 21, 15, 23, 15, 24, 15, 25, 16, 31, 16, 20, 16, 21, 16, 22,
                      16, 23, 16, 24, 16, 25, 16, 26, 16, 28, 16, 30, 16, 32, 17, 20, 17, 21, 17, 23, 17, 24, 17, 25,
                      17, 28, 18, 21, 18, 22, 18, 23, 18, 24, 18, 32)
counter = rep(0, 35)
for (num in bipartiteEdgeList){
  counter[num] = counter[num] + 1
}
bipartiteEdgeList = matrix(bipartiteEdgeList, nc=2, byrow=TRUE)

bipartiteDict = list()
bipartiteEdgeList = c()
index = 1
for (movieId in communites8c){
  movie = index2movie[[movieId]]
  bipartite[[movie]] = index
  index = index + 1
}
for (movieId in communites8c){
  movie = index2movie[[movieId]]
  movieId = bipartiteEdgeList[[movie]]
  for (actor in movie2actor[[movie]]){
    if (actor %in% names(bipartiteDict)){
      actorId = bipartiteDict[[actor]]
    }
    else{
      bipartiteDict[[actor]] = index
      actorId = index
      index = index + 1
    }
    bipartiteEdgeList = c(bipartiteEdgeList, movieId, actorId)
  }
}
bipartiteEdgeList = matrix(bipartiteEdgeList, nc=2, byrow=TRUE)

bipartite = graph_from_edgelist(bipartiteEdgeList, directed=FALSE)
plot(bipartite)

# #9
# movieArray = list("Batman v Superman: Dawn of Justice (2016)"=10114, "Mission: Impossible - Rogue Nation (2015)"=38485, "Minions (2015)"=77924)

# for (movie in ls(movieArray)){
#   originId = movieArray[[movie]]
#   graphId = movie_idx_ori2gra[[originId]]
#   communityId = 0
#   for (communityIndex in 1:length(communities)){
#     if (graphId %in% communities[[communityIndex]]){
#       communityId = communityIndex
#       break
#     }
#   }
#   neighbors = neighbors(g, graphId)
#   avgRating = 0
#   ratings = c()
#   for (neighbor in neighbors){
#     avgRating = avgRating + ratingData[[neighbor]]
#     ratings = c(ratings, ratingData[[neighbor]])
#   }
#   avgRating = avgRating / length(neighbors)
  
#   hist(ratings)
#   write(neighbors, file="neighbor1", ncolumns = 1)
  
#   #10
#   avgCommunityRating = 0
#   numCommunityNeighbor = 0
#   communityRatings = c()
#   communityNeighbors = c()
#   for (neighbor in neighbors){
#     if (neighbor %in% communities[[communityId]]){
#       avgCommunityRating = avgCommunityRating + ratingData[[neighbor]]
#       numCommunityNeighbor = numCommunityNeighbor + 1
#       communityRatings = c(communityRatings, ratingData[[neighbor]])
#       communityNeighbors = c(communityNeighbors, neighbor)
#     }
#   }
#   avgCommunityRating = avgCommunityRating / numCommunityNeighbor
  
#   hist(communityRatings)
#   write(communityNeighbors, file="community_neighbor1", ncolumns = 1)
# }


# =================== Q9 ====================
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

mov_to_rating = hash()
f1 <- file("preprocess_data/movie_rating_utf-8", "r")
lines = readLines(f1)
for (line in lines) {
    if (nchar(line) > 1) {
        arr = strsplit(line, "[\t\t]")
        .set(mov_to_rating, arr[[1]][1], arr[[1]][3])
    }
}

movies = c('Batman v Superman: Dawn of Justice (2016)', 'Mission: Impossible - Rogue Nation (2015)', 'Minions (2015)')
indices = c('10114', '38485', '77924')
for (i in c(1:3)) {
    print(paste('=======', i, '======='))
    print(movies[i])
    ori_idx = indices[i]
    gra_idx = ori_to_gra[[ori_idx]]
    neis = neighbors(g, as.numeric(gra_idx))
    print(paste('neighbors: ', length(neis)))
    sum_rating = 0
    count = 0
    rating_count = hash()
    for (nei in neis) {
        tmp = gra_to_ori[[as.character(nei)]]
        movie = idx_to_mov[[tmp]]
        rating = mov_to_rating[[movie]]

        if (!is.null(rating)) {
            sum_rating = sum_rating + as.numeric(rating)
            count = count + 1
            if (is.null(rating_count[[rating]])) {
                .set(rating_count, rating, 0)
            }
            .set(rating_count, rating, rating_count[[rating]] + 1)
        }
    }
    print(paste("Average rating:", as.character(sum_rating/count)))
    
    key_list = c()
    val_list = c()
    for (k in ls(rating_count)) {
        key_list = c(key_list, k)
        val_list = c(val_list, rating_count[[k]])
    }
    
    df <- data.frame(rating=key_list, freq=val_list)
    # plot rating distribution histogram
    dis_plot <- ggplot(df, aes(x=rating, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    labs(title=movies[i], x="Rating", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    print(dis_plot)
}


# =================== Q10 ====================
movies = c('Batman v Superman: Dawn of Justice (2016)', 'Mission: Impossible - Rogue Nation (2015)', 'Minions (2015)')
indices = c('10114', '38485', '77924')
for (i in c(1:3)) {
    print(paste('=======', i, '======='))
    print(movies[i])
    ori_idx = indices[i]
    gra_idx = ori_to_gra[[ori_idx]]
    neis = neighbors(g, as.numeric(gra_idx))
    print(paste('neighbors: ', length(neis)))
    sum_rating = 0
    count = 0
    rating_count = hash()
    
    cur_communityId = -1
    for (communityIndex in 1:length(fg)){
        if (ori_idx %in% fg[[communityIndex]]){
          cur_communityId = communityIndex
          break
        }
    }
    print(paste('community:', cur_communityId))
    for (nei in neis) {
        nei_ori = gra_to_ori[[as.character(nei)]]
        nei_com = -1
        for (communityIndex in 1:length(fg)){
            if (nei_ori %in% fg[[communityIndex]]){
              nei_com = communityIndex
              break
            }
        }
        if (nei_com == cur_communityId) {
            movie = idx_to_mov[[as.character(gra_to_ori[[as.character(nei)]])]]
            rating = mov_to_rating[[movie]]
            if (!is.null(rating)) {
                sum_rating = sum_rating + as.numeric(rating)
                count = count + 1
                if (is.null(rating_count[[rating]])) {
                    .set(rating_count, rating, 0)
                }
                .set(rating_count, rating, rating_count[[rating]] + 1)
            }
        }
    }
    print(paste('average rating:', as.character(sum_rating/count)))
    key_list = c()
    val_list = c()
    for (k in ls(rating_count)) {
        key_list = c(key_list, k)
        val_list = c(val_list, rating_count[[k]])
    }
    df <- data.frame(genre=key_list, freq=val_list)
    # plot rating distribution histogram
    dis_plot <- ggplot(df, aes(x=genre, y=freq))+ 
    geom_bar(stat = "identity", fill="hotpink")+
    labs(title=movies[i], x="Rating", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    print(dis_plot)
}

