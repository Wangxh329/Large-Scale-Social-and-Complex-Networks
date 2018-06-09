
# coding: utf-8

# In[1]:


# Q1: preprocessing

raw_data = ['project4_data/actor_movies.txt', 'project4_data/actress_movies.txt']
with open('preprocess_data/merge.txt', 'w') as f1:
    for data in raw_data:
        with open(data, 'r', encoding='cp1252') as f2:
            for line in f2:
                f1.write(line)

movies = set()
names = set()
with open('preprocess_data/trim.txt', 'w') as f3, open('preprocess_data/merge.txt', 'r') as f4:
    for line in f4:
        arr = line.strip().split('\t\t')
        if len(arr) > 10:
            name = arr[0]
            names.add(name)
            curLine = arr[0].strip()
            for index in range(1, len(arr), 1):
                movie = arr[index].split('(')[0].strip()
                curLine += '\t\t' + movie
                movies.add(movie)
            f3.write(curLine + '\n')

print('number of actors & actresses: ' + str(len(names)))
print('number of movies: ' + str(len(movies)))


# In[52]:


# preprocessing for Q2
# get edge list and weight list

node_movies = {}
with open('preprocess_data/trim.txt', 'r') as f1:
    for line in f1:
        name_movies = line.split('\t\t')
        movies = set()
        for index in range(1, len(name_movies), 1):
            movies.add(name_movies[index])
        node_movies[name_movies[0]] = movies

name_list = []
movie_list = []
index = 0
with open('preprocess_data/index_name_list', 'w') as f2:
    for k in node_movies:
        f2.write(str(index).ljust(10) + k + '\n')
        name_list.append(k)
        movie_list.append(node_movies[k])
        index += 1

with open('preprocess_data/edge_weight_list', 'w') as f3:
    for i in range(len(name_list)):
        for j in range(len(name_list)):
            if i != j:
                intersection = movie_list[i] & movie_list[j]
                curWeight = len(intersection) / len(movie_list[i])
                if curWeight > 1:
                    raise ValueError('Weight > 1!')
                if curWeight > 0:
                    f3.write(str(i) + ' ' + str(j) + ' ' + str(curWeight) + '\n')

