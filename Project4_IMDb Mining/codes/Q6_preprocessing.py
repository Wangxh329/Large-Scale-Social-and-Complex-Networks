
# coding: utf-8

# In[2]:


# using trimmed data to generate a map<movie, set(actor)>

movie2actors = {}
with open('preprocess_data/trim', 'r') as f1:
    for line in f1:
        arr = line.split('\t\t')
        actor = arr[0]
        for i in range(1, len(arr)):
            if not arr[i] in movie2actors:
                movie2actors[arr[i]] = set()
            movie2actors[arr[i]].add(actor)

less_than_5 = []
for k in movie2actors:
    if len(movie2actors[k]) < 5:
        less_than_5.append(k)

for k in less_than_5:
    movie2actors.pop(k)

movie_list = []
actors_list = []
index = 0
with open('preprocess_data/index2movie', 'w') as f2:
    for k in movie2actors:
        f2.write(str(index).ljust(10) + k + '\n')
        movie_list.append(k)
        actors_list.append(movie2actors[k])
        index += 1

with open('preprocess_data/edge_weight2', 'w') as f3:
    for i in range(len(movie_list)):
        for j in range(i + 1, len(movie_list)):
            intersection = actors_list[i] & actors_list[j]
            union = actors_list[i] | actors_list[j]
            curWeight = len(intersection) / len(union)
            if curWeight > 1:
                raise ValueError('Weight > 1!')
            if curWeight > 0:
                f3.write(str(i) + ' ' + str(j) + ' ' + str(curWeight) + '\n')


# In[ ]:


# mapping: original index -> graph index
ori_gra = {}
cur_index = 1
with open('preprocess_data/edge_weight2', 'r') as f:
    for line in f:
        arr = line.split(' ')
        for i in range(2):
            if not arr[i] in ori_gra:
                ori_gra[arr[i]] = cur_index
                cur_index += 1

with open('preprocess_data/movie_idx_ori2gra', 'w') as f2:
    for k in ori_gra:
        f2.write(str(k) + ' ' + str(ori_gra[k]) + '\n')

