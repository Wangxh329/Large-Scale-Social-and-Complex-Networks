
# coding: utf-8

# In[1]:


# =================== Q4 & Q5 analysis ====================

node_movies = {}
with open('preprocess_data/trim', 'r') as f1:
    for line in f1:
        name_movies = line.split('\t\t')
        movies = set()
        for index in range(1, len(name_movies), 1):
            movies.add(name_movies[index])
        node_movies[name_movies[0]] = movies


# In[3]:


# Q4
sig_acts = ['Flowers, Bess', 'Tatasciore, Fred', 'Harris, Sam (II)', 'Blum, Steve (IX)', 'Miller, Harold (I)', 'Jeremy, Ron', 'Lowenthal, Yuri', 'Phelps, Lee (I)', 'Downes, Robin Atkin', 'O\'Connor, Frank (I)']

for i in range(10):
    sig_movies = node_movies[sig_acts[i]]
    print('=========' + str(i+1) + '==========')
    print(sig_acts[i] + ': ' + str(len(sig_movies)) + ' movies')
    print('')


# In[4]:


# Q5
input_acts = ['Cruise, Tom', 'Watson, Emma (II)', 'Clooney, George', 'Hanks, Tom', 'Johnson, Dwayne (I)', 'Depp, Johnny', 'Smith, Will (I)', 'Streep, Meryl', 'DiCaprio, Leonardo', 'Pitt, Brad']

for i in range(10):
    input_movies = node_movies[input_acts[i]]
    print('=========' + str(i+1) + '==========')
    print(input_acts[i] + ': ' + str(len(input_movies)) + ' movies')
    print('')

