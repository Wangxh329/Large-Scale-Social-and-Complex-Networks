
# coding: utf-8

# In[16]:


import numpy as np
with open('dataset/san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv', 'r') as f1, open('dataset/deletecol', 'w') as f2:
    for line in f1:
        arr = line.split(',')
        if arr[2] == '12':
            f2.write(arr[0] + ' ' + arr[1] + ' ' + arr[3] + '\n')
        


# In[25]:


path_time = {}
with open('dataset/deletecol', 'r') as f3:
    for line in f3:
        arr = line.split(' ')
        key1 = (arr[0], arr[1])
        key2 = (arr[1], arr[0])
        if key1 in path_time:
            path_time[key1].append(float(arr[2]))
        elif key2 in path_time:
            path_time[key2].append(float(arr[2]))
        else:
            path_time[key1] = [float(arr[2])]
            
with open('dataset/edge_weight', 'w') as f4:
    for k in path_time:
        avg_time = np.mean(path_time[k])
        f4.write(str(k[0]) + ' ' + str(k[1]) + ' ' + str(avg_time) + '\n')
    
    

