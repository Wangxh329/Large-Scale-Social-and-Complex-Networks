
# coding: utf-8

# In[1]:


# =================== Q3 preprocessing ====================
# get a mapping from original index to graph index

ori_gra = {}
cur_index = 1
with open('preprocess_data/edge_weight_list', 'r') as f:
    for line in f:
        arr = line.split(' ')
        for i in range(2):
            if not arr[i] in ori_gra:
                ori_gra[arr[i]] = cur_index
                cur_index += 1

with open('preprocess_data/idx_ori2gra', 'w') as f2:
    for k in ori_gra:
        f2.write(str(k) + ' ' + str(ori_gra[k]) + '\n')
        

