import numpy as np
import pandas as pd
import os

os.chdir('/Users/Ray/Desktop/ECE232e/Project4/partIIdata')
movie2actors = {}
cnt = 0
with open('trim.txt', 'r') as f1:
    for line in f1:
        arr = line.split('\t\t')
        actor = arr[0]
        cnt +=1
        for i in range(1, len(arr)):
            if not arr[i] in movie2actors:
                movie2actors[arr[i]] = set()
            movie2actors[arr[i]].add(actor)
        print(cnt)

less_than_5 = []
for k in movie2actors:
    if len(movie2actors[k]) < 5:
        less_than_5.append(k)


for k in less_than_5:
    movie2actors.pop(k)


actors_set = []
actors_list = []

movie_list = []
index = 0
for k in movie2actors:
    actors_set.append(movie2actors[k])
    movie_list.append(k) #202875
    index += 1
    print(index)
    #if index == 2:
        #break

#turn set to list and also eliminate replication
for z in range(len(actors_set)):
    for j in actors_set[z]:
        actors_list.append(j)#####get set element
actors_list_uni = list(np.unique(actors_list))  #112354


index = 0
with open('index2actor.txt', 'w') as f2:
    for k in range(len(actors_list_uni)):
        f2.write(str(index) + ' ' + actors_list_uni[k] + '\n')
        index += 1
f2.close()


# there are 112354 actors
# find the actor index from 'index2actor.txt'
target_movie = []
compare_actor = []
for k in range(len(movie_list)):
    for j in (actors_set[k]):
        for idx in range(len(actors_list_uni)):
            if actors_list_uni[idx] == j:
                #print(idx)
                break
        target_movie.append(k)
        compare_actor.append(idx)
    print(k)

'''
with open('movVact.txt', 'w') as f3:
    for k in range(df_mov2act.shape[0]):
        f3.write(str(df_mov2act['mov'][k]) + ' ' + str(df_mov2act['actor'][k]) + '\n')
        #index += 1
f3.close()
'''

###############################################
cnt = 0
list_mov=[]
list_act=[]
#due to memory constraint, i take first 10000+3 movie
with open('movVact.txt', 'r') as f3:
    for line in f3:
        arr = line.split(' ')
        if int(arr[0]) < 10000:
            list_mov.append(int(arr[0]))
            list_act.append(int(arr[1]))#len = 2592117
            cnt +=1
        else:
            if (int(arr[0]) == 10114) or (int(arr[0]) == 38485) \
                    or (int(arr[0]) == 77924):
                list_mov.append(int(arr[0]))
                list_act.append(int(arr[1]))  # len = 2592117
                cnt += 1
            if int(arr[0]) > 80000:
                break
        print(cnt)

temp = [1]*len(list_act)#add 1 into new column
d = {'mov': list_mov, 'actor': list_act, 'val': temp}
df_mov2act = pd.DataFrame(data=d)
mov2act_mat = df_mov2act.pivot(index='mov', columns='actor', values='val')
act_weight = np.sum(mov2act_mat)
#mov2act_comp_mat = np.zeros((202874, 112353))

'''
#load index2movie
list1 = []
list2 = []
with open('index2movie.txt', 'r') as f1:
    for line in f1:
        arr = line.split(' ' * (10 - len(line.split(' ')[0])))
        if arr[0] == '\n':
            continue
        else:
            list1.append(int(arr[0]))
            list2.append(arr[1][:-1])
    pass
d = {'index': list1, 'name': list2}
index2movie = pd.DataFrame(data=d)
f1.close()
del list1, list2, f1
'''
#load rating
list6=[]
list7=[]
with open('movie_rating.txt', 'r',encoding='cp1252') as f3:
    for line in f3:
        arr = line.split('\t\t')
        #if arr[0] == '\n':
            #continue
        #else:
        list6.append(arr[0])
        list7.append(float(arr[1]))
    pass
#list6 = name, list7 = rate
# number->name->rate
d = {'name': list6, 'rating': list7}
df_rate = pd.DataFrame(data=d)
#name->rate
list8 = []#rating in the form of sorting index
for i in range(10000):
    if np.where(df_rate['name']==index2movie['name'][i])[0].shape[0]>0 :
        list8.append(df_rate['rating']
                     [np.where(df_rate['name']==index2movie['name'][i])[0][0]])
    else:
        list8.append(0)
    print(i)

#drop no rating movie
mov2act_mat= mov2act_mat.drop(mov2act_mat.index[np.ndarray.tolist(np.where(np.array(list8)==0)[0])])
list9= [x for x in list8 if x != 0]
#Add the last three desired to the end of list
list9.append(6.6)
list9.append(7.4)
list9.append(6.4)
act_weight = list(np.sum(mov2act_mat))#actor weight


#remove column which actor weight = 0, get new weight
mov2act_mat_new = mov2act_mat.iloc[:, (np.where(np.array(act_weight)!=0)[0])]
act_weight_new = [x for x in act_weight if x != 0]


#training
#mov2act_mat_new_sub = mov2act_mat_new.iloc[:10,:]
mov2act_mat_new_Tr = mov2act_mat_new.iloc[:-3, :]
ratXmat_Tr = mov2act_mat_new_Tr.mul(list9[:-3], axis=0)
rat_act_Tr = list(np.mean(ratXmat_Tr))

feature_raw = mov2act_mat_new.mul(rat_act_Tr,axis=1)

#compute movie weight average
feature_rawXwght = feature_raw.mul(act_weight_new,axis=1)
temp = list(np.sum(feature_rawXwght, axis=1))
mov2actXwght = mov2act_mat_new.mul(act_weight_new,axis=1)
temp1 = list(np.sum(mov2actXwght, axis=1))
#del feature_rawXwght, mov2actXwght
mov_weightAVG = list(np.asarray(temp)/np.asarray(temp1))

feature_5 = np.zeros((7953,5))
feature_5[:,0] = mov_weightAVG
for i in range(feature_raw.shape[0]):
    feature_5[i, 1] = np.nanpercentile(feature_raw.iloc[i, :], 25)
    feature_5[i, 2] = np.nanpercentile(feature_raw.iloc[i, :], 50)
    feature_5[i, 3] = np.nanpercentile(feature_raw.iloc[i, :], 75)
    feature_5[i, 4] = np.nanvar(feature_raw.iloc[i, :])
    print(i)

y_train = list9[:-3]
y_test = list9[-3:]

X_train = feature_5[:-3]
X_test = feature_5[-3:]
from sklearn.linear_model import LinearRegression
lr = LinearRegression()
lr.fit(X_train, y_train)
y_pred_train = lr.predict(X_train)
RMSE = np.sqrt(np.average(np.square(y_train - y_pred_train)))#test
print('train RMSE is ', RMSE)
#test
y_pred = lr.predict(X_test)
print(y_pred[-3:])
print('test RMSE is ',np.sqrt(np.average(np.square(y_test- y_pred))))

###########################################create graph
cnt = 0
list_mov=[]
list_act=[]
with open('movVact.txt', 'r') as f3:
    for line in f3:
        arr = line.split(' ')
        list_mov.append(int(arr[0]))
        list_act.append(int(arr[1]))#len = 2592117
        cnt += 1
        print(cnt)


list_act1 = [x+500000 for x in list_act]
with open('movVact_mod.txt', 'w') as f6:
    for k in range(len(list_mov)):
        f6.write(str(list_mov[k]) + ' ' + str(list_act1[k]) + '\n')
        #index += 1
f6.close()
