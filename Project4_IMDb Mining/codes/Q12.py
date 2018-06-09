import numpy as np
import pandas as pd
import os
os.chdir('/Users/Ray/Desktop/ECE232e/Project4/partIIdata')

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


list3 = []
list4 = []
list5 = []
# for the memory constraint, we use the top 10000+3 target
with open('edge_weight2.txt', 'r') as f2:
    for line in f2:
        arr = line.split(' ')
        if int(line.split(' ')[0]) < 10000:
            #break
            list3.append(int(arr[0]))
            list4.append(int(arr[1]))
            list5.append(float(arr[2]))
        else:
            if (int(line.split(' ')[0]) == 10114) or (int(line.split(' ')[0]) == 38485) \
                    or (int(line.split(' ')[0]) == 77924):
                list3.append(int(arr[0]))
                list4.append(int(arr[1]))
                list5.append(float(arr[2]))
            if int(line.split(' ')[0]) > 80000:
                break
        print(int(line.split(' ')[0]))
f2.close()

d = {'target': list3, 'compare': list4, 'relation': list5}
df = pd.DataFrame(data=d)

list4_new=[]
list3_new=[]
list5_new=[]
for i in range(len(list4)):
    if (list4[i] < 10000) or (list4[i] == 10114) \
            or (list4[i] == 38485) or (list4[i] == 77924):
        list4_new.append(list4[i])
        list3_new.append(list3[i])
        list5_new.append(list5[i])
    else:
        continue
    pass
d = {'target': list4_new, 'compare': list3_new, 'relation': list5_new}
df_temp = pd.DataFrame(data=d)
df = pd.concat([df, df_temp])

#d = pd.DataFrame(0, index=np.arrange(len(data)), columns=['target', 'compare', 'relation'])
#df.to_pickle('12targetDf')
#df = pd.DataFrame(np.zeros((3, 11725202)))
#df = pd.read_pickle('12targetDf')

rating_mat = df.pivot(index='target', columns='compare', values='relation')

del list3, list4, list5, list3_new, list4_new, list5_new
del  df

# take the rate
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

del list6, list7
del f2, f3
#There are 2050 no ratings, so remove the rows
rating_mat = rating_mat.drop(rating_mat.index[np.ndarray.tolist(np.where(np.array(list8)==0)[0])])
list9= [x for x in list8 if x != 0]

#Add the last three desired to the end of list
list9.append(6.6)
list9.append(7.4)
list9.append(6.4)

#rating_mat.to_pickle('12ratingMatDf')
#rating_mat = pd.DataFrame(np.zeros((7953, 189555)))
#rating_mat= pd.read_pickle('12targetDf')

#find the top related index
score_mat = np.zeros((7953,5))
rel_mat = np.zeros((7953,5))


for i in range(0, 7953):
    count = 0
    for j in range(40):#big number
        idx = np.argmax(rating_mat.iloc[i,0:10000])
        relation = np.max(rating_mat.iloc[i,0:10000])
        rating_mat.iloc[i, idx] = 0
        if list8[idx] > 0:
            score_mat[i, count] = list8[idx]
            rel_mat[i,count] = relation
            count +=1
            #print(idx)
        if count == 5:
            print(i)
            break

y_train = list9[:-3]
y_test = list9[-3:]

X_train = score_mat[:-3] #* rel_mat[:6500]
X_test = score_mat[-3:] #* rel_mat[6500:]
from sklearn.linear_model import LinearRegression
lr = LinearRegression()
addweight = np.sum(rel_mat[:-3],axis=1)
lr.fit(X_train, y_train, addweight)
y_pred_train = lr.predict(X_train)#test
RMSE = np.sqrt(np.average(np.square(y_train - y_pred_train)))#test

y_pred = lr.predict(X_test)
print(y_pred[-3:])
print('test RMSE is ',np.sqrt(np.average(np.square(y_test- y_pred))))

