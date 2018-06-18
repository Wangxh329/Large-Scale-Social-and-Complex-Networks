import os
import pandas as pd
import math
import numpy as np

directory = os.path.abspath("finance_data/finance_data/data")
#print(directory)
d_r = {}
for file in os.listdir(directory):
    if (file[-4:] != '.csv'): continue
    file_name = file[:-4]
    file = directory + '/' + file
    print(file)
    df = pd.read_csv(file)
    arr_d = df['Date']
    arr_p = df['Close']
    arr_r = []
    if len(arr_p) != 765:
        continue
    for i in range(1, 765):
        if pd.Timestamp(arr_d[i]).weekday() == 0:
            q = (arr_p[i] - arr_p[i-1])/arr_p[i-1]
            r = math.log(1+q)
            arr_r += [r]
    d_r[file_name] = arr_r

file = open(r"weight2.txt", "w")
file_names = list(d_r.keys()) #list
print(len(file_names))
for i in range(len(file_names)):
    for j in range(i+1, len(file_names)):
        r_i = np.array(d_r[file_names[i]])
        r_j = np.array(d_r[file_names[j]])
        n = len(r_i)
        avg_r_i = sum(r_i)/n
        avg_r_j = sum(r_j)/n
        ro_i_j = (np.inner(r_i, r_j)/n - avg_r_i*avg_r_j)/math.sqrt((np.inner(r_i, r_i)/n-pow(avg_r_i,2))*(np.inner(r_j, r_j)/n-pow(avg_r_j,2)))
        w_i_j = math.sqrt(2*(1-ro_i_j))
        if w_i_j != 0:
            file.write('{} {} {}\n'.format(file_names[i], file_names[j], w_i_j))

file.close()