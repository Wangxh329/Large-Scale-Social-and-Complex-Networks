############################################################### Q 11
#from Q6 get GCC vertex
import os
os.chdir('/Users/Ray/Desktop/ECE232_HW5/2/dataset/preprocess')

import igraph as ig
g = ig.Graph.Read(f = 'preprocessed.txt', format = 'ncol', directed = False)
gcc = g.components().giant()
ig.summary(gcc)

vertexID_list = []
for i in range(len(gcc.vs)):
    vertexID_list.append(int(gcc.vs[i]['name']))

import json
import pandas as pd
import numpy as np
os.chdir('/Users/Ray/Desktop/ECE232_HW5')
with open('san_francisco_censustracts.json') as f:
    geoBound = json.load(f)

cood_mean_arr = np.zeros((len(geoBound['features']), 2))
# use mean coordinate to represent the node
for i in range(len(geoBound['features'])):
    cood_mean_arr[i, :] = np.mean(geoBound['features'][i]['geometry']['coordinates'][0][0], axis=0)

import matplotlib.pyplot as plt
from scipy.spatial import Delaunay
cood_mean_arr_gcc = cood_mean_arr[[x-1 for x in vertexID_list], :] #fit to cood_mean
tri = Delaunay(cood_mean_arr_gcc)
plt.triplot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], tri.simplices, linewidth=0.8)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', markersize=1)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Road Mesh from Delaunay Triangulation on Nodes in SF Bay Area')
#plt.xlim(-122.06, -122.19)
#plt.ylim(36.96, 37.45)
plt.show()

# delete duplicate edge
temp0 = tri.simplices #index based
temp1 = np.zeros_like(temp0)

#turn index back to node ID
for i in range(temp0.shape[0]):
    for j in range(temp0.shape[1]):
        temp1[i, j] = vertexID_list[temp0[i,j]]

temp2 = []
for i in range(temp1.shape[0]):
    temp2.append([temp1[i, 0], temp1[i, 1]])
    temp2.append([temp1[i, 1], temp1[i, 2]])
    temp2.append([temp1[i, 2], temp1[i, 0]])

edge_unique = []
for i in range(len(temp2)):
    if (temp2[i] in edge_unique) or (temp2[i][::-1] in edge_unique):
        continue
    else:
        edge_unique.append(temp2[i])
for i in range(len(edge_unique)):
    edge_unique.append(edge_unique[i][::-1])

#create graph with triangulation
SF_edge_data = pd.read_csv('san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv')
i_12s =[]
for i in range(SF_edge_data.shape[0]):
    if SF_edge_data.iloc[i, 2] == 12:
        i_12s.append(i)
SF_edge_data12 = SF_edge_data.iloc[i_12s, :]

i_s = []
for i in range(SF_edge_data12.shape[0]):
    if list(SF_edge_data12.iloc[i, 0:2]) in edge_unique:
        i_s.append(i)
    print(i)

#save i_s
'''
with open('indexPicked.txt', 'w') as f2:
    for k in range(len(i_s)):
        f2.write(str(i_s[k]) + '\n')
f2.close()

'''

#extract column we need only
SF_edge_data12_ext = SF_edge_data12[['sourceid', 'dstid', 'mean_travel_time']].copy()
SF_edge_12_time = SF_edge_data12_ext.iloc[i_s, :]

# total 1880 node, given edge only 1845 node
# have to find missing node/edges
temp = list(SF_edge_12_time['sourceid'])
temp2 = list(SF_edge_12_time['dstid'])
temp3 = list(set(temp) & set(temp2))

#create edge list for SF_edge_12_time
SF_edge_12_list=[]
for i in range(SF_edge_12_time.shape[0]):
    SF_edge_12_list.append(list(SF_edge_12_time.iloc[i, 0:2]))
    print(i)
#find edge with no time
temp_edgeNotime = []
for i in range(len(edge_unique)):
    if edge_unique[i] in SF_edge_12_list:
        continue
    else:
        temp_edgeNotime.append(edge_unique[i])
    print(i)

def findcood(nodeID, list):
    return (list[nodeID -1])

def findDist(cood1, cood2):
    return np.sqrt( np.sum( np.square((cood1 - cood2) * 69)))

dist_12 = []
for i in range(SF_edge_12_time.shape[0]):
    dist_12.append(findDist(findcood( SF_edge_12_time.iloc[i,0], cood_mean_arr),
                            findcood( SF_edge_12_time.iloc[i,1], cood_mean_arr)))
# put dis to SF_edge_12_time
se = pd.Series(dist_12)
SF_edge_12_time['dist'] = se.values

findcood(896, cood_mean_arr)
findcood(2017, cood_mean_arr)

'''
with open('sidDidTime_dist.txt', 'w') as f3:
    for k in range(SF_edge_12_time.shape[0]):
        f3.write(str(SF_edge_12_time.iloc[k,0]) + ' ' + str(SF_edge_12_time.iloc[k,1]) +' ' +
                    str(SF_edge_12_time.iloc[k, 2])+ ' ' + str(SF_edge_12_time.iloc[k, 3]) + '\n')
f3.close()
'''
'''
with open('sidDidTime_only.txt', 'w') as f4:
    for k in range(SF_edge_12_time.shape[0]):
        f4.write(str(SF_edge_12_time.iloc[k,0]) + ' ' + str(SF_edge_12_time.iloc[k,1]) +' ' +
                    str(SF_edge_12_time.iloc[k, 2]) + '\n')
f4.close()
'''
with open('edgeNotime.txt', 'w') as f5:
    for k in range(len(temp_edgeNotime)):
        f5.write(str(temp_edgeNotime[k][0]) + ' ' + str(temp_edgeNotime[k][1]) + '\n' )
f5.close()



############################################################### Q12 calculate car flow
#load data back from r
list1=[]
list2=[]
list3=[]
list4=[]
with open('edge_wTimeDist_11.txt', 'r') as f6:
    for line in f6:
        arr = line.split('\t')
        list1.append(int(arr[0]))
        list2.append(int(arr[1]))
        list3.append(float(arr[2]))
        list4.append(float(arr[3]))
f6.close()
d = {'A.sourceid': list1, 'B.dstid': list2, 'C.time': list3, 'distance': list4}
SF_edge_Q12 = pd.DataFrame(data=d)

def getcarflow(time, dist):
    velocity = (dist/time)*3600
    return 1/ (0.003/velocity + 1/1800) * 2
list_carFlow = []
list1=[]
list2=[]
for i in range(SF_edge_Q12.shape[0]):
    list_carFlow.append(getcarflow(SF_edge_Q12.iloc[i, 2], SF_edge_Q12.iloc[i, 3]))
    list1.append(SF_edge_Q12.iloc[i, 0])
    list2.append(SF_edge_Q12.iloc[i, 1])
d = {'A.sourceid': list1, 'B.dstid': list2, 'C.Flow': list_carFlow}
SF_edge_Q12_flow = pd.DataFrame(data=d)


###############################################################Q 13
with open('Q12flow.txt', 'w') as f11:
    for i in range(SF_edge_Q12_flow.shape[0]):
        f11.write(str(SF_edge_Q12_flow.iloc[i, 0])+ ' '
                  +str(SF_edge_Q12_flow.iloc[i, 1])+ ' ' +str(SF_edge_Q12_flow.iloc[i, 2])+ '\n' )
f11.close()

# from coord to ID (ID = index+1)
#check a point is in a polygon
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon
def cood2nodeID(geoBound, cood1, cood2):
    point = Point(cood1, cood2)
    for i in range(len(geoBound['features'])):
        points = np.asarray(geoBound['features'][i]['geometry']['coordinates'][0][0])
        polygon = Polygon(points)
        # print(polygon.contains(point))
        if polygon.contains(point) == 1:
            break
    return (int(geoBound['features'][i]['properties']['MOVEMENT_ID']))
cood2nodeID(geoBound, -122.1887333, 37.4269353)#Stanford ID = 2607
cood2nodeID(geoBound, -122.0748253, 36.9731906)#UCSC ID = 1968

#draw the road by edges we got
for i in range(SF_edge_Q12.shape[0]):
    src_id = SF_edge_Q12['A.sourceid'][i]
    dst_id = SF_edge_Q12['B.dstid'][i]
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=1)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Road Mesh for SF bay area')
plt.show()

#plot 1, 2(zoom)
plt.plot(-122.17598222,   37.42968589, 'o', color='red')
plt.plot(-122.06456181,   36.97416881, 'o', color='black')
for i in range(SF_edge_Q12.shape[0]):
    src_id = int(SF_edge_Q12.iloc[i, 0])
    dst_id = int(SF_edge_Q12.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=2)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Road Mesh for Stanford(red), UCSC(black) (zoom in)')
plt.xlim(-122.30, -121.7)
plt.ylim(36.90, 37.47)
plt.show()

#plot3
plt.plot(-122.17598222,   37.42968589, 'o', color = 'red')
for i in range(SF_edge_Q12.shape[0]):
    src_id = int(SF_edge_Q12.iloc[i, 0])
    dst_id = int(SF_edge_Q12.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=2)
plt.xlim(-122.19, -122.11)
plt.ylim(37.38, 37.47)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Road Mesh near Stanford(red) (zoom in)')
plt.show()


#plot 4
plt.plot(-122.06456181,   36.97416881, 'o', color = 'black')
for i in range(SF_edge_Q12.shape[0]):
    src_id = int(SF_edge_Q12.iloc[i, 0])
    dst_id = int(SF_edge_Q12.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color='green', markersize=2)
plt.xlim(-122.10, -122.02)
plt.ylim(36.93, 37.01)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Road Mesh near UCSC(black) (zoom in)')
plt.show()


###############################################################Q14 plot multiple edge on the graph
point = Point(-122.479, 37.83) # 260.11sec, 332.08 sec
point = Point(-122.387, 37.93)#2121 to 1653, 454.32sec, 423.75 sec

#Golden Gate Bridge: [1292, 950]
cood2nodeID(geoBound, -122.475, 37.806)
cood2nodeID(geoBound, -122.479, 37.83)

#Richmond, San Rafael Bridge: [2121, 1653]
cood2nodeID(geoBound, -122.501, 37.956)
cood2nodeID(geoBound, -122.387, 37.93)

#San Mateo Bridge: [2247, 2236]
cood2nodeID(geoBound, -122.273, 37.563)
cood2nodeID(geoBound, -122.122, 37.627)

#Dambarton Bridge: [1473, 2686]
cood2nodeID(geoBound, -122.142, 37.486)
cood2nodeID(geoBound, -122.067, 37.54)

#San Francisco - Oakland Bay Bridge: [2308, 285]
cood2nodeID(geoBound, -122.388, 37.788)
cood2nodeID(geoBound, -122.302, 37.825)

findDist(np.asarray([-122.388, 37.788]), np.asarray([-122.302, 37.825]))
temp1 = [1292, 950] # 260.11sec, 332.08 sec
temp2 = [2121, 1653]# 454.32, 423.75 sec
temp3 = [2247, 2236]# 581.54, 575.08
temp4 = [1473, 2686]# 529.8, 790.67
temp5 = [2308, 285] # 493.36, 618.5
#find if there's a direct edge between two points
def findtimeof2ID(SF_edge_Q12, temp):
    for i in range(SF_edge_Q12.shape[0]):
        if SF_edge_Q12.iloc[i, 0] == temp[0]:
            if SF_edge_Q12.iloc[i, 1] == temp[1]:
                break
    time1 = SF_edge_Q12.iloc[i,2]
    for i in range(SF_edge_Q12.shape[0]):
        if SF_edge_Q12.iloc[i, 0] == temp[1]:
            if SF_edge_Q12.iloc[i, 1] == temp[0]:
                break
    time2 = SF_edge_Q12.iloc[i,2]
    return [time1, time2]
findtimeof2ID(SF_edge_Q12, temp5)

# see there are some long edge on G, that should be fake edge
# set threshold on time to get of the edge
time_threshold = 750
i_thrs = []
for i in range(SF_edge_Q12.shape[0]):
    if SF_edge_Q12.iloc[i,2] < time_threshold:
        i_thrs.append(i)

SF_edge_Q12_pick = SF_edge_Q12.iloc[i_thrs,:]

#Plot
# get the mean coordinate of bridge two point
(np.asarray([-122.475, 37.806]) + np.asarray([-122.479, 37.83]))/2
#Richmond, San Rafael Bridge: [2121, 1653]
(np.asarray([-122.501, 37.956]) + np.asarray([-122.387, 37.93]))/2

#San Mateo Bridge: [2247, 2236]
(np.asarray([-122.273, 37.563]) + np.asarray([-122.122, 37.627]))/2

#Dambarton Bridge: [1473, 2686]
(np.asarray([-122.142, 37.486]) + np.asarray([-122.067, 37.54]))/2

#San Francisco - Oakland Bay Bridge: [2308, 285]
(np.asarray([-122.388, 37.788]) + np.asarray([-122.302, 37.825]))/2

plt.plot(-122.477,  37.818, 'o', color='red', markersize=3)
plt.plot(-122.444,  37.933, 'o', color='red', markersize=3)
plt.plot(-122.1975, 37.585, 'o', color='red', markersize=3)
plt.plot(-122.1055, 37.503 , 'o', color='red', markersize=3)
plt.plot(-122.345 , 37.8065, 'o', color='red', markersize=3)
for i in range(SF_edge_Q12_pick.shape[0]):
    src_id = int(SF_edge_Q12_pick.iloc[i, 0])
    dst_id = int(SF_edge_Q12_pick.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=1.5)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Defoliated Graph and the Five Bridges(Red)(zoom in)')
plt.xlim(-122.50, -121.5)
plt.ylim(37.00, 38.00)
plt.show()

############################################################### Q15
#fix SF_edge_Q12_flow
time_threshold = 750
i_thrs = []
for i in range(SF_edge_Q12.shape[0]):
    if SF_edge_Q12.iloc[i,2] < time_threshold:
        i_thrs.append(i)

SF_edge_Q12_flow_pick = SF_edge_Q12_flow.iloc[i_thrs,:]
'''
with open('Q12flow_pick.txt', 'w') as f12:
    for i in range(SF_edge_Q12_flow_pick.shape[0]):
        f12.write(str(SF_edge_Q12_flow_pick.iloc[i, 0])+ ' '
                  +str(SF_edge_Q12_flow_pick.iloc[i, 1])+ ' ' +str(SF_edge_Q12_flow_pick.iloc[i, 2])+ '\n' )
f12.close()
'''

#plot1
plt.plot(-122.17598222,   37.42968589, 'o', color = 'red')
plt.plot(-122.06456181,   36.97416881, 'o', color = 'black')
for i in range(SF_edge_Q12_pick.shape[0]):
    src_id = int(SF_edge_Q12_pick.iloc[i, 0])
    dst_id = int(SF_edge_Q12_pick.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=2)
plt.xlim(-122.30, -121.7)
plt.ylim(36.90, 37.47)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Defoliated Graph and Stanford(red), UCSC(black)')
plt.show()

#plot 2
plt.plot(-122.06456181,   36.97416881, 'o', color = 'black')
for i in range(SF_edge_Q12_pick.shape[0]):
    src_id = int(SF_edge_Q12_pick.iloc[i, 0])
    dst_id = int(SF_edge_Q12_pick.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=2)
plt.xlim(-122.10, -122.02)
plt.ylim(36.93, 37.01)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Defoliated Graph focused on UCSC(black)')
plt.show()

#plot3
plt.plot(-122.17598222,   37.42968589, 'o', color = 'red')
for i in range(SF_edge_Q12_pick.shape[0]):
    src_id = int(SF_edge_Q12_pick.iloc[i, 0])
    dst_id = int(SF_edge_Q12_pick.iloc[i, 1])
    x_cood = [findcood(src_id, cood_mean_arr)[0], findcood(dst_id, cood_mean_arr)[0]]
    y_cood = [findcood(src_id, cood_mean_arr)[1], findcood(dst_id, cood_mean_arr)[1]]
    plt.plot(x_cood, y_cood, color='blue', linewidth=0.8)
    print(i)
plt.plot(cood_mean_arr_gcc[:,0], cood_mean_arr_gcc[:,1], '.', color = 'green', markersize=2)
plt.xlim(-122.19, -122.11)
plt.ylim(37.38, 37.47)
plt.xlabel('longitude')
plt.ylabel('latitude')
plt.title('Defoliated Graph focused on Stanford(red)')
plt.show()
