
# ============================ Q6 =============================

import igraph as ig
import json

g = ig.Graph.Read(f = 'dataset/edge_weight', format = 'ncol', directed = False)
gcc = g.components().giant()
ig.summary(g)

name_to_disname_loc = {}

with open('dataset/san_francisco_censustracts.json', 'r') as f:
    data = json.loads(f.readline())
    features = data['features']
    for feature in features:
        coordinates = feature['geometry']['coordinates'][0][0]
        latitude = 0
        longitude = 0
        for coordinate in coordinates:
            latitude += coordinate[0]
            longitude += coordinate[1]
        latitude /= len(coordinates)
        longitude /= len(coordinates)
        name_to_disname_loc[feature['properties']['MOVEMENT_ID']] = (feature['properties']['DISPLAY_NAME'], latitude, longitude)

seq = gcc.vs()
for i in seq:
    i['display name'] = name_to_disname_loc[i['name']][0]
    i['location'] = (name_to_disname_loc[i['name']][1], name_to_disname_loc[i['name']][2])
ig.summary(gcc)



# ============================ Q7 =============================

mst = gcc.spanning_tree(weights = gcc.es["weight"])
vertices = mst.vs()
ig.summary(mst)
print()
print('+' + 90 * '-' + '+')
for e in mst.es():
    st = e.tuple
    print(str(vertices[st[0]]['display name']) + ' -- ' + str(vertices[st[1]]['display name']))
    print('+' + 90 * '-' + '+')



# ============================ Q8 =============================

import random

max_index = len(gcc.vs()) - 1 # 1879
gcc_vertices = gcc.vs()
num_of_experience = 1000
used_combine = set()
i = 0

num_of_satisfy = 0
while i < num_of_experience:
    cur_combine = ()
    while True:
        indices = set()
        while len(indices) < 3:
            indices.add(random.randint(0,max_index))
        li = list(indices)
        li.sort()
        cur_combine = tuple(li)
        if not cur_combine in used_combine:
            used_combine.add(cur_combine)
            break
    
    edges = gcc.es.select(_within = cur_combine)
    if len(edges) < 3:
        continue
    num = 'experience ' + str(i)
    i += 1
    print(num.center(90, '='))
    count = 1
    weights = []
    for edge in edges:
        weights.append(edge['weight'])
        st = edge.tuple
        print('edge '+str(count)+': ' + str(gcc_vertices[st[0]]['display name']) +               ' -- ' + str(gcc_vertices[st[1]]['display name']))
        print('weight: ' + str(edge['weight']))
        count += 1
        print()
    if weights[0] + weights[1] > weights[2] and weights[2] + weights[1] > weights[0] and     weights[0] + weights[2] > weights[1]:
        print('satisfy the triangle inequality')
        num_of_satisfy += 1
    else:
        print('not satisfy the triangle inequality')
print(90 * '=')
print('satisfying rate: ' + str(num_of_satisfy / num_of_experience))



# ============================ Q9 =============================

import heapq

# get an Eulerian walk
def euler(g, v, vset):
    edges = g.es.select(_source_in = [v])
    if len(edges) == 0:
        return [v]
    else:
        walk = [v]
        awalk(g, walk, set(), vset)
        for i in range(len(walk) - 1):
            edges = g.es.select(_between = ([walk[i]], [walk[i + 1]]))
            g.delete_edges(edges[0])
        res = []
        for vertex in walk:
            nextwalk = euler(g, vertex, vset)
            for i in nextwalk:
                res.append(i)
        return res

def awalk(g, res, visited, vset):
    cur = res[-1]
    if len(res) > 1 and cur == res[0]:
        return True
    else:
        edges = g.es.select(_between = ([cur], vset))
        for edge in edges:
            if not edge in visited:
                visited.add(edge)
                tu = edge.tuple
                nex = -1
                if tu[0] == cur:
                    nex = tu[1]
                else:
                    nex = tu[0]
                res.append(nex)
                nexres = awalk(g, res, visited, vset)
                if nexres:
                    return True
                else:
                    res.pop()
                    visited.remove(edge)
        return False

# shortest path (Dijkstra algorithm, only when not finding weight in the original graph)
def shortest_path(graph, idx1, idx2):
    v_set = [i for i in range(len(graph.vs))]
    
    expanded = {} # data -> (priority, who generate)
    generated = {} # data -> (priority, who generate)
    # initialize
    heap = [[0, idx1]] # heap initial: (priority, data)
    generated[idx1] = [0, idx1]
    # expand & generate
    while len(heap) != 0:
        # expand
        cur = heapq.heappop(heap)
        cur_idx = cur[1]
        cur_wei = cur[0]
        expanded[cur_idx] = generated[cur_idx]
        if cur_idx == idx2:
            break
        # generate
        edges = graph.es.select(_between = ([cur_idx], v_set))
        for edge in edges:
            if edge.tuple[0] == cur_idx:
                gen_idx = edge.tuple[1]
            else:
                gen_idx = edge.tuple[0]
            new_wei = edge['weight'] + cur_wei
            if gen_idx not in expanded:
                if gen_idx not in generated:
                    generated[gen_idx] = [new_wei, cur_idx]
                    heapq.heappush(heap, [new_wei, gen_idx])
                elif new_wei < generated[gen_idx][0]:
                    heap.remove([generated[gen_idx][0], gen_idx])
                    heap.append([new_wei, gen_idx])
                    heapq.heapify(heap)
                    generated[gen_idx] = [new_wei, cur_idx]
    
    # create path from idx1 to idx2
    shortest_wei = expanded[idx2][0]
    path = list()
    cur = idx2
    path.append(idx2)
    while cur != idx1:
        cur = expanded[cur][1]
        path.append(cur)
    path.reverse()
    
    return path, shortest_wei


import sys
sys.setrecursionlimit(10000)  # set the maximum depth as 10000

# (b) Eulerian spanning graph
edgelist = []
for e in mst.es():
    edgelist.append(e.tuple)

mst_double = mst.as_undirected()
mst_double.add_edges(edgelist)

# (c) get an Eulerian walk
mst1 = mst_double.as_undirected()
euwalk = euler(mst1, 0, mst1.vs())


# (d) a 2-approximate tour
path = []
visited = set()
for index in euwalk:
    if not index in visited:
        path.append(index)
        visited.add(index)

total_weight = 0
final_path = []
for i in range(len(path) - 1):
    index1 = path[i]
    index2 = path[i + 1]
    edges = gcc.es.select(_between = ([index1], [index2]))
    if len(edges) > 0:
        total_weight += edges[0]['weight']
        final_path.append(index1)
        final_path.append(index2)
    else: # if there are no valid edge in the original graph, use shortest path 
        path1, weight = shortest_path(gcc, index1, index2)
        total_weight += weight
#         total_weight += (gcc.shortest_paths_dijkstra([index1], [index2], weights = gcc.es()['weight']))[0][0]
        for p in path1:
            final_path.append(p)

print('total weight: ' + str(total_weight))


# upper bound
upper_bound = total_weight / sum(mst.es['weight'])
print(upper_bound)


# print path
print(mst.vs[final_path[0]]['display name'] + ' -->\n')
for i in range(len(final_path)):
    if i%2 != 0:
        print(mst.vs[final_path[i]]['display name'] + ' -->\n')



# ============================ Q10 =============================

print(len(final_path))
print(len(path))

with open('dataset/path.csv', 'w') as f:
    f.write('Longitude, Latitude, Line Group (Path ID), Order of Points \n')
    # first node
    loc = gcc.vs[final_path[0]]['location']
    f.write(str(loc[0]) + ', ' + str(loc[1]) + ', ' + '1, 1 \n')
    order = 2
    for i in range(1, len(final_path) - 1):
        if i%2 != 0:
            location = gcc.vs[final_path[i]]['location']
            f.write(str(location[0]) + ', ' + str(location[1]) + ', ' + '1, ' + str(order) + '\n')
            order += 1
    # last node
    loc = gcc.vs[final_path[len(final_path) - 1]]['location']
    f.write(str(loc[0]) + ', ' + str(loc[1]) + ', ' + '1, ' + str(order) + '\n')
    
