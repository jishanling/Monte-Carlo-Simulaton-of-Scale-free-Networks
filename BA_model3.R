## STAT 428 Final Project
## Simulate preferential attachment
## Tinghao Guo
rm(list=ls())
library(igraph)
n_total = 10000       # total number of nodes
m0= c(1,3,5,7)        # initial starting graph m0 = 1,3,5,7 repectively
alpha = list()
KS = list()
p = list()
degree = list()
degree_dist = list()

for (l in 1:length(m0)){
  label_list = NULL
  num_add = n_total - m0[l]
  m = m0[l] # edges to be added each time
  adj = matrix(0, m0[l] + num_add, m0[l] + num_add) # pre-allocate the adjacency matrix
  for (i in 1:num_add){
    if (length(label_list) != 0){
      # uniformly at random choose two target nodes from the label list
      r = sample(1:length(label_list), m, replace = FALSE)
      for (j in 1:m){
        # connect the edge between the targets and the new node
        adj[i+m0[l], label_list[r[j]]] = 1 
        adj[label_list[r[j]], i+m0[l]] = 1
        # update the label list
        label_list = c(label_list,label_list[r[j]])
        label_list = c(label_list,i+m0[l])
      }
    }
    else{
      for (k in 1:m){
        # base case: the initial graph starts with m0 nodes
        # connect the new node to the initial m0 nodes
        adj[m0[l]+1,k] = 1;
        adj[k,m0[l]+1] = 1;
        label_list = c(label_list,m0[l]+1,k) 
      }
    }
  }
  ## create a graph object using igraph
  g = graph.adjacency(adj, mode = "undirected", weighted = NULL,diag = FALSE)
  fit = power.law.fit(degree(g),10)
  degree[[l]] = degree(g)
  alpha[[l]] = fit$alpha
  KS[[l]] = fit$KS.stat
  p[[l]] = fit$KS.p
  degree_dist[[l]] = degree.distribution(g)
}

## Figure
plot(degree_dist[[1]],col = "blue", xlim = c(1,1000), pch = 0, log = "xy",xlab = "Degree k",ylab = "P(k)")
par(new = T)
plot(degree_dist[[2]],col = "red", xlim = c(1,1000), pch = 1, 
     log = "xy",xlab = "Degree k",ylab = "P(k)", axes = FALSE)
par(new = T)
plot(degree_dist[[3]],col = "green", xlim = c(1,1000), pch = 2, 
     log = "xy",xlab = "Degree k",ylab = "P(k)", axes = FALSE)
par(new = T)
plot(degree_dist[[4]],col = "black", xlim = c(1,1000), pch = 3, 
     log = "xy",xlab = "Degree k",ylab = "P(k)", axes = FALSE)
title(main = "Degree Distribution", xlab = "Degree k",ylab = "P(k)", font.main = 4)
legend('topright', pch = c(0,1,2,3), col = c("blue","red","green","black"), 
       legend = c("m0 = 1","m0 = 3","m0 = 5","m0 = 7"))

