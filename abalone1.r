library(copent)
library(igraph)

# abalone data
aba1 = read.csv("~/Rworks/dse/abalone.data", header = F)
names(aba1) = c("S","L","D","H","ww","sw1","vw","sw2","R")
aba1$S[which(aba1$S=="M")] = 1; aba1$S[which(aba1$S=="F")] = 2; aba1$S[which(aba1$S=="I")] = 3; 
aba1$S = as.numeric(aba1$S)
aba1$S = aba1$S + runif(dim(aba1)[1]) * 0.000001

# copula entropy based adj matrix
adjm = matrix(0,9,9); rownames(adjm) = names(aba1); colnames(adjm) = names(aba1)
for(i in 1:8){
  for(j in (i+1):9){
    adjm[i,j] = copent(aba1[1:1500,c(i,j)])
    adjm[j,i] = adjm[i,j]
  }
}

# maximum spanning tree
g1 = graph_from_adjacency_matrix(1/adjm, weighted = TRUE, mode = "undirected")
g1mst = mst(g1)
plot(g1mst)
