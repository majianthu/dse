library(copent)
library(igraph)

# Boston housing data
house1 = read.csv("~/Rworks/dse/housing.csv")
house1$CHAS = house1$CHAS + 0.00000001 * runif(dim(house1)[1])

# copula entropy based adj matrix
adjm = matrix(0,14,14); rownames(adjm) = names(house1); colnames(adjm) = names(house1)
for(i in 1:13){
  for(j in (i+1):14){
    adjm[i,j] = copent(house1[,c(i,j)])
    adjm[j,i] = adjm[i,j]
  }
}
adjm = adjm + 0.3

# maximum spanning tree
g1 = graph_from_adjacency_matrix(1/adjm, weighted = TRUE, mode = "undirected")
g1mst = mst(g1)
plot(g1mst)
