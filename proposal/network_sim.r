library(igraph)

set.seed(1)
# want to simulate a small bipartite graph
# x taxa, n localities
xt <- seq(9)
nl <- LETTERS[1:3] 

# make random numbers of occurrences of 1:3
occ <- list()
for(ii in seq(length(xt))) {
  no <- sample(seq(nl), 1)
  oo <- sample(nl, no)
  occ[[ii]] <- cbind(rep(xt[ii], length(oo)), oo)
}
edgelist <- Reduce(rbind, occ)


graph <- graph.data.frame(edgelist, directed = FALSE)
V(graph)$type <- V(graph)$name %in% edgelist[, 1]

co <- layout.kamada.kawai(graph)

pdf(file = './figure/sim_graph.pdf')
par(mar = rep(0, 4))
print(plot(graph, 
           vertex.color = rep(c('lightblue', 'goldenrod'), 
                              c(length(xt), length(nl))),
           vertex.size = 20,
           edge.width = 5,
           layout = co,
           edge.curved = TRUE))
dev.off()
