### Graph ----
library(igraph)

edge_list = read.csv(file.choose(), sep = ",", header = T)
edge_list$X <- NULL

graph=graph.edgelist(as.matrix(edge_list),directed=FALSE)

## start the graph ##
set.seed(12)
l <- layout.kamada.kawai(graph) #explain differnt graph layouts

# Size of node by in-degree.
V(graph)$size <- 15*(degree(graph, mode="in")/ max(degree(graph, mode="in")))

# Size of node label by in-degree.
V(graph)$label.cex <- ((betweenness(graph, directed = T)+1)/(max(betweenness(graph, directed = T))+1))*2

# plot the graph
plot(graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")
#title("Co-Authorship Graph",cex.main=0.8,col.main="black")

### B.3. Community detection ----
fgn = edge.betweenness.community (graph, directed = TRUE, edge.betweenness = TRUE, merges = TRUE,
                                  bridges = TRUE, modularity = TRUE, membership = TRUE)  ## run Girvan-Newman partitioning

plot(fgn, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot G-N partitioning

fwt <- walktrap.community(graph, steps=200,modularity=TRUE) # , labels=TRUE)  ## run random walk partitioning

plot(fwt, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot R-W partitioning

flp = label.propagation.community(graph)  ## run label propogation partitioning

plot(flp, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot L-P partitioning

#comparing different community dittection algorithms
compare(fgn, fwt, method= c("nmi"))
compare(fgn, fwt, method= c("rand"))
compare(fgn, fwt, method= c("adjusted.rand"))

compare(fgn, flp, method= c("nmi"))
compare(fgn, fwt, method= c("rand"))
compare(fgn, flp, method= c("adjusted.rand"))

## get the results in a dataframe

girvan = data.frame(fgn$membership)
rw = data.frame(fwt$membership)
flpm = data.frame(flp$membership)

degrees <- cbind(degrees, girvan, rw, flpm)

