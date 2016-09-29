### 0. Libraries ----
library(reshape)
library(dplyr)
library(stringdist)

### 1. Functions ----
Matrix2Edge <- function(x){
  final <- data.frame("a"=character(),"b"=character()) #an empty dataframe to add the edge list to
  
  for (i in 1:(ncol(x)-1)){
    id = paste("V", i, sep="")
    edge_list <- melt(x, id = (id))
    x[id] <- NULL
    keeps <- c(id, "value")
    edge_list <- edge_list[keeps]
    colnames(edge_list) <- colnames(final)
    final <- rbind(final,edge_list)
  }
  final <- final[!(final$b=="" | final$b==" "),]
  return(final)
}

simmilarity_index <- function(x, y){
  
  sim_index <- 1- stringdist(x, y, method = "lv")/max(nchar(as.character(x)), nchar(as.character(y)))
  return(sim_index)  
}

### A.1. Loading the file ----
source_file = read.csv(file.choose(), sep = ";", header = F)

### A.2. Create the edge list ----
edge_list <- Matrix2Edge(source_file)

# triming and cleaning up cells
library(dplyr)

edge_list <- as.data.frame(sapply(edge_list, function(x) gsub("[^[:alnum:]]"," ", x))) #removes all non-alphanumerical characters (like punctuations, special characters, etc)
edge_list <- as.data.frame(sapply(edge_list, function(x) trimws(x))) #removes leading and trainling whitespaces
edge_list <- mutate_each(edge_list, funs(tolower))

#entity resolution
for (i in 1:nrow(edge_list)){
  for (j in 1:i){
    if (simmilarity_index(edge_list$a[i],edge_list$a[j]) > 0.8){
      edge_list$a[i] <- edge_list$a[j]
    }
  }
}

for (i in 1:nrow(edge_list)){
  for (j in 1:i){
    if (simmilarity_index(edge_list$b[i],edge_list$b[j]) > 0.8){
      edge_list$b[i] <- edge_list$b[j]
    }
  }
}

write.csv(edge_list, file = "edge_list_synthbio.csv")

### B.1. Network Analysis ----
#use the created edge list csv file "edge_list_synthbio.csv"
edge_list = read.csv(file.choose(), sep = ",", header = T)
edge_list$X <- NULL

library(igraph)
graph=graph.edgelist(as.matrix(edge_list),directed=FALSE)

#graph density
graph.density(graph)

#calculating different degrees
degrees <- data.frame(degree = degree(graph),
in_degree = degree(graph, mode = c("in"), loops = FALSE, normalized = FALSE),
out_degree = degree(graph, mode = c("out"), loops = FALSE, normalized = FALSE),
btwn= betweenness(graph, directed = T),
close = closeness(graph, mode = c("all")),
eigen <- evcent(graph)
)
degrees <- data.frame(degree,in_degree,out_degree,btwn,close,eigen)
#cleaning up the table
degrees = degrees[,c(1:6, 28)]
  #include this in markdown
degrees_sorted <- degrees[order(-degrees$in_degree),] 

library(gridExtra)
pdf("data_output.pdf", height=15, width=15)
grid.table(degrees)
dev.off()


# correlate the measures
#incldue this in markdown
cor(degrees_sorted)
   
# regress attributes on centrality measures
  #, do more corelation (in-degree and betweeness)
in_degree = degree(graph, mode = c("in"), loops = FALSE, normalized = FALSE)
out_degree = degree(graph, mode = c("out"), loops = FALSE, normalized = FALSE)
total_degree = degree(graph)
btwn= betweenness(graph, directed = T)
summary(lm(total_degree ~ btwn))

### B.2. Graph ----
## start the graph ##
set.seed(12)
l <- layout.kamada.kawai(graph)

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


### to do----
  # transform entity resolution to a function
  # scraping journal articles to extract authors




