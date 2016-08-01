### 0. Libraries ----
library(reshape)
library(dplyr)
library(stringr)
library(stringdist)

### 1. Functions ----
Matrix2Edge <- function(x){
  
  data <- x
  final <- data.frame("a"=character(),"b"=character())
  
  #the idea is to iterate over the columns of  the dataframe, melt it, and each time drop the melted column
  for (i in 1:(ncol(source_file)-1)){
    id = paste("V", i, sep="")
    edge_list <- melt(data, id = (id))
    data[id] <- NULL
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
edge_list$a <- str_replace_all(edge_list$a, "[^[:alnum:]]", " ")
edge_list$b <- str_replace_all(edge_list$b, "[^[:alnum:]]", " ")

edge_list$a <- trimws(edge_list$a)
edge_list$b <- trimws(edge_list$b)

edge_list$a <- str_replace_all(edge_list$a, "  ", " ")
edge_list$b <- str_replace_all(edge_list$b, "  ", " ")

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

### B. Network Analysis ----
#use the created edge list csv file "edge_list_synthbio.csv"
edge_list = read.csv(file.choose(), sep = ",", header = T)
edge_list$X <- NULL

library(igraph)
el=as.matrix(edge_list)
graph=graph.edgelist(el,directed=TRUE)

#calculating different degrees
degrees <- data.frame(degree = degree(graph),
in_degree = degree(graph, mode = c("in"), loops = FALSE, normalized = FALSE),
out_degree = degree(graph, mode = c("out"), loops = FALSE, normalized = FALSE),
btwn= betweenness(graph, directed = T),
close = closeness(graph, mode = c("all")),
eigen <- evcent(graph),
bon <- bonpow(graph)
)

#cleaning up the table
degrees = degrees[,c(1:6, 28)]

# correlate the measures
#turn it to a table
cor(degrees)

# regress attributes on centrality measures
summary(lm(in_degree ~ out_degree))

### Graph ----
## start the graph ##
set.seed(12)
l <- layout.kamada.kawai(graph)

# Size node by in-degree.
V(graph)$size <- 3*sqrt(degree(graph, mode="in"))
V(graph)$size2 <- V(graph)$size * .2

# Size node by in-betweenes
V(graph)$size <- ((((betweenness(graph, directed = T)-23.832627)/4.566372)/5.2191602)+1)*2

# Size of node label by in-degree.
V(graph)$label.cex <- 2.5 * degree(graph, mode="in") / max(degree(graph, mode="in"))
V(graph)$label.cex <- 0.001
   # to do: make graph lables conditoined on in-betweeness


# plot the graph
plot(graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")
#title("Co-Authorship Graph",cex.main=0.8,col.main="black")


### to do----
  # transform entity resolution to a function
  # scraping journal articles to extract authors
  # community detection




