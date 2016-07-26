### Libraries ----
library(reshape)
library(dplyr)
library(stringr)

### Loading the file ----
source_file = read.csv(file.choose(), sep = ";", header = F)

### Functions ----
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

### Create the edge list ----
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

### Network Analysis ----

### to do
  # transform entity resolution to a function
  # scraping journal articles to extract authors



