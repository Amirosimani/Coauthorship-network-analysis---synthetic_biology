
data = read.csv(file.choose(), sep = ";", header = F)
edgelist = read.csv(file.choose(), sep = ";", header = F)

edgelist <- as.matrix(edgelist)

matrix <-  matrix(0, 212,212)
rownames(matrix) <- edgelist$V1
colnames(matrix) <- edgelist$V1

data[] <- lapply(data, as.character)

for(i in 1:nrow(data)){matrix[rownames(matrix)==data[i,2],colnames(matrix)==data[i,1]] <- 1}

for (i in 1:nrow(data)){
 if(rownames(matrix) == data[i,1]){
   matirx[]
 }
     
  }
}
rownames(matrix)
