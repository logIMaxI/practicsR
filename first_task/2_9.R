vector <- read.table("LPnorm.txt")$V1
result <- vector[-1]
result <- result-vector
result <- result[-length(result)]
new_result <- result[-1]
new_result <- new_result-result
new_result <- new_result[-length(new_result)]
write(result,file = "diff_vectors.txt",ncolumns = 17,sep=';')
write(new_result,file="diff_vectors.txt",append = T,ncolumns = 16,sep=';')