#--------------------------(1)
matr <- matrix(data = 3,nrow = 3,ncol = 4)
matr
matr[1,3] <- 4
matr[2,1] <- 1
matr[3,2] <- NA
matr[3,4] <- 1
matr
a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)
matr_sec <- cbind(a,b,c)
matr_third <- rbind(a,b,c)
matr_sec
matr_third
rown <- paste("row",1:5)
coln <- paste("col",1:5)
row.names(matr_sec) <- rown
colnames(matr_sec) <- coln[c(1:3)]
row.names(matr_third) <- rown[c(1:3)]
colnames(matr_third) <- coln
matr_sec
matr_third
#---------------------------(3)
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
table <- matrix(data = 0, nrow = 2, ncol = 4)
colnames(table) <- names
table[1,] <- ages
table[2,] <- gender
table
#---------------------------(4)
nag_list <- list(names,ages,gender)
nag_list[[1]][2]
nag_list[[3]]
names(nag_list) <- c("names","ages","gender")
nag_list$names
nag_list$drinks <- c("juice","tea","rum","coffee")
nag_list$names[5] <- "John"
nag_list$ages[5] <- 2
nag_list$gender[5] <- 1
nag_list$drinks[5] <- "milk"
#---------------------------(5)
index <- "0.72;0.38;0.99;0.81;0.15;0.22;0.16;0.4;0.24"
index <- as.numeric(c(unlist(strsplit(index,';'))))