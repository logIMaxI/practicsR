vector <- read.table("LPnorm.txt")$V1
vector <- abs(vector)
vector <- vector**2.75
result <- sum(vector)**(1/2.75)
write(result, file = "result.txt")