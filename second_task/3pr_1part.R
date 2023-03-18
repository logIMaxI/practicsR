#----------------------------(1)
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
first_list <- list(names,ages,gender)
first_list[[1]][2]
first_list[[3]]
names(first_list) <- c("name","age","gender")
first_list$name
first_list$drink <- c("juice","tea","rum","coffee")
first_list$name[length(first_list$name)+1] <- "John"
first_list$age[length(first_list$age)+1] <- 2
first_list$gender[length(first_list$gender)+1] <- 1
first_list$drink[length(first_list$drink)+1] <- "milk"
first_list
#----------------------------(2)
index <- "0.72;0.38;0.99;0.81;0.15;0.22;0.16;0.4;0.24"
index <- strsplit(index,';')
index <- as.numeric(unlist(index))
index