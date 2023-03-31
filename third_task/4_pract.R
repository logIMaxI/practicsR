#----------------------(1)
df <- read.csv("https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/GAZ.csv",
               header = T, sep = '\t')
df$дата.замера <- as.Date(df$дата.замера, format = "%d/%m/%Y")
#----------------------(2)
df <- df[-which(df == "", arr.ind = T)[,1], ]
#----------------------(3)
library(stringr)
df[, 2] <- str_replace_all(df[, 2], ',', '.')
df[, 3] <- str_replace_all(df[, 3], ',', '.')
df[, 4] <- str_replace_all(df[, 4], ',', '.')
df[, 5] <- str_replace_all(df[, 5], ',', '.')
df[, 6] <- str_replace_all(df[, 6], ',', '.')
df[,c(2:6)] <- apply(X = df[,c(2:6)], MARGIN = 2, FUN = as.numeric)
df[, 3] <- as.numeric(df[,3]) + 273
colnames(df)[3] <- "Туст.K"
#----------------------(4)
df[,c('ID', 'Куст', 'Группа')] <- apply(X = df[,c('ID', 'Куст', 'Группа')],
                                        MARGIN = 2, FUN = factor)
#----------------------(5)
df$газ_конд <- df[, 4] / df[,5]
df$газ_вода <- df[, 4] / df[,6]
df$вода_конд <- df[, 6] / df[,5]
#----------------------(6)
df <- df[-which(df == 0 | is.na(df) | df == Inf, arr.ind = T)[, 1],]
subset_2018 <- 
#----------------------(7)
subset_ID_111 <- subset(df, df[,"ID"] == 111)
#----------------------(8)
inset <- df[, c(6,7)]
inset <- aggregate(x = inset, by = list(inset$ID), FUN = max)[,c(2,3)]
ID_vectors <- subset(inset, inset$вода.м3.сут <= 2, select = "ID")
ID_vectors
#----------------------(9)
inset <- df[, c(4:7)]
inset$добыча <- apply(X = inset[, c(1:3)], MARGIN = 1, FUN = sum)
inset <- inset[, c(4:5)]
inset <- aggregate(x = inset, by = list(inset$ID), FUN = min)[,c(2,3)]
ID_vectors <- subset(inset, inset$добыча >= 1000, select = "ID")
if (length(ID_vectors) == 0 | length(ID_vectors) == length(inset$ID)){
  sort(inset[, 2], decreasing = T)[c(1:3)]
} else{
  ID_vectors
}
#----------------------(10)
inset <- df[, c(1, 4, 9)]
inset <- inset[(as.Date('31-12-2018', format = "%d-%m-%Y") - 
                 inset[, 1] >= 0) & (as.Date('31-12-2018', format = "%d-%m-%Y") -
                                       inset[, 1] <= 365),]
inset <- inset[, c(2,3)]
inset <- aggregate(x = inset[, 1], by = list(inset$Группа), FUN = sum)
colnames(inset) <- c('Группа', 'добыча')
inset$Группа[which.max(inset$добыча)]
#----------------------(11)
inset <- df[, c(1, 6, 8)]
inset <- inset[(as.Date('31-12-2018', format = "%d-%m-%Y") - 
                  inset[, 1] >= 0) & (as.Date('31-12-2018', format = "%d-%m-%Y") -
                                        inset[, 1] <= 365),]
inset <- inset[, c(2,3)]
inset <- aggregate(x = inset[, 1], by = list(inset$Куст), FUN = sum)
colnames(inset) <- c('Куст', 'добыча')
inset$Куст[which.max(inset$добыча)]
#----------------------(12)
inset <- df[, c("Куст", "газ_вода")]
inset <- aggregate(x = inset[, 2], by = list(inset$Куст), FUN = max)
colnames(inset) <- c("Куст", "газ_вода")
inset$Куст[which.max(inset$газ_вода)]
