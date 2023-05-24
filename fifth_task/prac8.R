#-------------------(1)
download.file("https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt",
              destfile = "data.txt")
#-------------------(2)
df <- read.csv("data.txt", skip = 47, header = T, sep = '\t')
#-------------------(3)
names <- paste("канал_", c(1:(length(df) - 1)))
colnames(df) <- c("Период", names)
#-------------------(4)
#which(df[, 2] == '', arr.ind=T) -> сколько неинформативных строк в каждом блоке перед новой серией
list_df <- list()
cur_ind <- 1
index_vec <- which(df[, 1] == 0, arr.ind=T)
for (ind in seq(1, length(index_vec))){
  if (ind == 1){
    list_df[[cur_ind]] <- df[c(1:(index_vec[ind] - 7)), ]
    cur_ind <- cur_ind + 1
  } else {
    list_df[[cur_ind]] <- df[c(index_vec[ind-1]:(index_vec[ind] - 7)), ]
    cur_ind <- cur_ind + 1
  }
}
list_df[cur_ind] <- df[c(index_vec[length(index_vec)-1]:length(df)), ]
#количество опытов
length(list_df)
#-------------------(5)
options(digits.secs = 3)
start_date <- strptime("00:00.000",  format = "%M:%OS")

for (ind in seq(1, length(list_df))){
  list_df[[ind]]$Время <- format(seq.POSIXt(as.POSIXct(start_date), length.out = nrow(list_df[[ind]]),
                                            units = "seconds", by = .004), "%Y-%m-%d %H:%M:%OS3")
  list_df[[ind]]$Время <- zoo::na.approx(list_df[[ind]]$Время)
}
#-------------------(6)
correct_numeric <- function(x){
  x <- stringr::str_replace_all(x, ',', '.')
  return(as.numeric(x))
}

for (ind in seq(1, length(list_df))){
  list_df[[ind]][, c(2:6)] <- apply(list_df[[ind]][, c(2:6)], MARGIN = 2, FUN = correct_numeric)
}
for (ind in seq(1, length(list_df))){
  print(ind)
  print(summary(list_df[[ind]][, c(2:6)]))
  print("--------")
}
#-------------------(7)
#посмотрим на изменение исходных ВР при применении алгоритма сглаживания (функция Альтер-Джонса)
alter_johns <- function(y){
  result <- c()
  for (tau in c(1:length(y) - 1)){
    sup_y <- y[c((1 + tau):length(y))]
    sup_y <- sup_y - y[c(1:(length(y) - tau))]
    a_tau <- sum(sup_y) / (length(y) - tau)
    result <- c(result, a_tau)
  }
  return(result)
}
#График ВР можно построить для любой серии, изменяя индекс
library(ggplot2)
ggplot()+
  geom_line(x = strptime(list_df[[1]][, 7], format = "%Y-%m-%d %H:%M:%OS"), y = list_df[[1]][, 3])+
  labs(title = "Временной ряд для серии 1 и канала 2", x = "Время", y = "канал_ 2") +
  scale_x_date(date_labels = "%M:%OS3")

ggplot()+
  geom_line(x=strptime(list_df[[1]][, 7], format = "%Y-%m-%d %H:%M:%OS"), y=alter_johns(list_df[[1]][, 3]))+
  labs(title = "Сглаженный временной ряд для серии 1 и канала 2", x = "Время", y = "канал_ 2") +
  scale_x_date(date_labels = "%M:%OS3")
