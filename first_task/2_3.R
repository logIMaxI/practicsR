inputs <- readline(prompt = "Введите вектор одного типа: ")
vec <- as.character(c(unlist(strsplit(inputs,split=' '))))
vec[vec=="NA"] <- NA
which(is.na(vec),arr.ind = T)