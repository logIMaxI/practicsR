inputs <- readline(prompt = "Введите вектор одного типа: ")
vec <- as.character(c(unlist(strsplit(inputs,split=' '))))
vec[vec=="NA"] <- NA
length(which(is.na(vec)))