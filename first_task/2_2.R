inputs <- readline("Введите числовой вектор через пробел: ")
vec <- as.numeric(c(unlist(strsplit(inputs,split=' '))))
vec[length(vec)] <- NA
vec