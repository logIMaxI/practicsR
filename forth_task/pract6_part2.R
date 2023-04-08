#-----------------------(1)
library(quantmod)

library(stringr)

downloadable_stocks <- c("ATVI", "^IXIC")
getSymbols(Symbols = downloadable_stocks,
           src = "yahoo",
           from = as.Date.character("1900-01-01"))
df <- data.frame(get(downloadable_stocks[1]))
downloadable_stocks <- str_remove(downloadable_stocks,
                                           "[:punct:\\^]")
rm(list = downloadable_stocks)
#-----------------------(2)
out_of_trend <- function(x, dt, method = 
                           c("Arifm", "Geom", "Garm")){
  if (length(x) < 3){
    print("Error: vector x is too small (min length= 3)")
    return()
  } else if (dt > ceiling(length(x) / 2) - 1){
    print("Error: dt-value is too big (max = ceiling(x / 2) - 1")
    return()
  } else if (!is.numeric(x) || !is.numeric(dt)){
    print("Error: vector x or dt is not numeric")
    return()
  } else {
    x <- x + min(x) + 1
    if (method == "Garm"){
      t_min <- 1 + 2 * dt
      t_max = length(x) - 2 * dt
      new_x <- x[c((dt + 1): (length(x) - dt))]
      
      sup_x <- x[c(t_min:length(x))]
      sum_x <- (x + sup_x)[c(1: t_max)]
      
      x <- 2 * x * sup_x
      x <- x[c(1: t_max)]
      
      y <- log(x / (new_x * sum_x))
      return(y)
    } else if (method == "Geom"){
      t_min <- 1 + 2 * dt
      t_max = length(x) - 2 * dt
      new_x <- x[c((dt + 1): (length(x) - dt))]
      sup_x <- x[c(t_min:length(x))]
      x <- x * sup_x
      x <- x[c(1: t_max)]
      y <- log(x / (new_x ** 2))
      return(y)
    } else {
      t_min <- 1 + 2 * dt
      t_max = length(x) - 2 * dt
      new_x <- x[c((dt + 1): (length(x) - dt))]
      sup_x <- x[c(t_min:length(x))]
      x <- x + sup_x
      x <- x[c(1: t_max)]
      y <- log(x / (2 * new_x))
      return(y)
    }
  }
}
#-----------------------(3)
t <- seq(0, 10, 0.1)
x <- 2 * t + 3 + sin(2 * t)
x_mean <- mean(x)
xn <- out_of_trend(x, 2, method = "Arifm")
xn_mean <- mean(xn)
xn_mean
#-----------------------(4)
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
#-----------------------(5)
xn_alter <- alter_johns(xn)
which.min(xn_alter)
#-----------------------(6)
library(ggplot2)
my_vector <- alter_johns(df$ATVI.Open)
my_df <- data.frame(list(as.Date(row.names(df), format = "%Y-%m-%d"), my_vector))
colnames(my_df) <- c("Date", "Alter_John_function")
ggplot(data = my_df, aes(x = Date, y = Alter_John_function))+
  geom_line()
