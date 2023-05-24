# Пользовательские функции ------------------------------------------------------------------------------
erfc <- function(x){
  return(2 / sqrt(acos(-1)) * exp(-(x * x)))
}

shov.crit <- function(data){
  x <- (data - mean(data)) / sd(data)
  values <- c()
  for (elem in x){
    values <- c(values, integrate(f = erfc, lower = elem, upper = +Inf)$value)
  }
  data <-  ifelse(values < 1 / (2 * length(data)), median(data), data)
  return(data)
}

train.nnetar <- function(data){
  MSE.threshold <- 0.005 * mean(data)
  best.model <- nnetar(y = data, p = 1, size = 1)
  MSE.best <- sum(best.model$residuals ** 2, na.rm = T) /
    length(best.model$residuals)
  for (p in seq(4, 20, 2)){
    for (size in seq(4, 40, 2)){
      model <- nnetar(y = data, p = p, size = size, repeats = 30)
      model.MSE <- sum(model$residuals ** 2, na.rm = T) / length(model$residuals)
      if (model.MSE <= MSE.threshold){
        return(model)
      } else if (model.MSE <= MSE.best){
        best.model <- model
        MSE.best <- model.MSE
      }
    }
  }
  return(best.model)
}

get_elements_from_page <- function(path){
  page <- read_html(path)
  elements <- html_elements(x = page, css = "tr") %>% html_text()
  elements <- elements[-1]
  elements <- str_split(elements, "[a-z0-9]+")
  result_vec <- c()
  for (i in seq(1, length(elements))){
    cur_elem <- elements[[i]][1]
    cur_elem <- substr(cur_elem, 1, str_length(cur_elem) - 1)
    result_vec <- c(result_vec, cur_elem[length(cur_elem)])
  }
  return(result_vec)
}

NA.clear <- function(data){
  for (i in seq(1, length(colnames(data)))){
    cur_median <- median(data[, colnames(data)[i]], na.rm = T)
    data[, colnames(data)[i]] <- ifelse(is.na(data[, colnames(data)[i]]), cur_median, 
                                        data[, colnames(data)[i]])
  }
  return(data)
}
#--------------------

# Внешние подключения -----------------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(quantmod)
library(ggplot2)
library(forecast)
library(ggpubr)

#--------------------

# ОБРАБОТКА служебных знаков
# Парсинг сайта Yahoo для получения акций--------------------------------------------------------------------
first_page <- get_elements_from_page("https://finance.yahoo.com/etfs?offset=0&count=100")
second_page <- get_elements_from_page("https://finance.yahoo.com/etfs?count=100&offset=100")
all_symbs <- c(first_page, second_page)
cat("Все возможные акции: ", all_symbs)
names <- readline(prompt = "Введите название интересущей акции / акций:")
names <- c(unlist(str_split(names, ' ')))
df <- getSymbols(Symbols = names, src = "yahoo", from = as.Date("2022-01-01"))
#--------------------

# Предобработка данных ----------------------------------------------------------------------------------
list_df <- list()
for (name in names) {
  list_df[[name]] <- get(name)
}
df <- data.frame()
#оставляем только close - по нему будем вести предсказание
for (i in seq(1, length(names))){
  if (length(df) == 0){
    df <- list_df[[names[i]]][, 4]
  } else {
    df <- cbind(df, list_df[[names[i]]][, 4])
  }
}
colnames(df) <- names
dates <- index(get(name))
#а также будем оставлять данные только на конец рабочей недели - для долгосрочных предсказаний(!)
df <- df[wday(dates, week_start = 1) == 5, ]
df_time_seq <- index(df)

#до очистки данных
plots <- list()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot(data = df)+
      geom_boxplot(aes(x = name, y = df[, name]))+
      labs(title = paste("Курс акции", name),
           x = "Акция", y = "Курс акции")
  })
}
plots[[length(colnames(df)) + 1]] <- 
  ggtexttable(summary(df))
plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2)
annotate_figure(plot_range, top = text_grob("Графики распределения значений временных рядов до очистки", 
                                            face = "bold", size = 16))

#обработка данных - проверка на NA и экстремальные значения
NA.count <- sum(is.na(df))
if (NA.count != 0){
  df <- apply(X = df, MARGIN = 2, FUN = NA.clear)
}
df <- data.frame(apply(X = df, MARGIN = 2, FUN = shov.crit))

#после очистки данных
plots <- list()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot(data = df)+
      geom_boxplot(aes(x = name, y = df[, name]))+
      labs(title = paste("Курс акции", name),
           x = "Акция", y = "Курс акции")
  })
}
plots[[length(colnames(df)) + 1]] <- 
  ggtexttable(summary(df))
plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2)
annotate_figure(plot_range, top = text_grob("Графики распределения значений временных рядов после очистки", 
                                            face = "bold", size = 16))
#--------------------

# Построение графиков ВР ------------------------------------------------------------------------------
plots <- list()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot(data = df) +
      geom_line(aes(x = df_time_seq, y = df[, name]))+
      labs(x = "Дата", y = paste("Курс акции", name, "на конец рабочей недели"),
           title = paste("Временной ряд для акции", name))
  })
}
plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2)
annotate_figure(plot_range, top = text_grob("Графики исходных временных рядов", face = "bold", size = 16))
#--------------------

# Работа с моделью, оценка и визуализация ---------------------------------------------------------------
#обучение модели и предсказания
models <- apply(X = df, MARGIN = 2, FUN = train.nnetar)
fitted_values <- data.frame()
errors_values <- data.frame()
for (name in colnames(df)){
  if (length(fitted_values) == 0){
    fitted_values <- models[[name]]$fitted
    errors_values <- models[[name]]$residuals
  } else{
    fitted_values <- cbind(fitted_values, models[[name]]$fitted)
    errors_values <- cbind(errors_values, models[[name]]$residuals)
  }
}
colnames(fitted_values) <- names
colnames(errors_values) <- names

#графики ошибок
residuals <- list()
for (name in colnames(df)){
  residuals[[name]] <- local({
    name <- name
    ggplot() +
      geom_line(aes(x = df_time_seq, y = errors_values[, name]))+
      geom_point(aes(x = df_time_seq, y = errors_values[, name]))+
      labs(title = paste("График распределения ошибок для акции", name),
           x = "Дата", y = paste("Величина ошибки"))
  })
}
plot_range <- ggarrange(plotlist = residuals,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2)
annotate_figure(plot_range, top = text_grob("Графики распределния ошибок составленных моделей", face = "bold",
                                            size = 16))

#построение прогнозов
predict_values <- data.frame()
for (name in names){
  if (length(predict_values) == 0){
    predict_values <-  data.frame(forecast(models[[name]], h = 15))[, 1]
  } else {
    predict_values <- cbind(predict_values, data.frame(forecast(models[[name]], h = 15))[, 1])
  }
}
colnames(predict_values) <- names
new_time_vec <- seq(from = df_time_seq[length(df_time_seq)] + 7,
                    to = df_time_seq[length(df_time_seq)] + 111, by = 7)

#построение графиков исходных ВР, моделей ВР и предсказаний
plots <- list()
MSEs <- data.frame()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot() +
      geom_line(aes(x = df_time_seq, y = df[, name]), color = "black", size = 1)+
      geom_line(aes(x = df_time_seq, y = fitted_values[, name]), color = "red", size = 1)+
      geom_line(aes(x = c(df_time_seq[length(df_time_seq)], new_time_vec[1]),
                    y = c(fitted_values[, name][length(fitted_values[, name])], predict_values[, name][1])), 
                color = "red", linetype = 2)+
      geom_line(aes(x = new_time_vec, y = predict_values[, name]), color = "green", size = 1)+
      labs(x = "Дата", y = paste("Курс акции", name, "на конец рабочей недели"),
           title = paste("Временной ряд для акции", name))
  })
  if (length(MSEs) == 0){
    MSEs <- sum(models[[name]]$residuals ** 2, na.rm = T) / length(models[[name]]$residuals)
  } else {
    MSE <- sum(models[[name]]$residuals ** 2, na.rm = T) /
      length(models[[name]]$residuals)
    MSEs <- cbind(data.frame(MSEs), MSE)
  }
}

#добавление на график сводки по ошибкам моделей -> оценка полученных моделей через MSE
plots[[length(colnames(df)) + 1]] <- ggtexttable(t(MSEs), rows = names)
plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(plot_range, top = text_grob("Графики исходных временных рядов, построенных моделей и предсказаний, а также MSE", 
                                            face = "bold", size = 16))

#--------------------

# вердикт -------------------------------------------------------------------------------
plots <- list()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot() +
      geom_line(aes(x = new_time_vec, y = predict_values[, name]), color = "green", size = 1)+
      labs(x = "Дата", y = paste("Курс акции", name, "на конец рабочей недели"),
           title = paste("Предсказания на год вперёд для акции ", name))
  })
}

plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(plot_range, top = text_grob("Предсказания на год вперёд для инетерсующих акций", 
                                            face = "bold", size = 16))

plots <- list()
for (name in colnames(df)){
  plots[[name]] <- local({
    name <- name
    ggplot() +
      geom_violin(aes(x = name, y = predict_values[, name]), color = "black", fill = "lightblue")+
      labs(x = "Дата", y = paste("Курс акции", name, "на конец рабочей недели"),
           title = paste("Предсказания на год вперёд для акции ", name))
  })
}

plot_range <- ggarrange(plotlist = plots,
                        ncol = (length(colnames(df)) + 1) %/% 2, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(plot_range, top = text_grob("Предсказания на год вперёд для инетерсующих акций", 
                                            face = "bold", size = 16))

