#-------------------------(1)
demography <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv")
#-------------------------(2)
demography$young_share <- (demography$young_total / demography$popul_total * 100) 
demography$trud_share <- (demography$wa_total / demography$popul_total * 100) 
demography$old_share <- (demography$ret_total / demography$popul_total * 100) 
#-------------------------(3)
library(ggplot2)

ggplot(data = demography, aes(x = trud_share)) +
  geom_histogram(bins = 15, color = "black", fill = "white") +
  geom_vline(xintercept = median(demography$trud_share),
             color = "red") +
  labs(title = "Гистограмма распределения доли трудоспособного населения",
       x = "Доля трудоспособных") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10)
#-------------------------(4)
ggplot(data = demography, aes(x = trud_share, group = region,
                              fill = region)) +
  geom_density(alpha = 0.5)

ggplot(data = demography, aes(x = region, y = trud_share,
                              fill = region)) +
  geom_violin()
ggplot(data = demography, aes(x = region, y = trud_share,
                              fill = region)) +
  geom_boxplot()
#-------------------------(5)
ggplot(data = demography) +
  geom_point(aes(x = young_share, y = old_share), color = 'red', 
             size = 3) +
  labs(title = "Scatter plot",
       subtitle = "for people younger and older than w.a. people",
       x = "young, %",
       y = "old, %") +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 15)
#-------------------------(6)
demography$male_share <- ((demography$wa_male + demography$ret_male +
                             demography$young_male) / demography$popul_total * 100)
demography$male <- ifelse(demography$male_share >= 50, 1, 0) 
#-------------------------(7)
demography$male <- as.factor(demography$male)
ggplot(data = demography, aes(x = young_share, y = old_share, size = male_share, 
                              color = male)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("yellow", "green"))+
  scale_size(name = "male_share", range = c(.1, 20))

ggplot(data = demography) +
  geom_bar(mapping = aes(region, color = region, fill = region))
