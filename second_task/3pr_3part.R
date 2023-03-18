#---------------------------(1)
library(car)
data_orn <- Ornstein
#---------------------------(2)
summary(data_orn)
str(data_orn)
#---------------------------(3)
data_orn[!complete.cases(data_orn),]
#---------------------------(4)
data_a <- data_orn[data_orn$assets <= 20000 & data_orn$assets >= 10000, ]
data_b <- data_orn[data_orn$interlocks <= 30, ]
data_c <- data_orn[data_orn$sector == "TRN" & data_orn$nation=="CAN", ]
#---------------------------(5)
data_orn$log_assets <- log(data_orn$assets)
#---------------------------(6)
library(ggplot2)
ggplot(data = data_orn)+
  geom_line(mapping = aes(x = interlocks, y = assets, color = nation))
#---------------------------(6)
write.table(x = data_orn, file = "Firms.dta")
