income <- c(10000, 32000, 28000, 150000, 65000, 1573)
income <- ifelse(income > mean(income),1,0)
income