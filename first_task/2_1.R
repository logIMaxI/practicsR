g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
g[1]
g[length(g)]
g[c(3:5)]
g[g==2 & !is.na(g)]
g[g>4 & !is.na(g)]
g[g%%3==0 & !is.na(g)]
g[(g%%3==0)&(g>4) & !is.na(g)]
g[((g<1)|(g>5)) & !is.na(g)]
which(g==0)
which(g>=2 & g<=8)
a <- g[g!=2]
sort(a,na.last = T)