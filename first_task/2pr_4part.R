cars_matrix <- as.matrix(cars)
#----------------------------(1)
ones <- rep(1,nrow(cars_matrix))
cars_speed <- matrix(data = c(ones,cars_matrix[,1]),
                     nrow = nrow(cars_matrix), ncol = 2)
#----------------------------(2)
cars_dist <- cars_matrix[,2]
#----------------------------(3)
alpha <- solve(t(cars_speed) %*% cars_speed) %*% 
  t(cars_speed) %*% cars_dist
#----------------------------(4)
if (is.vector(alpha) == F){
  alpha <- as.vector(alpha)
}
#----------------------------(5)
alpha_c <- alpha[1]
alpha_d <- alpha[2]
cat("alpha_c = ", alpha_c,'\n')
cat("alpha_d = ", alpha_d, '\n')
#----------------------------(6)
cars_speed_lm <- cars_matrix[,1]
#----------------------------(7)
cars_dist_lm <- alpha_c + cars_speed_lm * alpha_d
#----------------------------(8)
dist_residuals <- cars_dist_lm - cars_speed
#----------------------------(9)
mean_dr <- mean(dist_residuals)
sd_dr <- sd(dist_residuals)
#----------------------------(10)
cars_dist_lm
#----------------------------(11)
mean_dr
sd_dr
library(ggplot2)
ggplot(data = as.data.frame(cars_matrix), aes(x = speed, y = dist))+
  geom_point(mapping = aes(x = speed, y = dist), color = "red")+
  geom_line(mapping = aes(x = sort(cars_matrix[,1]),
                          y = cars_dist_lm), color = "blue")
