SIM <- function(A, u0, f, n_iter = 10e5, eps = 10e-7){
  alpha <- (-1) * A / diag(A)
  diag(alpha) <- 0
  beta <- f / diag(A)
  u <- u0
  for (i in c(1:n_iter)){
    if (is.na(max(abs(beta + alpha %*% u - u)) < eps)){
      print("Решения нет")
      return(u)
    } else if (max(abs(beta + alpha %*% u - u)) < eps){
      return(u)
    } else {
      u <- beta + alpha %*% u
    }
  }
  return(u)
}