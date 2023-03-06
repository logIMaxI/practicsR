#----------------------------(1)
A <- diag(x = c(4,9),2,2)
rownames(A) <- c("eq1","eq2")
colnames(A) <- c("x1","x2")
#----------------------------(2)
values <- eigen(A,only.values = T)
values
#----------------------------(3)
B <- diag(x=1,2,2)-A
B
#----------------------------(4)
f <- c(4,2)
u <- c(0.2,-0.3)
#----------------------------(5)
u_result <- solve(A,f)
u_result
#----------------------------(6)
for (i in 1:7){
  u <- B%*%u + f
}
#----------------------------(7)
u_first <- u
u_first
u_result
#----------------------------(8)
f <- f/max(A)
A <- A/max(A)
#----------------------------(9)
values <- eigen(A,only.values = T)
values
B <- diag(x=1,2,2)-A
B
u_result <- solve(A,f)
u_result
u <- c(0.2,-0.3)
for (i in 1:7){
  u <- B%*%u + f
}
#----------------------------(10)
u
u_first


