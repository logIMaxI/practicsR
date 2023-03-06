write_in_file <- function(path, step_x,
                          dekart_begin_x, dekart_end_x,
                          step_y, dekart_begin_y,
                          dekart_end_y){
  x <- seq(from = dekart_begin_x,to = dekart_end_x,
           by = step_x)
  y <- seq(from = dekart_begin_y, to = dekart_end_y,
           by = step_y)
  surface_matrix <- outer(X = x,Y = y,FUN = function(x,y)
    Re(exp(-1i*0.5*x*y)))
  dimnames(surface_matrix) <- list(x,y)
  write(x = "number of matrix elements: ", file = path,
        sep = " ")
  write(x = ncol(surface_matrix)*nrow(surface_matrix), 
        file = path, sep = " ", append = T)
  write(x = "number of rows: ", file = path,
        sep = " ", append = T)
  write(x = nrow(surface_matrix), file = path, 
        sep = " ", append = T)
  write(x = "number of cols: ", file = path,
        sep = " ", append = T)
  write(x = ncol(surface_matrix), file = path, 
        sep = " ", append = T)
  write(x = "sum of main diag elements: ", file = path,
        sep = " ", append = T)
  write(x = sum(diag(surface_matrix)), file = path, 
        sep = " ", append = T)
  write(x = "row sums: ", file = path, sep = " ", append = T)
  write(x = apply(X = surface_matrix, MARGIN = 1,FUN = sum),
        file = path, sep = " ", append = T)
  write(x = "col sums: ", file = path, sep = " ", append = T)
  write(x = apply(X = surface_matrix, MARGIN = 2,FUN = sum),
        file = path, sep = " ", append = T)
  return (surface_matrix)
}
#----------------------------(1)
surface_matrix <- write_in_file("summary.txt",1,-5,5,1,-5,5)
path <- "summary.txt"
write(x = "sum of middle row elements: ",
      file = path, sep = " ", append = T)
result <- sum(surface_matrix[,ncol(surface_matrix)/2 + 
                               ncol(surface_matrix) %% 2]) 
write(x = result, file = path, sep = " ", append = T)
write(x = "sum of middle column elements: ",
      file = path, sep = " ", append = T)
result <- sum(surface_matrix[nrow(surface_matrix)/2 + 
                               nrow(surface_matrix) %% 2,])
write(x = result, file = path, sep = " ", append = T)
#----------------------------(2)
step <- as.numeric(readline(
  prompt = "Введите параметр step: "))
dekart_begin <- as.numeric(readline(
  prompt = "Введите параметр dekart_begin: "))
dekart_end <- as.numeric(readline(
  prompt = "Введите параметр dekart_end: "))
write_in_file("summary2.txt", step, dekart_begin, dekart_end,
              step, dekart_begin, dekart_end)
#----------------------------(3)
data <- as.numeric(read.table(file = "inputs.txt",
                              sep = " "))
write_in_file("summary3.txt",data[1],data[2],data[3],
              data[4],data[5],data[6])




