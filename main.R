prs_algorithm <- function(fun, dim, num_of_points, min_range, max_range){
  f <- fun(dim);
  min_value <- Inf
  min_point <- Inf
  
  for(x in num_of_points){
    point <- runif(dim, min_range, max_range)
    value <- f(point)
    
    if(value < min_value){
      min_value <- value
      min_point <- point
    }
  }
  
  return(min_value)
}

prs_mean <- mean(replicate(50,prs_algorithm(makeAlpine01Function, 2, 50, -10, 10)))