prs_algorithm <- function(fun, dim, num_of_points){
  f <- fun(dim);
  min_value <- Inf
  
  for(x in num_of_points){
    point <- runif(dim, getLowerBoxConstraints(f), getUpperBoxConstraints(f))
    value <- f(point)
    
    if(value < min_value){
      min_value <- value
    }
  }
  
  return(min_value)
}

ms_algorithm <- function(fun, dim, num_of_points){
  f <- fun(dim);
  min_value <- Inf
  
  for(x in num_of_points){
    point <- runif(dim, getLowerBoxConstraints(f), getUpperBoxConstraints(f))
    
    value <- as.numeric(optim(point, f, method = "L-BFGS-B", lower = getLowerBoxConstraints(f), upper = getUpperBoxConstraints(f))["value"])
    
    if(value < min_value){
      min_value <- value
    }
  }
  
  return(min_value)
}

prs_mean <- mean(replicate(50, prs_algorithm(makeAlpine01Function, 2, 1000)))
ms_mean <- mean(replicate(50, ms_algorithm(makeAlpine01Function, 2, 1000)))