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
  counter <- 0
  
  for(x in 1:num_of_points){
    point <- runif(dim, getLowerBoxConstraints(f), getUpperBoxConstraints(f))
    
    result <- optim(point, f, method = "L-BFGS-B", lower = getLowerBoxConstraints(f), upper = getUpperBoxConstraints(f))
    value <- as.numeric(result$value)
    counter <- counter + as.numeric(result$counts[1])
    
    if(value < min_value){
      min_value <- value
    }
  }
  return(list(min_value, counter))
}

compare_algorithms <- function(alg1, alg2, func, dim){
  res_alg1 <- replicate(50, alg1(func, dim, 100))
  alg1_counters <- as.numeric(res_alg1[2,])
  alg1_points <- as.numeric(res_alg1[1,])
  counter <- mean(alg1_counters)
  res_alg2 <- replicate(50, alg2(func, dim, counter))
  
  hist(alg1_points, main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
  hist(res_alg2, main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
  boxplot(alg1_points, main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
  boxplot(res_alg2, main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
  t.test(x = res_alg2, y = alg1_points, conf.level = 0.95)
  
}

compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 2)
compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 2)

compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 10)
compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 10)

compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 20)
compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 20)