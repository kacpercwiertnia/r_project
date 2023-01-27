prs_algorithm <- function(fun, dim, num_of_points){
  f <- fun(dim);
  min_value <- Inf
  
  for(x in 1:num_of_points){
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
  
  return(list(alg1_points, res_alg2))
}

res_alpine01_2 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 2)
hist(res_alpine01_2[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_alpine01_2[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_alpine01_2[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_alpine01_2[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_alpine01_2[[1]], conf.level = 0.95)
t.test(x = res_alpine01_2[[2]], conf.level = 0.95)
t.test(x = res_alpine01_2[[2]], y = res_alpine01_2[[1]], conf.level = 0.95)

res_ackley_2 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 2)
hist(res_ackley_2[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_ackley_2[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_ackley_2[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_ackley_2[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_ackley_2[[1]], conf.level = 0.95)
t.test(x = res_ackley_2[[2]], conf.level = 0.95)
t.test(x = res_ackley_2[[2]], y = res_ackley_2[[1]], conf.level = 0.95)

res_alpine01_10 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 10)
hist(res_alpine01_10[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_alpine01_10[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_alpine01_10[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_alpine01_10[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_alpine01_10[[1]], conf.level = 0.95)
t.test(x = res_alpine01_10[[2]], conf.level = 0.95)
t.test(x = res_alpine01_10[[2]], y = res_alpine01_10[[1]], conf.level = 0.95)

res_ackley_10 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 10)
hist(res_ackley_10[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_ackley_10[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_ackley_10[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_ackley_10[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_ackley_10[[1]], conf.level = 0.95)
t.test(x = res_ackley_10[[2]], conf.level = 0.95)
t.test(x = res_ackley_10[[2]], y = res_ackley_10[[1]], conf.level = 0.95)

res_alpine01_20 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAlpine01Function, 20)
hist(res_alpine01_20[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_alpine01_20[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_alpine01_20[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_alpine01_20[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_alpine01_20[[1]], conf.level = 0.95)
t.test(x = res_alpine01_20[[2]], conf.level = 0.95)
t.test(x = res_alpine01_20[[2]], y = res_alpine01_20[[1]], conf.level = 0.95)

res_ackley_20 <- compare_algorithms(ms_algorithm, prs_algorithm, makeAckleyFunction, 20)
hist(res_ackley_20[[1]], main = "Histogram algorytmu MS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
hist(res_ackley_20[[2]], main = "Histogram algorytmu PRS", xlab = "Zakresy minimów", ylab = "Ilość minimów")
boxplot(res_ackley_20[[1]], main = "Wykres pudełkowy algorytmu MS", ylab = "Zakres minimów")
boxplot(res_ackley_20[[2]], main = "Wykres pudełkowy algorytmu PRS", ylab = "Zakres minimów")
t.test(x = res_ackley_20[[1]], conf.level = 0.95)
t.test(x = res_ackley_20[[2]], conf.level = 0.95)
t.test(x = res_ackley_20[[2]], y = res_ackley_20[[1]], conf.level = 0.95)
