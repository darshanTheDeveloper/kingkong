
diff.mean.power <- function(x,mean.data,power_value){
  data <- x - mean.data
  return(data^power_value)
}


standard_deviation <- function(data){
  mean.data <- mean(data)
  x <- diff.mean.power(data,mean.data,2)
  return(sqrt(sum(x)/(length(data)-1)))
}

skewness <- function(data){
  sd <- standard_deviation(data)
  mean.data <- mean(data)
  x2 <- diff.mean.power(data,mean.data,3)
  return(sum(x2)/((length(data)-1)*sd^3))
}

kurtosis <- function(data){
  sd <- standard_deviation(data)
  mean.data <- mean(data)
  x3 <- diff.mean.power(data,mean.data,4)
  return(sum(x3)/((length(data)-1)*sd^4))
}

diff.mean.power <- function(x,mean.data,power_value){
  data <- x - mean.data
  return(data^power_value)
}


standard_deviation <- function(data){
  mean.data <- mean(data)
  x <- diff.mean.power(data,mean.data,2)
  return(sqrt(sum(x)/(length(data)-1)))
}

skewness <- function(data){
  sd <- standard_deviation(data)
  mean.data <- mean(data)
  x2 <- diff.mean.power(data,mean.data,3)
  return(sum(x2)/((length(data)-1)*sd^3))
}

kurtosis <- function(data){
  sd <- standard_deviation(data)
  mean.data <- mean(data)
  x3 <- diff.mean.power(data,mean.data,4)
  return(sum(x3)/((length(data)-1)*sd^4))
}
