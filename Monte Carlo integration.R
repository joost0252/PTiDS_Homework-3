f <- function(x) {
  return(exp(x) * log(x))
}

find_integral <- function(f, xlim, N = 1000, seed = 10, make_plot = TRUE){
  if (missingArg(f)) {
    f <- function(x) {exp(x) * log(x)}
  }
  if (missingArg(xlim)) {
    xlim <- c(0.1, 2)
  } else if (!is.numeric(xlim) || length(xlim) != 2) {
    stop('xlim is not properly defined')
  }
  # Control seed
  set.seed(seed)
  
  # Simulate N points
  U = runif(N, min = 0.1, max = 2)
  N=N
  for (i in 1:length(U)){
    fi[i] = f(U[i])
  }
  
  I = 1.9 * (1/N) * sum(f(U))
  

  
  #plot(f(x), fi)
  
  integral_app <-   plot(f, type="l", col="blue", ylim=c(-3,5), xlim=c(0.1,2))
  lines(c(0,2.1), c(0,0), type="l", col="black")
  
  for (i in 1:length(U)){
  lines(c(U[i],U[i]), c(fi[i],0), type="l", pch=22, col="red")
  } 
  return(integral_app)
}

find_integral()