#Monte Carlo Integration
set.seed(2718)

#The function we wish to integrate
w <- function(t){
  (16-t^2)^0.5 - 3
}

#Bounds of integration (a,b)
a <- -sqrt(7)
b <- sqrt(7)
#set sample size and generate N observations from U(a,b)
N <- 1000000
obs <- runif(N,a,b)

#Define I as the expected value of our function we want to integrate.
I <- 1/N*sum(w(obs)*(b-a))
I

#Determine s and SE to define our 95% CI inverval
s <- sqrt( sum( (w(obs) - I)^2/(N-1) ) )
se <- s/sqrt(N)
lower <- I-qnorm(1-.05/2)*se
upper <- I+qnorm(1-.05/2)*se
