### part (a)
n = 43
y = 15


theta = seq(0, 1, by = 0.01)
priorvalues = dbeta(theta, 8, 2)

plot(theta, dbinom(y, n, theta))

lik <- function(theta){
  choose(n,y)*theta^(y)*(1-theta)^(n-y)
}

plot(theta, lik(theta), type = "l")


bayes <- function(theta){
  (lik(theta)*theta)/y
}

plot(theta, bayes(theta))

mean(theta)
# Create the function.
getmode <- function(theta) {
  uniqv <- unique(theta)
  uniqv[which.max(tabulate(match(theta, uniqv)))]
}


# Calculate the mode using the user function.
result <- getmode(theta)
print(result)

sd(theta)

beta_interval(0.95, c(8,2))