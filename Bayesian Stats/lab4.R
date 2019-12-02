
library(runjags)
library(ggplot2)
library(coda)

setwd("C:/Users/Gorge-PC/Documents")
dramadata = read.csv("KDramaData.csv", header=T)

##question 1

modelString <-"
model {

## likelihod
for(i in 1:N){
  y[i] ~ dnorm(mu_j[schedule[i]], invsigma2)
}

## priors

for (j in 1:J){
  mu_j[j] ~ dnorm(mu, invtau2)T(0,)
}

invsigma2 ~ dgamma(a_g, b_g)
sigma <- sqrt(pow(invsigma2, -1))

##hyperpriors
mu ~ dnorm(mu0, 1/g0^2)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))

}
"

y = dramadata$Rating        # The y values are in the column named Rating
schedule = dramadata$Schedule      # The schedule index is in the column named Schedule
N = length(y)  # Compute the number of observations
J = length(unique(schedule)) # Compute the number of schedules/subsamples

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

the_data <- list("y" = y, "schedule" = schedule, "N" = N, "J" = J, 
                 "mu0" = 0.1, "g0" = 0.5, 
                 "a_t" = 1, "b_t" = 1,
                 "a_g" = 1, "b_g" = 1)

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu", "tau", "mu_j", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      thin = 1, 
                      inits = initsfunction)

options(digits = 3)
summary(posterior)

plot(posterior, vars = "mu_j[1]")


## question 2

modelString <-"
model {

## likelihod
for(i in 1:N){
y[i] ~ dnorm(mu_j[schedule[i]], invsigma2)
}

## priors

for (j in 1:J){
mu_j[j] ~ dlnorm(mu, invtau2)T(0,)
}

invsigma2 ~ dgamma(a_g, b_g)
sigma <- sqrt(pow(invsigma2, -1))

##hyperpriors
mu ~ dlnorm(mu0, 1/g0^2)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))

}
"

y = dramadata$Rating        # The y values are in the column named Rating
schedule = dramadata$Schedule      # The schedule index is in the column named Schedule
N = length(y)  # Compute the number of observations
J = length(unique(schedule)) # Compute the number of schedules/subsamples

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

the_data <- list("y" = y, "schedule" = schedule, "N" = N, "J" = J, 
                 "mu0" = 0.1, "g0" = 0.5, 
                 "a_t" = 1, "b_t" = 1,
                 "a_g" = 1, "b_g" = 1)

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu", "tau", "mu_j", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      thin = 1, 
                      inits = initsfunction)

options(digits = 3)
summary(posterior)

plot(posterior, vars = "mu_j[1]")

##question 3
modelString <-"
model {

## likelihod
for(i in 1:N){
y[i] ~ dnorm(mu_j[schedule[i]], invsigma2)
}

## priors

for (j in 1:J){
mu_j[j] ~ dweibull(1, mu)T(0,)
}

invsigma2 ~ dgamma(a_g, b_g)
sigma <- sqrt(pow(invsigma2, -1))

##hyperpriors
mu ~ dlnorm(mu0, 1/g0^2)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))

}
"

y = dramadata$Rating        # The y values are in the column named Rating
schedule = dramadata$Schedule      # The schedule index is in the column named Schedule
N = length(y)  # Compute the number of observations
J = length(unique(schedule)) # Compute the number of schedules/subsamples

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

the_data <- list("y" = y, "schedule" = schedule, "N" = N, "J" = J, 
                 "mu0" = 0.1, "g0" = 0.5, 
                 "a_t" = 1, "b_t" = 1,
                 "a_g" = 1, "b_g" = 1)

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu", "tau", "mu_j", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      thin = 1, 
                      inits = initsfunction)

options(digits = 3)
summary(posterior)

plot(posterior, vars = "mu_j[1]")
