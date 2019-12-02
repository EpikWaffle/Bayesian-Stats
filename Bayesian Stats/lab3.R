library(VGAM)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotrix)

set.seed(123)

n = 16
a = 2
b = 4
m = 500

beta_binom <- rbetabinom.ab(m, n, a, b, .dontuse.prob = NULL)
hist(beta_binom, breaks = 30, col="#bccbe2")

#run 500 chains with k = 10


set.seed(123)
k = 10
beta_binom_gibbs = rep(NA, m)
for (i in 1:m){
  y = rbeta(1, 1, 1)
  for (s in 1:k){
    x = rbinom(1, n, y)
    y = rbeta(1, x+a, n-x+b)
  }
  beta_binom_gibbs[i] = x
}

hist(beta_binom_gibbs, breaks=30, col="#db9159")

l <- list(beta_binom, beta_binom_gibbs)
multhist(l, breaks = seq(-0.5, 16.5, by=1), col=c("#bccbe2","#db9159"))
