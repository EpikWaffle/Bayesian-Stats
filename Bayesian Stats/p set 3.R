library(TeachBayes)
library(ggplot2)

###question 1

n = 1048
y = 692

###part a
p = 8/10

mu_A = p
var_A = (p*(1-p))/10
alpha_A = mu_A*(((mu_A*(1-mu_A))/var_A)-1)
beta_A = (1-mu_A)*((mu_A*(1-mu_A)/var_A)-1)

prior_A = rbeta(1000, alpha_A, beta_A)

###part b
beta.select(list(x = 0.3, p = 0.2),
            list(x = 0.4, p = 0.9))

prior_B = rbeta(1000, 34.95, 67.98)

###part c - alex

posterior_A = rbeta(1000, alpha_A+y, beta_A+n-y)
posterior_B = rbeta(1000, 34.95+y, 67.98+n-y)
posteriors = data.frame(A = posterior_A, B = posterior_B)


beta_interval(0.95, c(alpha_A+y, beta_A+n-y))

###lower bound is 0.633 and upperbound is 0.69

beta_interval(0.95, c(34.95+y, 67.98+n-y))

###lower bound is 0.604 and upperbound is 0.659

priors = data.frame(A = prior_A, B = prior_B)

ggplot(data=priors, aes(A))+geom_density(color="darkgreen")+geom_density(aes(B), color="blue")

ggplot(data=posteriors, aes(A))+geom_density(color="darkgreen") + geom_density(aes(B), color="blue")



###part d - alex
S = 1000
a <- alpha_A; b <- beta_A
n <- 1048; y <- 692
newy = as.data.frame(rep(NA,S))
names(newy) = c("y")

set.seed(123)
for (s in 1:S){
  pred_p_sim <- rbeta(1, a + y, b + n - y)
  pred_y_sim <- rbinom(1, n, pred_p_sim)
  newy[s,] = pred_y_sim
}

sum(newy > y)/S
1 - sum(newy > y)/S
###both probabilities here are almost 0.5 so not small. very good

###part d - benedict
S= 1000
a2 <- 34.95; b2 <- 67.98
n <- 1048; y <- 692
newy2 = as.data.frame(rep(NA,S))
names(newy2) = c("y")

set.seed(123)
for (s in 1:S){
  pred_p_sim <- rbeta(1, a2 + y, b2 + n - y)
  pred_y_sim <- rbinom(1, n, pred_p_sim)
  newy2[s,] = pred_y_sim
}

sum(newy2 > y)/S
1 - sum(newy2 > y)/S

###one is 0.075 other is 0.925. very big so not good


###question 2

### part a
set.seed(123)
S = 1000
BetaSamples = rbeta(S, alpha_A  + y, beta_A + n - y)
odds = BetaSamples / (1 - BetaSamples)

mean(odds)
###equals  1.953566
median(odds)
###equals 1.948986
quantile(odds, c(0.025, 0.975))
###lowerbound is 1.710385, upperbound is 2.175273

###part b
set.seed(123)
S = 1000
BetaSamples = rbeta(S, 726.95, 1115.98)
odds = BetaSamples / (1 - BetaSamples)

mean(odds)
###equals 0.6534631
median(odds)
###equals 0.6529434
quantile(odds, c(0.025, 0.975))
###lowerbound is 0.5953948, upperbound is 0.7184256

###question 3
set.seed(123)
S1 = 10
BetaSamples = rbeta(S1, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))
###lowerbound is 0.3817706, upperbound is 0.6560736

set.seed(123)
S2 = 100
BetaSamples = rbeta(S2, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))
###lowerbound is 0.4360145, upperbound is 0.7117032

set.seed(123)
S3 = 500
BetaSamples = rbeta(S3, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))
###lowerbound is 0.4287164, upperbound is 0.7273517

set.seed(123)
S4 = 1000
BetaSamples = rbeta(S4, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))
###lowerbound is 0.4266076, upperbound is 0.7333957

set.seed(123)
S5 = 5000
BetaSamples = rbeta(S5, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))
###lowerbound is 0.4266577, upperbound is 0.7416441

###question 5
###growth group
mu_0a = 0
sigma_0a = sqrt(20)
phi_0a = 1/20

ybara = 15.2
phia = 1/20
n = 6

mu_na = (phi_0a*mu_0a+n*ybara*phia)/(phi_0a+n*phia)
sd_na = sqrt(1/(phi_0a+n*phia))

set.seed(123)
S = 1000
mu_posta = rnorm(S, mean = mu_na, sd = sd_na)

###no growth group

mu_0n = 0
sigma_0n = sqrt(20)
phi_0n = 1/20

ybarn = 6.2
phin = 1/20
n = 6

mu_nn = (phi_0n*mu_0n+n*ybarn*phin)/(phi_0n+n*phin)
sd_nn = sqrt(1/(phi_0n+n*phin))

set.seed(123)
S = 1000
mu_postn = rnorm(S, mean = mu_nn, sd = sd_nn)

mu_postdiff <- setdiff(mu_postn, mu_posta)

sum(mu_postdiff > 0)/S

### prob is 1

###part b

S=1000
predA = numeric(S)
predN = numeric(S)
for(s in 1:S){
  predA[s] = mean(rnorm(6, mu_posta[s],4))
  
  predN[s] = mean(rnorm(6, mu_postn[s],4))
  
}

sum(predA>predN)/1000

### prob is 0.998