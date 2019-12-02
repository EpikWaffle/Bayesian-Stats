library(ggplot2)
library(grid)
library(gridExtra)
library(plotrix)

###question 1
set.seed(123)
c_alpha = 70
c_beta = 10
d_alpha = 33.3
d_beta = 3.3
n = 1000

c_gamma <- rgamma(n, c_alpha, c_beta)
d_gamma <- rgamma(n, d_alpha, d_beta)

priors = data.frame(A = c_gamma, B = d_gamma)
ggplot(data=priors, aes(A))+geom_density(color="darkgreen")+geom_density(aes(B), color="blue")

###mean for Chrystal is around 7, mean for Danny is around 9. Chrystal is more confident

qgamma(0.05, c_alpha, c_beta)
qgamma(0.95, c_alpha, c_beta)

qgamma(0.05, d_alpha, d_beta)
qgamma(0.95, d_alpha, d_beta)

###part d, if she decreases alpha and beta by the same amount, she can keep the same ratio for mean, but would increase her variance


Day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
Number_of_ER_Visits = c(8,6,6,9,8,9,7)    
data <- data.frame(Day, Number_of_ER_Visits)

y_sum = sum(Number_of_ER_Visits)
n = 7
s=1000

set.seed(123)
c_post <- rgamma(s, c_alpha + y_sum, c_beta + n)

qgamma(0.05, c_alpha + y_sum, c_beta + n)
qgamma(0.95, c_alpha + y_sum, c_beta + n)


sum(c_post > 6)/s

pred_theta <- rgamma(7, c_alpha + y_sum, c_beta + n)
sum_theta = sum(pred_theta)

pred_delta <- rnbinom(7, size = 70+sum_theta, mu = (70+sum_theta)/(10+n))
pred_delta
                     