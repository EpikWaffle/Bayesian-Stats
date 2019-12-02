install.packages("TeachBayes")
library(TeachBayes)

bayes_table <- data.frame(p = seq(.3, .8, by=.1),
                          Prior = c(0.125, 0.125, 0.250, 
                                    0.250, 0.125, 0.125))
bayes_table$Likelihood <- dbinom(10, size=30, prob=bayes_table$p)

bayesian_crank(bayes_table) -> bayes_table

bayes_table

sum(bayes_table$Posterior[bayes_table$p == 0.3])

sum(bayes_table$Posterior[bayes_table$p > 0.5])

sum(bayes_table$Posterior[bayes_table$p <= 0.4]) - 
  sum(bayes_table$Posterior[bayes_table$p <= 0.2])