#Here we use R for the Bayesian posterior distribution with binomial data and a discrete prior.

#A script to estimate the posterior distribution has been created and provided here. 
#You can copy this directly into R and it will run all the commands. 

#Bayesian inference for all-nighters problem

priorvalues = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
priorprob = c(0, 1/13, 2/13, 2/13, 2/13, 2/13, 2/13, 2/13, 0, 0, 0)

n = 20
y = 12

#vector for storing results
jointprob = numeric(length = length(priorvalues))

for(i in 1:length(priorvalues))
{
  
  #compute binomial probability given value of p
  binomprob = dbinom(y, n, p = priorvalues[i])
  
  #compute joint probability
  jointprob[i] = binomprob * priorprob[i]
  
}

#compute marginal probability of y 
pofy = sum(jointprob)

#compute posterior probabilities
posteriorprob = jointprob/pofy

#put posterior probabilities in one matrix object for easy viewing 
allnighterposterior = cbind(priorvalues, priorprob, posteriorprob)


#list the final posterior distribution, based on our prior derived in class
allnighterposterior

#plot the prior and posterior probabilities
plot(as.data.frame(allnighterposterior)$priorprob ~ as.data.frame(allnighterposterior)$priorvalues, xlab = "values", ylab = "probability",
     main = "prior and posterior probabilities", col = "red", ylim = c(0,0.5), pch = 1)
points(as.data.frame(allnighterposterior)$posteriorprob ~ as.data.frame(allnighterposterior)$priorvalues, col="blue", pch = 4)
legend("topright", legend=c("Prior", "Posterior"),
       col=c("red", "blue"), pch=c(1,4), cex=0.8)

