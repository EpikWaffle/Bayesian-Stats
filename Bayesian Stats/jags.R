library(runjags)

modelString = "
model{
for (i in 1:N) {
y[i] ~ dnorm(mu, phi)
}
mu ~ dnorm(mu_0, phi_0)
phi ~ dgamma(alpha, beta)
}
"
writeLines(modelString, con="norm_model.jags")

y <- CEdata$LogTotalExpLastQ
N <- length(y)
the_data <- list("y" = y, "N" = N, "mu_0"=5,"phi_0"=1/1^2, "alpha"=1,"beta"=1)


posterior <- run.jags('norm_model.jags',
                      data = the_data,
                      monitor = c("mu", "phi"),
                      n.chains = 1,
                      adapt = 1000,
                      burnin = 2000,
                      sample = 5000,
                      thin = 1)

summary(posterior)