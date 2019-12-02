uh <- seq(-3, 3, length = 1000)

x <- dlogis(uh)
y <- dnorm(uh)

colors <- c("red", "blue")
labels <- c("Normal", "Logistic")

plot(uh, y, type="l", col=colors[1], xlab="values", ylab= "Density", main = "Comparison of Logistic and Normal Distribution")
plot.new=TRUE
lines(uh, x, col=colors[2])

legend("topright", title = "Distributions", labels, lwd = 5, col=colors)

