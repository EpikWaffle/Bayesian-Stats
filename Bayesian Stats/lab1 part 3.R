### part (c)

a1 = 2
b1 = 8
a2 = 8
b2 = 2

theta = seq(0, 1, by = 0.001)

plot(x = theta, y = 0.75*dbeta(theta, a1, b1) + 0.25*dbeta(theta, a2, b2))