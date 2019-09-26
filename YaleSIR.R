
library(deSolve)
sir.model <- function(t, x, params) {
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(as.list(params), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    dx <- c(dS, dI, dR)
    list(dx)
  })
}

times <- seq(0, 120, by = 1)
params <- c(beta = 8e-06, gamma = 1/7)
xstart <- c(S = 9999/10000, I = 1/10000, 
            R = 0)
out <- as.data.frame(lsoda(xstart, times, 
                           sir.model, params))

plot(out$time, out$I, ylab = "Population proportion", 
     xlab = "Time", type = "l", bty = "n", 
     ylim = c(0, 1), col = "red", lwd = 2)
lines(out$time, out$S, col = "green", lwd = 2)
lines(out$time, out$R, col = "blue", lwd = 2)
legend(100, 0.5, c("S(t)", "I(t)", "R(t)"), 
       lty = 1, col = c("green", "red", "blue"))