library(deSolve)

######################## SI Model ########################

SI_model <- function(times, yinit, pars) {
  with(as.list(c(yinit, pars)), {
    dS <- lam - beta * I * S - mu_s * S
    dE <- beta * I * S - mu_e * E - del * E
    dI <- del * E - mu_i * I - gam * I
    dR <- gam * I - mu_r* R
    return(list(c(dS, dE, dI, dR)))
  })
}

################## Setting Parameters ##################
del = 1/10
gam = 1/7
beta = 8e-06  # beta is transmission parameter
mu_s = 1/30  # mu_s is the mortality rate for susceptibles
mu_i = 1/10  # mu_i is the mortality rate for infecteds
mu_r = 1/100
mu_e = 1/70
lam = 1000  # lam is the birth rate (assumed not to depend on population size)
pars <- cbind(lam, beta, del, gam, mu_s, mu_e, mu_i, mu_r)

############### Setting Time Frame ####################

start_time = 0  # start date
end_time = 200  # end date 
times <- seq(start_time, end_time, by = 1)  # gives a sequence from start to end

############ Setting Initial Conditions ###############

yinit <- c(S = 300000, E = 1000, I = 10, R=100)

############# Running The Model ####################

results <- as.data.frame(ode(y = yinit, times = times, func = SI_model, 
                             parms = pars, method = "rk4"))

############# Plotting Results ######################

plot(results$time, results$S, col = "green", type = "l", bty = "n", 
     lwd = 2, xlab = "Time", ylab = "Number", ylim = range(c(results$S, 
                                                             results$I, results$R, results$E)))
lines(results$time, results$I, col = "red", lwd = 3)
lines(results$time, results$R, col = "blue", lwd = 3)
lines(results$time, results$E, col = "black", lwd = 3)
legend("topright", c("Susceptible", "Infective", "Recovered", "Exposed"), col = c("green", 
                                                                       "red", "blue", "black"), lwd = 3)