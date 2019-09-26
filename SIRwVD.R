library(deSolve)

######################## SI Model ########################

SI_model <- function(times, yinit, pars) {
  with(as.list(c(yinit, pars)), {
    dS <- lam - beta * I * S - mu_s * S
    dI <- beta * I * S - mu_i * I
    dR <- gam * I - mu_r* R
    return(list(c(dS, dI, dR)))
  })
}

################## Setting Parameters ##################
gam = 1/7
beta = 8e-06  # beta is transmission parameter
mu_s = 1/30  # mu_s is the mortality rate for susceptibles
mu_i = 1/10  # mu_i is the mortality rate for infecteds
mu_r = 1/70
lam = 1000  # lam is the birth rate (assumed not to depend on population size)
pars <- cbind(lam, beta, mu_s, mu_i, mu_r)

############### Setting Time Frame ####################

start_time = 0  # start date
end_time = 200  # end date 
times <- seq(start_time, end_time, by = 1)  # gives a sequence from start to end

############ Setting Initial Conditions ###############

yinit <- c(S = 3000, I = 10, R=100)

############# Running The Model ####################

results <- as.data.frame(ode(y = yinit, times = times, func = SI_model, 
                             parms = pars, method = "rk4"))

############# Plotting Results ######################

plot(results$time, results$S, col = "green", type = "l", bty = "n", 
     lwd = 2, xlab = "Time", ylab = "Number", ylim = range(c(results$S, 
                                                             results$I, results$R)))
lines(results$time, results$I, col = "red", lwd = 3)
lines(results$time, results$R, col = "blue", lwd = 3)
legend(100, 20000, c("Susceptible", "Infective", "Recovered"), col = c("green", 
                                                          "red", "blue"), lwd = 3)