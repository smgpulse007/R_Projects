# the population size
N = 70

# the needle sharing connection
# probability as a function of the
# average degree
d_avg = 3
q = d_avg/(N - 1)

# the needle sharing rate once per day
lam = 1

# the transmission probability per
# sharing event
p = 0.01

# the transmission rate beta
bet = lam * p

# max time: one year, measured in days
tmax = 365

# set the infection indicator
y = rep(0, N)

# we will set person 1 to be infected at
# baseline
y[1] = 1

# set the infection time
ts = rep(NA, N)
ts[1] = 0

# set up a symmetric zero-diagonal
# contact matrix
A = matrix(0, nrow = N, ncol = N)
for (i in 1:(N - 1)) {
  for (j in (i + 1):N) {
    A[i, j] = rbinom(1, 1, q)
    A[j, i] = A[i, j]
  }
}
A[diag(A)] = 0

tcur = 0

while (tcur < tmax && any(y == 0)) {
  
  # the number of edges connected to each
  # susceptible unit that are attached to
  # an infected unit
  susc_edges = (1 - y) * (A %*% y)
  if (sum(susc_edges) == 0) 
    break
  
  # the waiting time to the next infection
  w = rexp(1, bet * sum(susc_edges))
  
  # if this would put us over the time
  # horizon, stop
  if (tcur + w > tmax) 
    break
  
  # choose the id of the next infected unit
  idx = sample(1:N, 1, prob = susc_edges)
  
  # mark this unit infected
  y[idx] = 1
  
  # record the infection time of the newly
  # infected unit
  ts[idx] = tcur + w
  
  # update the current time
  tcur = tcur + w
}

ts[y == 0] = Inf



plot(sort(ts), 1:N, bty = "n", xlab = "Infection time")



plot_network_infections = function(t, lay) {
  set.seed(1)
  g = network(A, vertex.attr = list(y = ifelse(ts <= 
                                                 t, 1, 0)), matrix.type = "adjacency", 
              directed = FALSE)
  plot(g, vertex.col = ifelse(get.vertex.attribute(g, 
                                                   "y"), "red", "white"), vertex.cex = 2, 
       main = paste("t =", round(t, digits = 2)))
}


tseq = seq(0, max(ts[ts < Inf]), length.out = 9)
par(mfrow = c(3, 3), mar = c(1, 0, 1, 0))

for (t in tseq) {
  plot_network_infections(t, lay)
}




