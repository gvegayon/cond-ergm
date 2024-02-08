library(ergm)
library(netplot)

# Rountine to fit ERGM with trycatch
ergm_try <- function(...) {
  tryCatch(
    ergm(...),
    error = function(e) e
  )
}

# Simulation parameters
nsims <- 1e3
ns    <- c(50, 100, 1000, 10000)

ergm_coefs <- c(-6, 1, 1.5)

set.seed(333)

net <- network(n)
net %v% "type" <- rep(c(1,2,3), n/3)

net <- simulate_formula(
  net ~ edges + gwdsp(0.5, fixed = TRUE) + nodematch("type"),
  coef = ergm_coefs
  )

fit1 <- ergm(net ~ edges + gwdsp(0.5, fixed = TRUE) + nodematch("type"))

fit2 <- ergm(
  net ~ edges + gwdsp(0.5, fixed = TRUE) + nodematch("type"),
  constraints = ~ edges
)
