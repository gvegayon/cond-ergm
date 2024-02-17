library(ergm)
library(netplot)

# Rountine to fit ERGM with trycatch
ergm_try <- function(...) {
  
  # Running while catching
  trun <- system.time({
    res <- tryCatch(
      ergm(...),
      error = function(e) e
    )
  })

  if (inherits(res, "error")) {
    return(res)
  } else {
    list(
      coef = coef(res),
      se   = sqrt(diag(vcov(res))),
      time = trun[3]
    )
  }
}

single_sim <- function(ergm_model, ...) {

  list(
    baseline    = ergm_try(ergm_model),
    constrained = ergm_try(ergm_model, constraints = ~ edges)
  )
  
}

# Simulation parameters
ns    <- c(50, 100, 1000, 10000)
nsims <- 1000/length(ns)

ergm_coefs <- c(-6, 1, 1.5)

set.seed(333)

n <- 50

net <- network(n)
net %v% "type" <- rep(c(1,2,3), n/3)

net <- simulate_formula(
  net ~ edges + gwdsp(0.50, fixed = TRUE) + nodematch("type"),
  coef = ergm_coefs
  )

fit1 <- ergm(net ~ edges + gwdsp(0.50, fixed = TRUE) + nodematch("type"))

fit2 <- ergm(
  net ~ edges + gwdsp(0.5, fixed = TRUE) + nodematch("type"),
  constraints = ~ edges
)
