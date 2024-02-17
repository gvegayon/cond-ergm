# Simulation parameters
ns    <- c(50, 100, 1000, 10000)
nsims <- 1000/length(ns)

ergm_coefs <- c(-6, 1, 1.5)

set.seed(333)

n <- 50


#' Simulate a network using an ERGM model
#'
#' This function generates a simulated network using an Exponential Random
#' Graph Model (ERGM).
#'
#' @param ergm_model Model formula, should include `net`.
#' @param n The number of networks to simulate.
#' @param ergm_coefs The coefficients for the ERGM model.
#'
#' @return A list of simulated networks.
#'
#' @examples
#' sim_net(my_ergm_model, 100, my_ergm_coefs)
#'
sim_net <- function(ergm_model, n, ergm_coefs) {

  net <- network(n)
  net %v% "type" <- rep(c(1,2,3), n/3)

  # Making sure where to evaluate the model
  environment(ergm_model) <- environment()

  simulate_formula(
    ergm_model,
    coef = ergm_coefs
    )

}

