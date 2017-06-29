library(shiny)
library(C3)
library(shinyBS)
library(shinythemes)

getSeries <- function(s0, mu = 0, sigma, nsims, periods = seq(1/365, 1, by = 1/365)){
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  drift = mu - 0.5 * sigma ^ 2
  y = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc = nsims)
  for (i in 2:nsteps) y[i, ] = y[i, ] * y[(i-1), ]
  s0 * y
}