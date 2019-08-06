##' m: number of locations
##' n: number of observations per location
##' p: dimension of covariate space
##' low: lambda1 in Equation (7)
##' high: lambdan in Equation (7)


aft_car <- nimbleCode({
  for (i in 1:m) {
    for (j in 1:n) {
      logtime[i, j] ~ dnorm(mu[i, j], sigma[i])
      censor[i, j] ~ dinterval(logtime[i, j], censortime[i, j])
      mu[i, j] <- inprod(beta[i, 1:p], X[1:p, i, j])
    }
    sigma[i] ~ dinvgamma(1, 1)
  }
  correlation[1:m, 1:m] <- inverse(DD[1:m, 1:m] - b * W[1:m, 1:m])
  b ~ dunif(low, high)
  for (i in 1:p) {
    covariance[i, 1:m, 1:m] <- sigmabeta[i] * correlation[1:m, 1:m]
    beta[1:m, i] ~ dmnorm(mu_beta[1:m], cov = covariance[i, 1:m, 1:m])
    sigmabeta[i] ~ dinvgamma(1, 1)
  }
})