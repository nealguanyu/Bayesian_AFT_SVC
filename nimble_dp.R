##' m: number of locations
##' n: number of observations per location
##' p: dimension of covariate space
##' M: maximum number of clusters assumed in the beginning of stick-breaking

aft_dp <- nimbleCode({
  for (i in 1:m) {
    for (j in 1:n) {
      z[i] ~ dcat(w[1:M])
      logtime[i, j] ~ dnorm(mu[i, j], sigma[i])
      censor[i, j] ~ dinterval(logtime[i, j], censortime[i, j])
      mu[i, j] <- inprod(beta[i, 1:p], X[1:p, i, j])
    }
    sigma[i] ~ dinvgamma(1, 1)
  }
  for (i in 1:(M - 1)) {
    v[i] ~ dbeta(1, alpha)
  }
  alpha ~ dgamma(1, 1)
  w[1:M] <- stick_breaking(v[1:(M - 1)])
  cov[1:p, 1:p] <- diag(100, p)
  for (i in 1:M) {
    beta[i, 1:p] ~ dmnorm(mu_beta[1:p], cov = cov[1:p, 1:p])
  }
})
