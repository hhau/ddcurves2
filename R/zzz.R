.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  # print(names(stanmodels))
  # print(modules)
  for (m in modules) {
    # print(m)
    Rcpp::loadModule(m, what = TRUE)
  }
}
