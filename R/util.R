# Given a series of lists as arguments, merge the lists from left to right (so
# that the right-most values override the left-most)
# thanks github/mbertolacci
.extend_list <- function(...) {
  lists <- list(...)
  output <- lists[[1]]
  for (value in lists[2 : length(lists)]) {
    for (name in names(value)) {
      output[[name]] <- value[[name]]
    }
  }
  return(output)
}

.single_tanh_function <- function(z, beta) {
  
  if (length(beta) != 4) {
    stop("Beta incorrect length")
  }
  
  res <- beta[1] - beta[2] * tanh((z + beta[3]) / beta[4])
  return(res)
  
}

.double_tanh_function <- function(z, beta) {
  
  if (length(beta) != 6) {
    stop("Beta incorrect length")
  }
  
  res <- beta[1] - beta[2] * (tanh((z + beta[3]) / beta[4]) + tanh((z + beta[5]) / beta[6]))  
  return(res)
  
}
