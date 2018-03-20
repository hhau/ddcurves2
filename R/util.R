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