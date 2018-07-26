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

#' Date a filename 
#' 
#' This function will create the directories in the full file path it 
#' constructs, and return the full file path to the specified file.
#' 
#'
#' @param filename the filename, with file extensions
#' @param folder_path the directory for the file, defaults to "./plots/"
#' @param date a date string, defaults to Sys.Date()
#' @param sep seperator character, if set to "" and combined with a leading "/"
#' in `filename`, then dated directories will be created.
#'
#' @return a filepath/filename string
#' @export
dated_filename <- function(filename, folder_path = "./plots/", date = Sys.Date(), sep = '_') {
  full_path <- file.path(folder_path, sprintf('%s%s%s', date, sep, filename))
  
  if (!dir.exists(dirname(full_path))) {
    dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
  }
  
  return(full_path)
  
}
