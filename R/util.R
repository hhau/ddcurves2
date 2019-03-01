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

# take an init list, return a jittered version of it
# where each element has jittered by its value * jitter_magnitude
# e.g.: init_jitter(list(a = 1), 0.01) 
#   list(a = with(init_list,
#    runif(
#    1,
#    lower = a - (a * jiiter_magnitude / 2),
#    upper = a + (a * jiiter_magnitude / 2)
#    ))
#' Init jitterer
#' 
#' Jitters the initial values for a list of initial values that is in the form
#' required by \code{\link[rstan:stanmodel-method-sampling]{sampling}}. 
#' \code{jitter_magnitude} controls the magnitude of the jittering, i.e. An 
#' initial value of 1 with \code{jitter_magnitude} = 0.001 will get jittered to
#' be uniformly distributed on the interval between 1 +/- (1 * 0.001 / 2).
#' 
#' @param init_list A list of initial values.
#' @param jitter_magnitude Magnitude of the jittering.
#'
#' @return A jittered version of \code{init_list}.
#' @export
init_jitter <- function(init_list, jitter_magnitude = 0.001) {
  n_params <- length(init_list)
  for (par in seq_len(n_params)) {
    sub_element <- as.array(init_list[[par]])
    sub_dims <- dim(sub_element)
    ## hard code for 1d & 2d here
    if (length(sub_dims) == 1) {
      for (val in seq_len(length(sub_element))) {
        magnitude <- sub_element[val]
        dist <- magnitude * jitter_magnitude / 2 
        init_list[[par]][val] <- runif(
          n = 1,
          min = magnitude - dist,
          max = magnitude + dist
        )
      }
    } else if (length(sub_dims) == 2) {
      for (col in seq_len(sub_dims[2])) {
        for (row in seq_len(sub_dims[1])) {
          magnitude <- sub_element[row, col]
          dist <- magnitude * jitter_magnitude / 2 
          init_list[[par]][row, col] <- runif(
            n = 1,
            min = magnitude - dist,
            max = magnitude + dist
          )
        }
      }
    }
  }
  return(init_list)  
}
