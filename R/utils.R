#' @description
#' Validate a Name string value. 
#' 
#' Checks the passed Name value begins with an alphabetic and ends with an alphanumeric chr.
#' Allows for alphanumeric or periods within.
#' 
#' @param x A character string to be validated.
#' 
#' @return A logical value: \code{TRUE} if the string is a valid name, \code{FALSE} otherwise.
is_valid_name <- function(x) {
  grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", x)
}

#' @description
#' Checks for the presence of a "DESCRIPTION" file in filenames.
#' 
#' @param filenames A character vector of file names.
#' 
#' @return A logical value: \code{TRUE} if "DESCRIPTION" is found in the vector, \code{FALSE} otherwise.
has_description <- function(filenames) {
  "DESCRIPTION" %in% filenames
}

#' @description
#' Checks for the presence of a "tests" file in filenames.
#' 
#' @param filenames A character vector of file names.
#' 
#' @return A logical value: \code{TRUE} if "tests" is found in the vector, \code{FALSE} otherwise.
has_tests <- function(filenames) {
  "tests" %in% filenames
}

#' @description
#' Checks for the presence of a "vignettes" file in filenames.
#' 
#' @param filenames A character vector of file names.
#' 
#' @return A logical value: \code{TRUE} if "vignettes" is found in the vector, \code{FALSE} otherwise.
has_vignettes <- function(filenames) {
  "vignettes" %in% filenames
}

#' @description
#' Checks if a package is available from the CRAN database.
#' 
#' @param name      A character string for the package name.
#' @param cran_db   A data frame containing CRAN package data with a \code{package} column
#'                  that details the names of packages on CRAN.
#' 
#' @return A logical value: \code{TRUE} if the package \code{name} is found in the \code{cran_db} data frame, \code{FALSE} otherwise.
on_cran <- function(name, cran_db) {
  name %in% cran_db$package
}

#' @description
#' Create a "filled" list of package metadata, given a set of target variables.
#' 
#' @param lst         A list of package metadata.
#'
#' @param all_names   A character vector of target variables.
#' 
#' @return A "filled" list of package metadata.
fill_na <- function(lst, all_names) {
  
  # Convert lst to list.
  lst <- as.list(lst)
  
  # Create standardized (i.e., filled) metadata list populating any missing elements with NA; else the corresponding value in lst.
  filled_list <- lapply(all_names, function(name) {
    if (is.null(lst[[name]])) {
      return(NA)
    } else {
      return(lst[[name]])
    }
  })
  
  # Re-name to target vars.
  names(filled_list) <- all_names
  
  # Return filled list.
  return(filled_list)
  
}
