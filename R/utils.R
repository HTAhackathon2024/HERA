is_valid_name <- function(x) {
  grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", x)
}

has_description <- function(filenames) {
  "DESCRIPTION" %in% filenames
}

has_tests <- function(filenames) {
  "tests" %in% filenames
}

has_vignettes <- function(filenames) {
  "vignettes" %in% filenames
}

on_cran <- function(name, cran_db) {
  name %in% cran_db$package
}

fill_na <- function(lst, all_names) {
  lst <- as.list(lst)
  filled_list <- lapply(all_names, function(name) {
    if (is.null(lst[[name]])) {
      return(NA)
    } else {
      return(lst[[name]])
    }
  })
  names(filled_list) <- all_names
  return(filled_list)
}
