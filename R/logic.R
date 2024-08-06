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
