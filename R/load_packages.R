#' @description 
#' Read in packages.csv and filter for entries with valid package names.
#' packages.csv contains a set of package name and associated GitHub URL 
#' fragment (owner/repo_name) values.
#' 
#' packages.csv comprises a fixed placeholder list - Functionality to be replaced by 
#' the GitHub search query in due course.
#' 
#' @return A data frame of package name and associated GitHub URL 
#' path fragment values.
get_packages <- function(path = "config/packages.csv") {
  
  # Read in packages.csv file and add col names.
  packages <- read.csv(path,
                       header = FALSE,
                       col.names = c("name", "url")
                       )
  
  # Filter on name values - Begins with alphabetic and ends with alphanumeric chr.
  packages <- packages[is_valid_name(packages$name), ]
  
}

#' @description 
#' Sources package data from CRAN database using tools::CRAN_package_db - 
#' Uses cached data, if available.
#' 
#' @param packages A data frame of package name and associated GitHub URL 
#' fragment values.Currently created by [get_packages()].
#' 
#' @param use_cache Optional Logical. Default TRUE - Use cache.
#' 
#' @return A data frame of package data:
#'              - "author"
#'              - "package"
#'              - "title" 
#'              - "description"
#'              - "license"
#'              - "date/publication"
#'              - "maintainer"
#'              - "reverse_depends"
get_cran_db <- function(packages, use_cache = TRUE) {
  
  # Set cran_db value to NULL
  cran_db <- NULL
  
  # If use cache and cran_db.rds exists
  if (use_cache & file.exists("cache/cran_db.rds")) {
    # Read in cran_db.rds.
    cached <- readRDS("cache/cran_db.rds")
    # Update cran_db value - Assign cran_db element from cached.
    cran_db <- cached$cran_db
    # Check package names in cached data are identical to name values in packages data frame.
    # Set cran_db to NULL, if not identical.
    if (!identical(cached$package_names, packages$name)) {
      cran_db <- NULL
    }
    
  }
  
  # If cran_db is NULL
  if (is.null(cran_db)) {
    # Source package data from CRAN DB.
    logger::log_info("Package list has changed, or cache not available. Fetching fresh CRAN db.")
    cran_db <- tools::CRAN_package_db()
    # Filter Package field value on name values in packages.
    cran_db <- cran_db[cran_db$Package %in% packages$name,]
    logger::log_info("Filtering CRAN database")
    # Select required parameter columns.
    cran_db <- cranly::clean_CRAN_db(cran_db)[, c("author",
                                                  "package",
                                                  "title",
                                                  "description",
                                                  "license",
                                                  "date/publication",
                                                  "maintainer",
                                                  "reverse_depends")
                                              ]
    logger::log_info("Updating cache")
    # Create "cache" dir if it doesn't exist.
    if (!dir.exists("cache")) {
      dir.create("cache")
    }
    
    # Cache sourced package data - Write list with elements for package names and cran_db as rds file to cache.
    saveRDS(list(package_names = packages$name, cran_db = cran_db), "cache/cran_db.rds")
  }
  
  # Return cran_db value.
  cran_db
  
}
