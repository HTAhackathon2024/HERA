#' @description 
#' Safely (i.e., if present) retrieve field value from DESCRIPTION file.
#' 
#' @param parsed A DESCRIPTION file - An object of class \code{R6} and \code{description}.
#' @param field A character string of length 1 for the field key.
#'  
#' @return \code{NA} if the field is not present; else, an un-named vector for the field value.
get_field_safely <- function(parsed, field) {
  # Use has_fields method to determine if field is present.
  if (parsed$has_fields(field)) {
    # Use get_field method to retrieve field value.
    return(parsed$get_field(field))
  } else { 
    # Else, return NA.
    return(NA)
  }
}

#' @description
#' Compiles all target metadata across the GitHub repo and CRAN database, if applicable.
#' For packages on CRAN, the CRAN database is used as primary for core metadata endpoints.
#' Otherwise, metadata is from the GitHub DESCRIPTION file contents and other API endpoint queries.
#' 
#' @param gh_data A metadata list object of class github_metadata.
#' @param cran_db A metadata list object of class cran_metadata.
#' @param description_contents Package DESCRIPTION file contents from GitHub; defaults to \code{NULL}
#' 
#' @return A list of target metadata. Includes:
#'          - The package name;
#'          - The title field;
#'          - The description field;
#'          - The license field;
#'          - The authors;
#'          - The maintainer;
#'          - The data published;
#'          - Whether a tests folder is present;
#'          - Whether a vignettes folder is present;
#'          - The number of contributors;
#'          - Whether or not the package is on CRAN.
compile_metadata <- function(gh_data, cran_data = NULL, description_contents = NULL) {
  
  # Input validation: check gh_data param is of appropriate class; stop if not.
  if (!("github_metadata" %in% class(gh_data))) {
    stop("gh_data must be of type 'github_metadata'")
  }
  
  # Set bln for on_cran - TRUE if cran_data param != default of NULL.
  on_cran <- !is.null(cran_data)
  
  # If not on cran.
  if (!on_cran) {
    # Input validation: check (GitHub) description_contents param is not default of NULL; stop if not.
    if (is.null(description_contents)) {
      stop("If CRAN metadata is NULL, must provide DESCRIPTION contents")
    }
    
    # Parse description_contents to DESCRIPTION file for easy field value extraction using desc.
    parsed <- desc::desc(text = description_contents)
    
    # Extract required metadata from GitHub DESCRIPTION file using get_field_safely fnc or appropriate desc method directly.
    title <- get_field_safely(parsed, "Title")
    description <- get_field_safely(parsed, "Description")
    license <- get_field_safely(parsed, "License")
    authors <- format(parsed$get_author(), include = c("given", "family"))
    maintainer <- parsed$get_maintainer()
    date_published <- NA
    
  } else {
    
    # Input validation: check cran_data param is of appropriate class; stop if not.
    if (!("cran_metadata" %in% class(cran_data))) {
      stop("cran_data must be of type 'cran_metadata'")
    }
    
    # Extract required metadata field values from cran_db.
    title <- cran_data$title
    description <- cran_data$description
    license <- cran_data$license
    authors <- paste(unlist(cran_data$author), collapse = ", ")
    maintainer <- cran_data$maintainer
    date_published <- cran_data$date_published
  }
  
  # Compile list of target metadata - modify/ estimate where necessary.
  ret <- list(name = gh_data$name,
              title = title,
              description = stringr::str_remove_all(description, "[\r\n]"),
              license = license,
              authors = paste(authors, collapse = ","),
              maintainer = maintainer,
              date_published = date_published,
              has_tests = gh_data$has_tests,
              has_vignettes = gh_data$has_vignettes,
              num_contributors = gh_data$num_contributors,
              on_cran = on_cran
  )
  
  # Append class "htahub_metadata".
  class(ret) <- append("htahub_metadata", class(ret))
  ret
  
}

#' @description
#' Compiles target metadata from CRAN database for a single package.
#' 
#' @param cran_db A data.frame of relevant metadata from the CRAN package database
#'                for all packages also available on CRAN.
#' @param name A character string of length 1 for the package name.
#' 
#' @return A list of target metadata. Includes:
#'          - The package name;
#'          - The title field;
#'          - The description field;
#'          - The license field;
#'          - The authors;
#'          - The maintainer;
#'          - The data published.
cran_metadata <- function(cran_db, name) {
  
  # Select cran_db rows for package name.
  rows <- cran_db[cran_db$package == name,]
  
  # Check at nrows selected is not 0. Return NULL if 0.
  if (nrow(rows) == 0) {
    return(NULL)
  }
  
  # Select first row in case of multiple.
  row <- rows[1, ]
  
  # Compile list of target metadata.
  ret <- list(name = row$package,
              title = row$title,
              description = row$description,
              license = row$license,
              authors = paste(unlist(row$author), collapse = ", "),
              maintainer = row$maintainer,
              date_published = row[["date/publication"]]
  )
  
  # Add class "cran_metadata".
  class(ret) <- append("cran_metadata", class(ret))
  ret
  
}

#' @description
#' Compiles target metadata excl. DESCRIPTION file data points for a single GitHub repo.
#' DESCRIPTION file data points are compiled separately in the 
#' [compiled_metadata()].
#' 
#' @param name A character vector of length 1 for the package name.
#' @param owner A character vector of length 1 for the package owner.
#' @param repo A character vector of length for the repo name.
#' 
#' @return A list of target metadata. Includes:
#'          - Whether a tests folder is present; 
#'          - Whether a vignettes folder is present;
#'          - The package name;
#'          - The repo URL;
#'          - The number of contributors.
github_metadata <- function(name, owner, repo) {
  
  # Get repo files metadata via "contents" API endpoint.
  files_meta <- api_get_endpoint(owner = owner, repo = repo, endpoint = "contents")
  
  # Check files_meta is not error return value of NULL. Return NULL if so.
  if (is.null(files_meta)) {
    logger::log_info(sprintf("Excluding package %s because not found on Github", name))
    return(NULL)
  }
  
  # Extract name field values from files_meta.
  filenames <- sapply(files_meta, function(x) x$name)
  
  # Check for DESCRIPTION file name using utils has_description function. Return NULL (exclude) if missing.
  if (!has_description(filenames)) {
    logger::log_info(sprintf("Excluding package %s because it does not have a DECSRIPTION file", name))
    return(NULL)
  }
  
  # Get additional repo metadata via contributors" and "commits" endpoints.
  extra_meta <- api_get_endpoints(owner = owner, repo = repo, endpoint = c("contributors", "commits"))
  logger::log_info(sprintf("Github metadata retrieved for package %s", name))
  
  # Compile list of target metadata.
  ret <- list(has_tests = has_tests(filenames),
              has_vignettes = has_vignettes(filenames),
              name = name,
              url = paste("https://github.com", owner, repo, sep = "/"),
              num_contributors = length(extra_meta$contributors))
  
  # Add class "github_metadata".
  class(ret) <- append("github_metadata", class(ret))
  ret
  
}