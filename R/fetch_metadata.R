#' @description 
#' Processing wrapper function for retrieving and compiling target metadata of a package.
#' 
#' @param name          A character vector of length 1 for the package name.
#' @param path_fragment A character vector of length 1 containing the fragment of 
#'                      the URL for the repo owner and repo name elements.
#' @param cran_db       A data frame of relevant metadata from the CRAN package database
#'                      for packages also available on CRAN.
#' 
#' @return The result of compile_metadata - A list of target metadata for a given package.
fetch_metadata_for_package <- function(name, path_fragment, cran_db) {
  
  # Split path_fragment to extract repo owner & repo name elements.
  owner_repo <- strsplit(path_fragment, "/")
  logger::log_info(path_fragment)
  # Extract owner element from strsplit return list.
  owner <- owner_repo[[1]][1]
  # Extract repo element from strsplit return list.
  repo <- owner_repo[[1]][2]
  logger::log_info(sprintf("Fetching metadata for %s", name))
  # Call on_cran utils fnc. Checks for package name in cran_db$packages.
  on_cran <- on_cran(name, cran_db)
  
  # Retrieve GitHub metadata with call to github_metadata. If NULL, return NULL - github_metadata returns NULL 
  # if package is not found or the description file is missing.
  gh_data <- github_metadata(name, owner, repo)
  if (is.null(gh_data)) {
    return(NULL)
  }
  # If package is available on CRAN:
  if (on_cran) {
    logger::log_info(sprintf("Package %s is on CRAN; using CRAN metadata", name))
    # Retrieve and compile metadata with calls to cran_metadata and compile_metadata.
    cran_data <- cran_metadata(cran_db, name)
    compile_metadata(gh_data, cran_data = cran_data)
    
    # Package not on CRAN:
  } else {
    logger::log_info(sprintf("Package %s is not on CRAN; retrieving DESCRIPTION file from Github", name))
    
    # Retrieve DESCRIPTION file contents from GitHub with API GET call. Return NULL if error.     
    res <- api_get_endpoint(owner = owner, repo = repo, endpoint = "contents/DESCRIPTION")
    if (is.null(res)) {
      logger::log_info(sprintf("Excluding package %s of error retrieving DESCRIPTION from Github", name))
      return(NULL)
    }
    
    # Convert raw DESCRIPTION file contents to character string.
    description_contents <- rawToChar(base64enc::base64decode(res$content))
    logger::log_info(sprintf("Compiling metadata for %s", name))
    # Compile metadata with call to compile_metadata.
    compile_metadata(gh_data, description_contents = description_contents)
  }
}

#' @description 
#' Retrieve and compiles target metadata for each package in packages via calls to [fetch_metadata_for_package()].
#' 
#' @param packages  A data frame containing the name and associated and GitHub URL fragment for 
#'                  each package.
#' @param cran_db   A data frame of relevant metadata from the CRAN package database
#'                  for packages also available on CRAN.
#' 
#' @return          A list containing target metadata for each package.
fetch_metadata <- function(packages, cran_db) {
  
  lapply(seq_len(nrow(packages)), function(i) {
    # Extract package name
    name <- packages[i, "name"]
    # Extract package URL fragment
    path_fragment <- packages[i, "url"]
    # Fetch metadata for package with call to fetch_metadata_for_package
    fetch_metadata_for_package(name, path_fragment, cran_db)
  })
  
}