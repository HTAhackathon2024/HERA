fetch_metadata_for_package <- function(name, path_fragment, cran_db) {
  owner_repo <- stringr::str_split(path_fragment, "/")
  logger::log_info(path_fragment)
  owner <- owner_repo[[1]][1]
  repo <- owner_repo[[1]][2]
  logger::log_info(sprintf("Fetching metadata for %s", name))
  on_cran <- on_cran(name, cran_db)
  gh_data <- github_metadata(name, owner, repo)
  if (is.null(gh_data)) {
    return(NULL)
  }
  if (on_cran) {
    logger::log_info(sprintf("Package %s is on CRAN; using CRAN metadata", name))
    cran_data <- cran_metadata(cran_db[cran_db$package == name,])
    compile_metadata(gh_data, cran_data = cran_data)
  } else {
    logger::log_info(sprintf("Package %s is not on CRAN; retrieving DESCRIPTION file from Github", name))
    res <- api_get_endpoint(owner = owner, repo = repo, endpoint = "contents/DESCRIPTION")

    if (is.null(res)) {
      logger::log_info(sprintf("Excluding package %s of error retrieving DESCRIPTION from Github", name))
      return(NULL)
    }
    description_contents <- rawToChar(base64enc::base64decode(res$content))
    logger::log_info(sprintf("Compiling metadata for %s", name))
    compile_metadata(gh_data, description_contents = description_contents)
  }
}

fetch_metadata <- function(packages, cran_db) {
  lapply(seq_len(nrow(packages)), function(i) {
    name <- packages[i, "name"]
    path_fragment <- packages[i, "url"]
    fetch_metadata_for_package(name, path_fragment, cran_db)
  })
}
