#' @description
#' GitHub REST API GET query function using gh package.
#' 
#' @param owner         A character vector of length 1 for the owner of the repo.         
#' @param repo          A character vector of length 1 for the name of the repo.
#' @param endpoint      A character vector of length 1 for the endpoint to be queried. 
#' 
#' @return              A list object containing the API query response data.
api_get_endpoint <- function(owner, repo, endpoint) {

  # Create API query to string
  api_query <- paste0("GET /repos/", owner, "/", repo, "/", endpoint)
  logger::log_debug(api_query)

  # Make GitHub API request for endpoint. tryCatch used as error handler wrapper.
  response <- tryCatch({
    # API query
    gh::gh(api_query, owner = owner, repo = repo)
  }, error = function(e) {
    # Logger with issue-specific error message  
    logger::log_error("Error fetching data from GitHub API for endpoint '", endpoint, "': ", e$message)
    return(NULL)
  })

  return(response)
}

#' @description
#' Wrapper function for querying multiple API endpoints for a single repo.
#'
#' @param owner         A character vector of length 1 for the owner of the repo.
#' @param repo          A character vector of length 1 for the name of the repo.
#' @param endpoint      A a list of character strings for endpoints to be queried.
#'
#' @return              A list object containing the API query response data for each endpoint.
api_get_endpoints <- function(owner, repo, endpoints) {

  # Query each endpoint in endpoints via call to api_get_endpoint.
  responses <- lapply(endpoints, function(endpoint) api_get_endpoint(owner, repo, endpoint))
  
  # Assign endpoint names to corresponding response data.
  names(responses) <- endpoints

  return(responses)

}
