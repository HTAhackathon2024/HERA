# Generic gh GitHub API GET query
api_get_endpoint <- function(owner, repo, endpoint) {

  # Create API endpoint string
  api_query <- paste0("GET /repos/", owner, "/", repo, "/", endpoint)
  logger::log_debug(api_query)

  # Query using gh
  response <- tryCatch({
    gh::gh(api_query, owner = owner, repo = repo)
  }, error = function(e) {
    logger::log_error("Error fetching data from GitHub API for endpoint '", endpoint, "': ", e$message)
    return(NULL)
  })

  return(response)
}

# Wrapper for multiple endpoints by repo (owner) using map
api_get_endpoints <- function(owner, repo, endpoints) {
  responses <- purrr::map(endpoints, function(endpoint) api_get_endpoint(owner, repo, endpoint))
  names(responses) <- endpoints
  return(responses)
}
