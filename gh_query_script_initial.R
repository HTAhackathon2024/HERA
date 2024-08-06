library(gh)
library(purrr)

df_repos <- data.frame(
  owner = c("octocat", "hesim-dev", "tidyverse"),
  repo = c("Hello-World", "hesim", "dplyr")
)

df_api_endpoints <- c("contributors", "commits")

# Generic gh GitHub API GET query
api_get_endpoint <- function(owner, repo, endpoint) {

  # Create API endpoint string
  api_query <- paste0("GET /repos/", owner, "/", repo, "/", endpoint)

  # Query using gh
  response <- tryCatch({
    gh(api_query, owner = owner, repo = repo)
  }, error = function(e) {
    message("Error fetching data from GitHub API for endpoint '", endpoint, "': ", e$message)
    return(NULL)
  })

  return(response)
}

# Wrapper for all endpoints by repo (owner) using map
api_get_endpoints <- function(owner, repo, endpoints) {

  responses <- map(endpoints, function(endpoint) api_get_endpoint(owner, repo, endpoint))
  names(responses) <- endpoints
  return(responses)

}

responses_out <- api_get_endpoints(df_repos$owner[3], df_repos$repo[3], df_api_endpoints)




