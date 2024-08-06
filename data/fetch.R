#!/usr/bin/Rscript
source("R/logic.R")
source("R/metadata.R")

fetch_metadata_for_package <- function(name, path_fragment, cran_db) {
  logger::log_info(sprintf("Fetching metadata for %s", name))
  on_cran <- on_cran(name, cran_db)
  gh_data <- github_metadata(name, path_fragment)
  if (is.null(gh_data)) {
    return(NULL)
  }
  if (on_cran) {
    logger::log_info(sprintf("Package %s is on CRAN; using CRAN metadata", name))
    cran_data <- cran_metadata(cran_db[cran_db$package == name,])
    compile_metadata(gh_data, cran_data = cran_data)
  } else {
    logger::log_info(sprintf("Package %s is not on CRAN; retrieving DESCRIPTION file from Github", name))
    res <- NULL
    tryCatch({
      res <- gh::gh("GET /repos/{path_fragment}/contents/DESCRIPTION", path_fragment = path_fragment)
    }, error = function(e) {
      logger::log_error(e$message)
    })
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

get_packages <- function() {
  packages <- read.csv("data/packages.csv",
                       header = FALSE,
                       col.names = c("name", "url"))
  packages <- packages[is_valid_name(packages$name),]
}

get_cran_db <- function(packages) {
  cran_db <- tools::CRAN_package_db()
  cran_db <- cran_db[cran_db$Package %in% packages$name,]
  cran_db_clean <- cranly::clean_CRAN_db(cran_db)[, c("author",
                                                      "package",
                                                      "title",
                                                      "description",
                                                      "license",
                                                      "date/publication",
                                                      "maintainer",
                                                      "reverse_depends")]
}

packages <- get_packages()
cran_db <- get_cran_db(packages)

package_metadata <- fetch_metadata(packages, cran_db)
package_metadata <- Filter(Negate(is.null), package_metadata)
all_names <- unique(unlist(lapply(package_metadata, names)))
filled_list <- lapply(package_metadata, fill_na, all_names = all_names)
df <- bind_rows(lapply(filled_list, as.data.frame))
