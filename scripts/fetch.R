#!/usr/bin/Rscript
source("R/utils.R")
source("R/metadata.R")
source("R/gh_api.R")
source("R/fetch_metadata.R")

get_packages <- function() {
  packages <- read.csv("config/packages.csv",
                       header = FALSE,
                       col.names = c("name", "url"))
  packages <- packages[is_valid_name(packages$name),]
}

get_cran_db <- function(packages) {
  logger::log_info("Getting CRAN database")
  cran_db <- tools::CRAN_package_db()
  cran_db <- cran_db[cran_db$Package %in% packages$name,]
  logger::log_info("Filtering CRAN database")
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
df <- dplyr::bind_rows(lapply(filled_list, as.data.frame))
write.csv(df, "data/data.csv", row.names = FALSE)
