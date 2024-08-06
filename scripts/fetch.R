#!/usr/bin/Rscript
source("R/utils.R")
source("R/metadata.R")
source("R/gh_api.R")
source("R/fetch_metadata.R")
source("R/load_packages.R")

packages <- get_packages()
cran_db <- get_cran_db(packages)

package_metadata <- fetch_metadata(packages, cran_db)
package_metadata <- Filter(Negate(is.null), package_metadata)
all_names <- unique(unlist(lapply(package_metadata, names)))
filled_list <- lapply(package_metadata, fill_na, all_names = all_names)
df <- dplyr::bind_rows(lapply(filled_list, as.data.frame))
write.csv(df, "data/data.csv", row.names = FALSE)
