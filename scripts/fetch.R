# Source fnc scripts
source("R/utils.R")
source("R/metadata.R")
source("R/gh_api.R")
source("R/fetch_metadata.R")
source("R/load_packages.R")

# Get placeholder (fixed) list of packages. 
packages <- get_packages()
# Get CRAN data for packages also published on CRAN.
cran_db <- get_cran_db(packages)
# Compile target metadata for each package in packages.
package_metadata <- fetch_metadata(packages, cran_db)
# Remove any NULL elements.
package_metadata <- Filter(Negate(is.null), package_metadata)
# Get unique list of metadata variables from package_metadata. Used as target.
all_names <- unique(unlist(lapply(package_metadata, names)))
# Create standardized (i.e., filled) list of target metadata - List nested by package.
filled_list <- lapply(package_metadata, fill_na, all_names = all_names)
# Create combined data frame of package metadata.
df <- dplyr::bind_rows(lapply(filled_list, as.data.frame))
# Write combined data frame to file as csv.
write.csv(df, "data/data.csv", row.names = FALSE)