get_packages <- function() {
  packages <- read.csv("config/packages.csv",
                       header = FALSE,
                       col.names = c("name", "url"))
  packages <- packages[is_valid_name(packages$name),]
}

get_cran_db <- function(packages, use_cache = TRUE) {
  cran_db <- NULL
  if (use_cache & file.exists("cache/cran_db.rds")) {
    cached <- readRDS("cache/cran_db.rds")
    cran_db <- cached$cran_db
    if (!identical(cached$package_names, packages$name)) {
      cran_db <- NULL
    }
  }
  if (is.null(cran_db)) {
    logger::log_info("Package list has changed, or cache not available. Fetching fresh CRAN db.")
    cran_db <- tools::CRAN_package_db()
    cran_db <- cran_db[cran_db$Package %in% packages$name,]
    logger::log_info("Filtering CRAN database")
    cran_db <- cranly::clean_CRAN_db(cran_db)[, c("author",
                                                  "package",
                                                  "title",
                                                  "description",
                                                  "license",
                                                  "date/publication",
                                                  "maintainer",
                                                  "reverse_depends")]
    logger::log_info("Updating cache")
    saveRDS(list(package_names = packages$name, cran_db = cran_db), "cache/cran_db.rds")
  }
  cran_db
}
