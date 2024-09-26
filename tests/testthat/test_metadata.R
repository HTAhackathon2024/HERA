packages <- get_packages(test_path("testdata", "packages.csv"))
cran_db <- get_cran_db(packages)

test_that("Can read metadata from cran", {
  meta <- cran_metadata(cran_db, "maic")
  expect_equal(meta$name, "maic")
  expect_equal(meta$title, "Matching-Adjusted Indirect Comparison")
  expect_equal(meta$license, "GPL-3")
  expect_equal(meta$authors, "Rob Young")
  expect_equal(meta$maintainer, "Rob Young")
  expect_equal(meta$date_published, "2022-04-27 14:50:07 UTC")
})

test_that("Can compile metadata", {
  cran_meta <- cran_metadata(cran_db, "maic")
  gh_meta <- github_metadata("maic", "Roche", "MAIC")
  compiled <- compile_metadata(gh_meta, cran_meta)
  expect_equal(names(compiled), c("name", 
                                  "title", 
                                  "description", 
                                  "license", 
                                  "authors",
                                  "maintainer", 
                                  "date_published",
                                  "has_tests", 
                                  "has_vignettes", 
                                  "num_contributors",
                                  "num_stars",
                                  "on_cran")
               )
})
