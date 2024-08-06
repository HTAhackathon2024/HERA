# HERA - Health Economics R Packages for All
Website for a repository/inventory of open source health economics R packages available on Github, with a number of metrics indicative of reliability.

## Compiling Quarto website
To compile the website locally for testing, run 
```
quarto render
```

This will generate static files in the `docs` folder. Open `docs/index.html` to view the website.

## Deploying Quarto website
The site is deployed automatically on merges to main via Github actions. The config for this
action can be found at [.github/workflows/quarto-publish.yml]()

## Compiling package metadata
To recompile metadata for the set of packages defined in `config/packages.csv`, run

```
./script/fetch.R
```

This script uses the Github API to fetch metadata for each package, as well as 
`tools::CRAN_package_db()` to fetch metadata from CRAN where it exists. 
It saves the results in `data/data.csv`.

Note that the Github API is heavily rate-limited if you don't provide a Github PAT. A PAT
can be provided by setting the environment variable `GITHUB_PAT`.

When you first run the script, the CRAN database is cached in `cache/cran_db.rds`, 
so subsequent script executions will be quicker. The cache will automatically update if
the package list changes, but you can also force a cache refresh by deleting the `rds` file.

## Testing
Tests can be executed by running `devtools::test()`.