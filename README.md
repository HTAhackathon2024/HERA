# HTA-hub
Website for a repository/inventory of health economics R packages 

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
