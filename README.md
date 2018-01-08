rpubpatterns
============



[![Build Status](https://travis-ci.org/ropenscilabs/rpubpatterns.svg?branch=master)](https://travis-ci.org/ropenscilabs/rpubpatterns)
[![codecov.io](https://codecov.io/github/ropenscilabs/rpubpatterns/coverage.svg?branch=master)](https://codecov.io/github/ropenscilabs/rpubpatterns?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/rpubpatterns?color=2ECC71)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/rpubpatterns)](https://cran.r-project.org/package=rpubpatterns)


`rpubpatterns` is a client for interacting with <https://ftdoi.org>

## Installation

Development version


```r
install.packages("devtools")
devtools::install_github("ropenscilabs/rpubpatterns")
```


```r
library('rpubpatterns')
```

## Heartbeat - list routes


```r
pb_heartbeat()
#> $routes
#>  [1] "/ (api docs)"             "/api (-> /api/heartbeat)"
#>  [3] "/api/heartbeat"           "/:doi (not working yet)" 
#>  [5] "/api/members"             "/api/members/:member"    
#>  [7] "/api/prefixes"            "/api/prefixes/:prefix"   
#>  [9] "/api/doi/*"               "/api/fetch/*"
```

## Contributors

* Scott Chamberlain [@sckott](https://github.com/sckott)

## Meta

* Please [report any issues or bugs](https://github.com/ropenscilabs/rpubpatterns/issues).
* License: MIT
* Get citation information for `rpubpatterns` in R doing `citation(package = 'rpubpatterns')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
