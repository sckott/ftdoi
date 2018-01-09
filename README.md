rftdoi
======



[![Build Status](https://travis-ci.org/ropenscilabs/rftdoi.svg?branch=master)](https://travis-ci.org/ropenscilabs/rftdoi)
[![codecov.io](https://codecov.io/github/ropenscilabs/rftdoi/coverage.svg?branch=master)](https://codecov.io/github/ropenscilabs/rftdoi?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/rftdoi?color=2ECC71)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/rftdoi)](https://cran.r-project.org/package=rftdoi)


`rftdoi` is a client for interacting with <https://ftdoi.org>

## Installation

Development version


```r
install.packages("devtools")
devtools::install_github("ropenscilabs/rftdoi")
```


```r
library('rftdoi')
```

## Heartbeat - list routes


```r
ftd_heartbeat()
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

* Please [report any issues or bugs](https://github.com/ropenscilabs/rftdoi/issues).
* License: MIT
* Get citation information for `rftdoi` in R doing `citation(package = 'rftdoi')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
