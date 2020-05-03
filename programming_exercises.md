Programming Exercises
================
Maria Benavides

## Load necessary libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Compute the number of unique values in each column of `iris`

### Using a `for` loop

``` r
# Familiarize with the iris dataset

names(iris)
```

    ## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"

``` r
# Create a vector to store the values 

uniq_val_iris_loop <- 
  vector("numeric", ncol(iris))

# Compute unique values with for loop

for (i in seq_along(iris)) {
  uniq_val_iris_loop[[i]] <- length(unique(iris[[i]])) # unique creates a list of the unique values
}

# Print results 

print(uniq_val_iris_loop)
```

    ## [1] 35 23 43 22  3

### Using a `map` function

``` r
# Compute unique values with map

uniq_val_iris_map <- iris %>%
  map(unique) %>%
  map_dbl(length)

# Print results 

print(uniq_val_iris_map)
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    ##           35           23           43           22            3

## Calculate the square of each element in vector `x`

``` r
x <- seq(from = 30, to = 1)
x
```

    ##  [1] 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6
    ## [26]  5  4  3  2  1

``` r
# Create square function

square <- function(x){
  return(x^2)
}

# Use function to square values in x 

square_x <- square(x)

# Print results 

print(square_x)
```

    ##  [1] 900 841 784 729 676 625 576 529 484 441 400 361 324 289 256 225 196 169 144
    ## [20] 121 100  81  64  49  36  25  16   9   4   1

### Using a `for` loop

``` r
# Create a vector to store the values (with a different approach than the previous exercise)

square_x_loop <- c()  # This creates an empty vector 

# Calculate squares for each value on x with a for loop

for (i in seq_along(x)) {
  square_x_loop[[i]] <- (x[[i]])^2
}

# Print results 

print(square_x_loop)
```

    ##  [1] 900 841 784 729 676 625 576 529 484 441 400 361 324 289 256 225 196 169 144
    ## [20] 121 100  81  64  49  36  25  16   9   4   1

### Using a `map` function

``` r
# Compute unique values with map (and the function I created before)

square_x_map <- map_dbl(x, square)

# Print results 

print(square_x_map)
```

    ##  [1] 900 841 784 729 676 625 576 529 484 441 400 361 324 289 256 225 196 169 144
    ## [20] 121 100  81  64  49  36  25  16   9   4   1

## Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem

``` r
# Set conditions and errors for when user does not include two values 

pythagorean_th <- function(a = NULL, b = NULL, c = NULL) {
  if (is.null(a) & is.null(b) & is.null(c)) {
    stop("Most include two numeric values")
  } else if (is.null(a) & is.null(b)) {
    stop("Most include two numeric values")
  } else if (is.null(b) & is.null(c)) {
    stop("Most include two numeric values")
  } else if(is.null(a) & is.null(c)) {
    stop("Most include two numeric values")
  } else if (!is.null(a) & !is.null(b) & !is.null(c)) {
    stop("Most include two numeric values")
  } 

# Calculate lenght of third side, when user includes the length of two sides 
  
  if (is.null(c)) {
    if (is.numeric(a) & is.numeric(b)) {  # Condition for when the values are not numeric 
      return(sqrt(a^2 + b^2)) 
      } else {
        stop("a or b are non-numeric values")
      } 
    
  } else if (is.null(a)) {
    if (is.numeric(b) & is.numeric(c)) {
      return(sqrt(c^2-b^2)) 
      } else {
        stop("b or b are non-numeric values")
      }
    
  } else {
    if (is.numeric(a) & is.numeric(c)) {
      return(sqrt(c^2-a^2)) 
      } else {
        stop("a or c are non-numeric values")
      }
    }
}

# Try function with different values 

print(pythagorean_th (a = 3, b = 4))
```

    ## [1] 5

``` r
print(pythagorean_th (b = 4, c = 5))
```

    ## [1] 3

``` r
print(pythagorean_th (c = 5, a = 3))
```

    ## [1] 4

## Session info

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                               
    ##  version  R version 3.6.3 (2020-02-29)        
    ##  os       Red Hat Enterprise Linux 8.1 (Ootpa)
    ##  system   x86_64, linux-gnu                   
    ##  ui       X11                                 
    ##  language (EN)                                
    ##  collate  en_US.UTF-8                         
    ##  ctype    en_US.UTF-8                         
    ##  tz       America/Chicago                     
    ##  date     2020-05-03                          
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [2] CRAN (R 3.6.3)
    ##  backports     1.1.5   2019-10-02 [2] CRAN (R 3.6.3)
    ##  broom         0.5.5   2020-02-29 [2] CRAN (R 3.6.3)
    ##  callr         3.4.3   2020-03-28 [2] CRAN (R 3.6.3)
    ##  cellranger    1.1.0   2016-07-27 [2] CRAN (R 3.6.3)
    ##  cli           2.0.2   2020-02-28 [2] CRAN (R 3.6.3)
    ##  colorspace    1.4-1   2019-03-18 [2] CRAN (R 3.6.3)
    ##  crayon        1.3.4   2017-09-16 [2] CRAN (R 3.6.3)
    ##  DBI           1.1.0   2019-12-15 [2] CRAN (R 3.6.3)
    ##  dbplyr        1.4.2   2019-06-17 [2] CRAN (R 3.6.3)
    ##  desc          1.2.0   2018-05-01 [2] CRAN (R 3.6.3)
    ##  devtools      2.3.0   2020-04-10 [1] CRAN (R 3.6.3)
    ##  digest        0.6.25  2020-02-23 [2] CRAN (R 3.6.3)
    ##  dplyr       * 0.8.5   2020-03-07 [2] CRAN (R 3.6.3)
    ##  ellipsis      0.3.0   2019-09-20 [2] CRAN (R 3.6.3)
    ##  evaluate      0.14    2019-05-28 [2] CRAN (R 3.6.3)
    ##  fansi         0.4.1   2020-01-08 [2] CRAN (R 3.6.3)
    ##  forcats     * 0.5.0   2020-03-01 [2] CRAN (R 3.6.3)
    ##  fs            1.4.0   2020-03-31 [2] CRAN (R 3.6.3)
    ##  generics      0.0.2   2018-11-29 [2] CRAN (R 3.6.3)
    ##  ggplot2     * 3.3.0   2020-03-05 [2] CRAN (R 3.6.3)
    ##  glue          1.4.0   2020-04-03 [1] CRAN (R 3.6.3)
    ##  gtable        0.3.0   2019-03-25 [2] CRAN (R 3.6.3)
    ##  haven         2.2.0   2019-11-08 [2] CRAN (R 3.6.3)
    ##  hms           0.5.3   2020-01-08 [2] CRAN (R 3.6.3)
    ##  htmltools     0.4.0   2019-10-04 [2] CRAN (R 3.6.3)
    ##  httr          1.4.1   2019-08-05 [2] CRAN (R 3.6.3)
    ##  jsonlite      1.6.1   2020-02-02 [2] CRAN (R 3.6.3)
    ##  knitr         1.28    2020-02-06 [2] CRAN (R 3.6.3)
    ##  lattice       0.20-38 2018-11-04 [2] CRAN (R 3.6.3)
    ##  lifecycle     0.2.0   2020-03-06 [2] CRAN (R 3.6.3)
    ##  lubridate     1.7.8   2020-04-06 [1] CRAN (R 3.6.3)
    ##  magrittr      1.5     2014-11-22 [2] CRAN (R 3.6.3)
    ##  memoise       1.1.0   2017-04-21 [2] CRAN (R 3.6.3)
    ##  modelr        0.1.6   2020-02-22 [2] CRAN (R 3.6.3)
    ##  munsell       0.5.0   2018-06-12 [2] CRAN (R 3.6.3)
    ##  nlme          3.1-144 2020-02-06 [2] CRAN (R 3.6.3)
    ##  pillar        1.4.3   2019-12-20 [2] CRAN (R 3.6.3)
    ##  pkgbuild      1.0.6   2019-10-09 [2] CRAN (R 3.6.3)
    ##  pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 3.6.3)
    ##  pkgload       1.0.2   2018-10-29 [2] CRAN (R 3.6.3)
    ##  prettyunits   1.1.1   2020-01-24 [2] CRAN (R 3.6.3)
    ##  processx      3.4.2   2020-02-09 [2] CRAN (R 3.6.3)
    ##  ps            1.3.2   2020-02-13 [2] CRAN (R 3.6.3)
    ##  purrr       * 0.3.3   2019-10-18 [2] CRAN (R 3.6.3)
    ##  R6            2.4.1   2019-11-12 [2] CRAN (R 3.6.3)
    ##  Rcpp          1.0.4   2020-03-17 [2] CRAN (R 3.6.3)
    ##  readr       * 1.3.1   2018-12-21 [2] CRAN (R 3.6.3)
    ##  readxl        1.3.1   2019-03-13 [2] CRAN (R 3.6.3)
    ##  remotes       2.1.1   2020-02-15 [2] CRAN (R 3.6.3)
    ##  reprex        0.3.0   2019-05-16 [2] CRAN (R 3.6.3)
    ##  rlang         0.4.5   2020-03-01 [2] CRAN (R 3.6.3)
    ##  rmarkdown     2.1     2020-01-20 [2] CRAN (R 3.6.3)
    ##  rprojroot     1.3-2   2018-01-03 [2] CRAN (R 3.6.3)
    ##  rstudioapi    0.11    2020-02-07 [2] CRAN (R 3.6.3)
    ##  rvest         0.3.5   2019-11-08 [2] CRAN (R 3.6.3)
    ##  scales        1.1.0   2019-11-18 [2] CRAN (R 3.6.3)
    ##  sessioninfo   1.1.1   2018-11-05 [2] CRAN (R 3.6.3)
    ##  stringi       1.4.6   2020-02-17 [2] CRAN (R 3.6.3)
    ##  stringr     * 1.4.0   2019-02-10 [2] CRAN (R 3.6.3)
    ##  testthat      2.3.2   2020-03-02 [2] CRAN (R 3.6.3)
    ##  tibble      * 3.0.0   2020-03-30 [2] CRAN (R 3.6.3)
    ##  tidyr       * 1.0.2   2020-01-24 [2] CRAN (R 3.6.3)
    ##  tidyselect    1.0.0   2020-01-27 [2] CRAN (R 3.6.3)
    ##  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 3.6.3)
    ##  usethis       1.6.0   2020-04-09 [1] CRAN (R 3.6.3)
    ##  vctrs         0.2.4   2020-03-10 [2] CRAN (R 3.6.3)
    ##  withr         2.1.2   2018-03-15 [2] CRAN (R 3.6.3)
    ##  xfun          0.12    2020-01-13 [2] CRAN (R 3.6.3)
    ##  xml2          1.3.0   2020-04-01 [2] CRAN (R 3.6.3)
    ##  yaml          2.2.1   2020-02-01 [2] CRAN (R 3.6.3)
    ## 
    ## [1] /home/mabenavides/R/x86_64-redhat-linux-gnu-library/3.6
    ## [2] /usr/lib64/R/library
    ## [3] /usr/share/R/library
