The frab package: an alternative interpretation of named vectors
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/frab.png" width = "150" align="right" />

# Overview

The `frab` package furnishes an alternative interpretation of named
vectors wherein addition is defined using the (unique) names as the
primary key. This allows one to “add” tables in a consistent and
meaningful way. The underlying mathematical object is the Free Abelian
group.

# The package in use

The package has a single S4 class of objects, `frab`. Primary
construction function `frab()` takes a named vector and returns a `frab`
object:

``` r
suppressMessages(library("frab"))
frab(c(x=1,b=2,a=2,b=3,c=7,x=-1))
#> A frab object with entries
#> a b c 
#> 2 5 7
```

Above, we see from the return value that function `frab()` has reordered
the labels of its argument, calculated the value for entry `b` \[as
![2+3=5](https://latex.codecogs.com/png.latex?2%2B3%3D5 "2+3=5")\],
determined that the entry for `x` has vanished \[the values cancelling
out\], and printed the result using a bespoke show method. However, the
package’s most useful feature is the overloaded definition of addition:

``` r
(x <- rfrab())
#> A frab object with entries
#> a b c d g i 
#> 3 6 1 5 7 5
(y <- rfrab())
#> A frab object with entries
#> a b c d e f i 
#> 4 4 1 1 8 5 2
x+y
#> A frab object with entries
#>  a  b  c  d  e  f  g  i 
#>  7 10  2  6  8  5  7  7
```

# Further information

For more detail, see the package vignette

`vignette("frab")`
