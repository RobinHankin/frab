The frab package: how to add R tables
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/frab.png" width = "150" align="right" />

# Overview

To cite the `frab` package in publications please use Hankin (2023). The
`frab` package allows one to “add” R tables in a natural way. It also
furnishes an alternative interpretation of named vectors wherein
addition is defined using the (unique) names as the primary key. Support
for multi-dimensional R tables is included. The underlying mathematical
object is the Free Abelian group.

The package has two S4 classes: `frab` and `sparsetable`. Class `frab`
is for one-dimensional R tables and is an alternative implementation of
named vectors; class `sparsetable` handles multi-way R tables in a
natural way.

# The package in use

## One-dimensional R tables: class `frab`

Primary construction function `frab()` takes a named vector and returns
a `frab` object:

``` r
suppressMessages(library("frab"))
p <- c(x=1,b=2,a=2,b=3,c=7,x=-1)
frab(p)
#> A frab object with entries
#> a b c 
#> 2 5 7
```

Above, we see from the return value that function `frab()` has reordered
the labels of its argument, calculated the value for entry `b` \[as
![2+3=5](https://latex.codecogs.com/png.latex?2%2B3%3D5 "2+3=5")\],
determined that the entry for `x` has vanished \[the values cancelling
out\], and printed the result using a bespoke show method. It is useful
to think of the input argument as a semi-constructed and generalized
“table” of observations. Thus

``` r
p
#>  x  b  a  b  c  x 
#>  1  2  2  3  7 -1
```

Above we see `p` might correspond to a story: “look, we have one `x`,
two `b`s, two `a`s, another three `b`s, seven `c`s…oh hang on that `x`
was a mistake I had better subtract one now”. However, the package’s
most useful feature is the overloaded definition of addition:

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

Above we see function `rfrab()` used to generate a random `frab` object,
corresponding to an R table. It is *possible* to add `x` and `y`
directly:

``` r
xn <- as.namedvector(x)
yn <- as.namedvector(y)
table(c(rep(names(xn),times=xn),rep(names(yn),times=yn)))
#> 
#>  a  b  c  d  e  f  g  i 
#>  7 10  2  6  8  5  7  7
```

but this is extremely inefficient and cannot deal with fractional (or
indeed negative) entries.

# Multi-way R tables

Class `sparsetable` deals with multi-way R tables. Taking three-way R
tables as an example:

``` r
(x3 <- rspar())
#>  Jan Feb Mar     val
#>    a   a   a  =   10
#>    a   c   b  =   15
#>    b   a   a  =   11
#>    b   a   b  =    9
#>    b   a   c  =   12
#>    b   b   a  =    6
#>    b   b   b  =    3
#>    b   b   c  =   14
#>    b   c   a  =    9
#>    b   c   c  =   21
#>    c   c   a  =   10
```

Function `rspar()` returns a random `sparsetable` object. We see that,
of the
![3^3=27](https://latex.codecogs.com/png.latex?3%5E3%3D27 "3^3=27")
possible entries, only 11 are non-zero. We may coerce to a regular R
table:

``` r
as.array(x3)
#> , , Mar = a
#> 
#>    Feb
#> Jan  a b  c
#>   a 10 0  0
#>   b 11 6  9
#>   c  0 0 10
#> 
#> , , Mar = b
#> 
#>    Feb
#> Jan a b  c
#>   a 0 0 15
#>   b 9 3  0
#>   c 0 0  0
#> 
#> , , Mar = c
#> 
#>    Feb
#> Jan  a  b  c
#>   a  0  0  0
#>   b 12 14 21
#>   c  0  0  0
```

In this case it is hardly worth taking advantage of the sparse
representation (which is largely inherited from the `spray` package) but
a larger example might be

``` r
rspar(n=4,l=10,d=12)
#>  Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec     val
#>    b   c   j   e   f   j   f   a   g   i   a   d  =    1
#>    g   a   j   e   c   f   e   c   a   f   g   c  =    4
#>    j   b   j   g   h   c   d   c   c   b   b   i  =    2
#>    j   j   h   h   a   a   i   f   c   h   g   h  =    3
```

The random `sparsetable` object shown above would require
![10^{12}](https://latex.codecogs.com/png.latex?10%5E%7B12%7D "10^{12}")
floating point numbers in full array form, of which only 4 are nonzero.
Multi-way R tables may be added in the same way as `frab` objects:

``` r
y3 <- rspar()
x3+y3
#>  Jan Feb Mar     val
#>    a   a   a  =   10
#>    a   a   b  =   14
#>    a   b   a  =    4
#>    a   c   a  =   14
#>    a   c   b  =   15
#>    b   a   a  =   11
#>    b   a   b  =   23
#>    b   a   c  =   12
#>    b   b   a  =   17
#>    b   b   b  =   13
#>    b   b   c  =   23
#>    b   c   a  =    9
#>    b   c   b  =    7
#>    b   c   c  =   24
#>    c   a   a  =   15
#>    c   c   a  =   15
#>    c   c   c  =   14
```

## Two-way R tables

Two-way R tables are something of a special case, having their own print
method. By default, two-dimensional `sparsetable` objects are coerced to
a matrix before printing, but otherwise operate in the same way as the
multi-dimensional case discussed above:

``` r
(x2 <- rspar2())
#>    bar
#> foo A  B  D  E  F
#>   a 3 20  0  0  9
#>   b 0  0 15  0  0
#>   c 0  0  0  4  0
#>   d 0  0  0  5 22
#>   e 0  2  0 11 29
(y2 <- rspar2())
#>    bar
#> foo A C  D  E  F
#>   a 9 0 25  6 10
#>   b 7 0  0  0  1
#>   c 0 0  0 11  0
#>   d 8 5  0  4  0
#>   e 0 3  2  0  0
#>   f 0 0 14  0 15
x2+y2
#>    bar
#> foo  A  B C  D  E  F
#>   a 12 20 0 25  6 19
#>   b  7  0 0 15  0  1
#>   c  0  0 0  0 15  0
#>   d  8  0 5  0  9 22
#>   e  0  2 3  2 11 29
#>   f  0  0 0 14  0 15
```

Above, note how the sizes of the coerced matrices are different
(![5\times 5](https://latex.codecogs.com/png.latex?5%5Ctimes%205 "5\times 5")
for `x2`,
![6\times 5](https://latex.codecogs.com/png.latex?6%5Ctimes%205 "6\times 5")
for `y2`) but the addition method copes, using a bespoke sparse matrix
representation. Also note that the sum has *six* columns (corresponding
to six distinct column headings) even though `x2` and `y2` have only
five.

# Further information

For more detail, see the package vignette

`vignette("frab")`

## References

- R. K. S. Hankin 2023. “The free Abelian group in `R`: the `frab`
  package”, arXiv, <https://arxiv.org/abs/2307.13184>.
- R. K. S. Hankin 2022. “Disordered vectors in `R`: introducing the
  `disordR` package”, arXiv, <https://arxiv.org/abs/2210.03856>
