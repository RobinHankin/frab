\name{sparsetable}
\alias{sparsetable}
\alias{as.sparsetable}
\alias{is.sparsetable}
\alias{sparsetable-class}
\alias{index}
\alias{index,sparsetable-method}
\alias{values,sparsetable-method}
\alias{names,sparsetable-method}
\alias{dimnames,sparsetable-method}
\alias{dimnames<-,sparsetable-method}
\alias{dimnames<-,sparsetable,ANY-method}
\alias{is.empty,sparsetable-method}
\alias{arity}
\alias{arity,sparsetable-method}
\alias{dim}
\alias{dim,sparsetable-method}
\alias{asum}
\alias{asum.sparsetable}
\alias{asum,sparsetable-method}
\alias{asum_sparsetable}
\alias{asum_exclude_sparsetable}
\alias{as.array,sparsetable-method}
\alias{nterms}
\alias{nterms,sparsetable-method}
\alias{show,sparsetable-method}
\alias{print_sparsetable_matrixform}
\alias{sparsetable_to_table}
\alias{sparsetable_to_frab}
\alias{sparsetable_asum_include}
\alias{sparsetable_asum_exclude}
\alias{table_to_sparsetable}
\alias{sparsetable}
\alias{array_to_sparsetable}
\alias{sparsetable_to_array}
\alias{sparsetable_pmax}
\alias{sparsetable_pmin}
\alias{sparsetable_negative}
\alias{sparsetable_times_scalar}
\alias{sparsetable_eq_sparsetable}
\alias{sparsetable_equality}
\alias{sparsetable_maker}
\alias{rspar}
\alias{rspar2}
\alias{rsparr}
\alias{sparsetable_add}
\alias{sparsetable_negative}
\alias{sparsetable_reciprocal}
\alias{sparsetable_plus_sparsetable}
\alias{sparsetable_multiply_sparsetable}
\alias{sparsetable_multiply_numeric}
\alias{sparsetable_power_numeric}
\alias{numeric_compare_sparsetable}
\alias{numeric_multiply_sparsetable}
\alias{numeric_power_sparsetable}
\alias{sparsetable_accessor}
\alias{sparsetable_unary}
\alias{sparsetable_arith_sparsetable}
\alias{sparsetable_arith_numeric}
\alias{numeric_arith_sparsetable}
\alias{sparsetable_overwrite}
\alias{sparsetable_setter}
\alias{sparsetable_eq}
\alias{sparsetable_compare_sparsetable}
\alias{sparsetable_eq_num}
\alias{sparsetable_gt_num}
\alias{sparsetable_ge_num}
\alias{sparsetable_lt_num}
\alias{sparsetable_le_num}
\alias{sparsetable_compare_numeric}
\alias{num_eq_sparsetable}
\alias{num_gt_sparsetable}
\alias{num_ge_sparsetable}
\alias{num_lt_sparsetable}
\alias{num_le_sparsetable}
\alias{drop}
\alias{drop,sparsetable-method}
\alias{[,sparsetable-method}
\alias{[,sparsetable,disord,missing-method}
\alias{[,sparsetable,disindex,missing,ANY-method}
\alias{[,sparsetable,disord,missing,ANY-method}
\alias{[,sparsetable,ANY,ANY,ANY-method}
\alias{[<-,sparsetable-method}
\alias{[<-,sparsetable,ANY,ANY,ANY-method}
\alias{[<-,sparsetable,disindex,missing,ANY-method}
\alias{[<-,sparsetable,disord,missing,numeric-method}
\alias{pmax_sparsetable}
\alias{pmin_sparsetable}
\alias{pmax.sparsetable}
\alias{pmin.sparsetable}
\alias{pmax,sparsetable-method}
\alias{pmin,sparsetable-method}
\alias{pmax_pair_sparsetable}
\alias{pmin_pair_sparsetable}
\alias{pmax_dots_sparsetable}
\alias{pmin_dots_sparsetable}

\title{Generalized sparse tables: \code{sparsetable} objects}
\description{Package idiom for creating and manipulating
  \code{sparsetable} objects}
\usage{
sparsetable(i,v=1)
rspar(n=15,l=3,d=3)
rspar2(n=15,l=6)
rsparr(n=20,d=6,l=5,s=4)
sparsetable_to_array(x)
array_to_sparsetable(x)
sparsetable_to_frab(x)
\S4method{index}{sparsetable}(x)
\S4method{values}{sparsetable}(x)
\S4method{dimnames}{sparsetable}(x)
\S4method{dim}{sparsetable}(x)
}
\arguments{
  \item{x}{In functions like \code{index()}, an object of class \code{sparsetable}}
  \item{i,v}{In standard constructor function \code{sparsetable()},
    argument \code{i} is the index matrix of strings, and \code{v} a
    numeric vector of values}
  \item{n,l,d,s}{In functions \code{rspar()}, \code{rspar2()}, and
    \code{rsparr()}, \code{n} is the number of terms, \code{l} the
    number of letters, \code{d} the dimensionality and \code{s} the
    number of distinct marginal values to return}
}
\details{

Most functions here mirror their equivalent in the \CRANpkg{spray}
package [which the \proglang{C} code is largely copied from] or the
\code{frab} functionality.  So, for example, \code{num_eq_sparsetable()}
is the equivalent of \code{num_eq_spray()}.

The print method treats arity-2 \code{sparsetable} objects differently
from other arities.  By default, arity-2 \code{sparsetable} objects are
displayed as two-dimensional tables.  Control this behaviour with option
\code{print_2dsparsetables_as_matrices}:

\preformatted{
  options("print_2dsparsetables_as_matrices" = FALSE)
}

The default value for this option, non-\code{FALSE} (including its
out-of-the-box status of \dQuote{unset}), directs the print method to
coerce arity-2 \code{sparsetable} objects to two-dimensional tables
before printing.  If this option is \code{FALSE}, arity-2 sparsetables
are printed using matrix index form, just the same as any other arity.

Functions \code{rspar()}, \code{rspar2()}, and \code{rsparr()} create
random \code{sparsetable} objects of increasing complexity.  The
defaults are chosen to make the values of sensible sizes.

Function \code{drop()} takes a sparsetable object of arity one and
coerces to a \code{frab} object.

Function \code{dim()} returns a named vector, with names being the
\code{dimnames} of its argument.

Extraction and replacement methods are a subset of \CRANpkg{spray}
methods, but most should work.  There is special dispensation so that
standard idiom for arrays [e.g. \code{x['a','b','a']} and
\code{x['a','b','a'] <- 55}] work as expected, although the general
expectation is that access and replacement use (character) matrices and
an index object.  However, indexing by \code{disord} and \code{disindex}
objects should also work [e.g. \code{x[x>7]}].

The \CRANpkg{spray} source code and the \code{sparstable} functionality
hve about 90\% overlap; there were enough small differences between the
codes to make it worth maintaining two sets of source code, IMO.

There is a discussion of package idiom in the vignette,
\code{vignette("frab")}.

}
\note{
  The pronunciation of \dQuote{sparsetable} has the emphasis on the
  first syllable, so it rhymes with \dQuote{Barnable} or
  \dQuote{Barnstaple}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{frab-class}}}
\examples{

sparsetable(matrix(sample(letters[1:4],36,replace=TRUE),ncol=2),1:18)
sparsetable(matrix(sample(letters[1:4],39,replace=TRUE),ncol=3),1:13)

(x <- rspar2(9))
(y <- rspar2(9))
x + y

x["KT","FF"] <- 100
x

rsparr()

a <- rspar(d=4)
asum(a,"Feb")

}
