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
\alias{is.empty,sparsetable-method}
\alias{arity}
\alias{arity,sparsetable-method}
\alias{nterms}
\alias{nterms,sparsetable-method}
\alias{show,sparsetable-method}
\alias{print_sparsetable_matrixform}
\alias{sparsetable_to_table}
\alias{sparsetable_asum_include}
\alias{sparsetable_asum_exclude}
\alias{table_to_sparsetable}
\alias{sparsetable}
\alias{sparsetable_pmax}
\alias{sparsetable_pmin}
\alias{sparsetable_negative}
\alias{sparsetable_times_scalar}
\alias{sparsetable_eq_sparsetable}
\alias{sparsetable_equality}
\alias{sparsetable_maker}
\alias{rspar}
\alias{rspar2}
\alias{rsparrr}
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
\alias{[,sparsetable,ANY,ANY,ANY-method}
\alias{[<-,sparsetable-method}
\alias{[<-,sparsetable,ANY,ANY,ANY-method}
\title{Generalized sparse tables: \code{sparsetable} objects}
\description{Package idiom for creating and manipulating
  \code{sparsetable} objects}
\details{

Most functions here mirror their equivalent in the \CRANpkg{spray}
package [which the \proglang{C} code is largely copied from] or the
\code{frab} functionality.  So, for example, \code{num_eq_sparsetable()}
is the equivalent of \code{num_eq_spray()}.

The print method coerces \code{sparsetable} objects to two-dimensional
tables.

Functions\code{rspar()}, \code{rspar2()}, and \code{rsparrr()} create
random \code{sparsetable} objects of increasing complexity.
}
\note{
  The pronunciation of \dQuote{sparsetable} has the emphasis on the
  first syllable, so it rhymes with \dQuote{vegetable}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{frab-class}}}
\examples{

(x <- rspar2())
(y <- rspar2())
x + y

}
