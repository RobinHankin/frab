setClass("frab",slots=c(x="numeric"))  # x is a named vector

`is.frab` <- function(x){inherits(x,"frab")}

setGeneric("names")
setMethod("names","frab",
          function(x){
              return(disord(names(x@x),h=hashcal(x@x)))
          } )

setReplaceMethod("names",signature(x="frab",value="disord"),
                 function(x,value){
                     v <- values(x)
                     stopifnot(consistent(v,value))
                     frab(setNames(elements(v),elements(value)))
                 } )

setReplaceMethod("names",signature(x="frab",value="character"),
                 function(x,value){
                     stopifnot(length(x)==1)
                     frab(setNames(values(x),value))
                 } )

setGeneric("values",function(x){standardGeneric("values")})
setMethod("values","frab",
          function(x){
            return(disord(as.numeric(x@x),h=hashcal(x@x)))
            ## no occurrences of "@" below this line; accessor methods end
          } )

setGeneric("values<-",function(x,value){standardGeneric("values<-")})
setReplaceMethod("values",signature(x="frab",value="numeric"),
                 function(x,value){
                     v <- values(x)
                     v[] <- value
                     frab(setNames(elements(v),elements(names(x))))
                 } )

setReplaceMethod("values",signature(x="frab",value="disord"),
                 function(x,value){
                     n <- names(x)
                     stopifnot(consistent(n,value))
                     frab(setNames(elements(value),elements(n)))
                 } )

setGeneric("namedvector",function(x){standardGeneric("namedvector")})
setMethod("namedvector","frab",function(x){x@x})

`as.namedvector`   <- function(v){namedvector(v)}
`is.namedvector`   <- function(v){is.vector(v) && is.numeric(v) && !is.null(names(v))}
`is.unnamedvector` <- function(v){is.vector(v) && is.numeric(v) &&  is.null(names(v))}
`is.namedlogical`   <- function(v){is.vector(v) && is.logical(v) && !is.null(names(v))}
`is.unnamedlogical` <- function(v){is.vector(v) && is.logical(v) &&  is.null(names(v))}

`frab` <- function(x){ # nv == named vector
  stopifnot(is.namedvector(x))
  jj <- c_frab_identity(names(x),x)
  new("frab", x=setNames(jj$values,jj$names))  # this is the only place new() is called
}

`list_to_frab` <- function(L){
  v <- L$values
  n <- L$names
  if(inherits(n,"disord") || inherits(v,"disord")){
    stop("not currently implemented")
  }
  frab(setNames(v,n))
}

`is.1dtable` <- function(x){is.table(x) && length(dim(x)==1)}
  
`table_to_frab` <- function(x){frab(setNames(as.vector(x),names(x)))}
setGeneric("as.table")
setMethod("as.table","frab",function(x,...){structure(as.namedvector(x),dim=length(x),dimnames=structure(list(names(x)),names=""),class="table")})

`as.frab` <- function(x){
  if(is.namedvector(x)){ 
    return(frab(x))
  } else if(is.list(x)){
    return(list_to_frab(x))
  } else if(is.1dtable(x)){
    return(table_to_frab(x))
  } else if(is.sparsetable(x)){
    return(sparsetable_to_frab(x))
  } else if(is.frab(x)){
    return(x)
  } else {
    stop("argument not recognised; did you mean to pass a _named_ numeric vector?")
  }
}

`frab_negative` <- function(x){frab(setNames(elements(-values(x)),elements(names(x)))) }
`frab_reciprocal` <- function(x){frab(setNames(elements(1/values(x)),elements(names(x)))) }
`frab_plus_frab` <- function(F1,F2){
  as.frab(c_frab_add(elements(names(F1)), elements(values(F1)),
                     elements(names(F2)), elements(values(F2))))
}

`frab_multiply_frab` <- function(F1,F2){
  as.frab(c_frab_multiply(elements(names(F1)), elements(values(F1)),
                     elements(names(F2)), elements(values(F2))))
}

`frab_plus_numeric`     <- function(e1,e2){if(is.namedvector(e2)){return(e1+frab(e2))}else{return(frab(setNames(elements(values(e1)+e2),elements(names(e1)))))}}
`frab_multiply_numeric` <- function(e1,e2){if(is.namedvector(e2)){stop("not defined")}else{return(frab(setNames(elements(values(e1)*e2),elements(names(e1)))))}}
`frab_power_numeric`    <- function(e1,e2){if(is.namedvector(e2)){stop("not defined")}else{return(frab(setNames(elements(values(e1)^e2),elements(names(e1)))))}}
`numeric_power_frab`    <- function(e1,e2){stop("<numeric> ^ <frab> not defined")}

`frab_unary` <- function(e1,e2){
  switch(.Generic,
         "+" = e1,
         "-" = frab_negative(e1),
         stop(gettextf("unary operator %s not implemented on frabs", dQuote(.Generic)))
         )
}

`frab_arith_frab` <- function(e1,e2){
  e1 <- as.frab(e1)
  e2 <- as.frab(e2)
  switch(.Generic,
         "+" = frab_plus_frab(e1, e2),
         "-" = frab_plus_frab(e1, frab_negative(e2)),
         "*" = frab_multiply_frab(e1, e2),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`frab_arith_numeric` <- function(e1,e2){ # e1 frab, e2 numeric; e2 might be a named vector.
  switch(.Generic,
         "+" = frab_plus_numeric(e1, e2),
         "-" = frab_plus_numeric(e1, -e2),
         "*" = frab_multiply_numeric(e1,e2),
         "/" = frab_multiply_numeric(e1,1/e2),
         "^" = frab_power_numeric(e1,e2),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`numeric_arith_frab` <- function(e1,e2){ # e1 numeric, e2 frab; e2 _might_ be a named vector.
  switch(.Generic,
         "+" = frab_plus_numeric( e2,e1),
         "-" = frab_plus_numeric(-e2,e1),
         "*" = frab_multiply_numeric(e2,e1), 
         "/" = frab_multiply_numeric(frab_reciprocal(e2),e1),
         "^" = numeric_power_frab(e1,e2),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`frab_eq` <- function(e1,e2){
  c_frab_eq(elements(names(e1)), elements(values(e1)),
            elements(names(e2)), elements(values(e2)))
}

`frab_compare_frab` <- function(e1,e2){
  switch(.Generic,
         "==" =  frab_eq(e1, e2),
         "!=" = !frab_eq(e1, e2),
         stop(gettextf("comparison '%s' not for frabs", dQuote(.Generic)))
         )
}

`frab_eq_num` <- function(e1,e2){values(e1) == e2}
`frab_ne_num` <- function(e1,e2){values(e1) != e2}
`frab_gt_num` <- function(e1,e2){values(e1) >  e2}
`frab_ge_num` <- function(e1,e2){values(e1) >= e2}
`frab_lt_num` <- function(e1,e2){values(e1) <  e2}
`frab_le_num` <- function(e1,e2){values(e1) <= e2}

`frab_compare_numeric` <- function(e1,e2){  # rfrab() > 3
  switch(.Generic,
         "==" = frab_eq_num(e1, e2),
         "!=" = frab_ne_num(e1, e2),
         ">"  = frab_gt_num(e1, e2),
         ">=" = frab_ge_num(e1, e2),
         "<"  = frab_lt_num(e1, e2),
         "<=" = frab_le_num(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }
    

`num_eq_frab` <- function(e1,e2){e1 == values(e2)}
`num_ne_frab` <- function(e1,e2){e1 != values(e2)}
`num_gt_frab` <- function(e1,e2){e1 >  values(e2)}
`num_ge_frab` <- function(e1,e2){e1 >= values(e2)}
`num_lt_frab` <- function(e1,e2){e1 <  values(e2)}
`num_le_frab` <- function(e1,e2){e1 <= values(e2)}

`numeric_compare_frab` <- function(e1,e2){  # 4 <= rfrab()
  switch(.Generic,
         "==" = num_eq_frab(e1, e2),
         "!=" = num_ne_frab(e1, e2),
         ">"  = num_gt_frab(e1, e2),
         ">=" = num_ge_frab(e1, e2),
         "<"  = num_lt_frab(e1, e2),
         "<=" = num_le_frab(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }

setMethod("Arith"  , signature(e1="frab"   , e2="missing"), frab_unary        )
setMethod("Arith"  , signature(e1="frab"   , e2="frab"   ), frab_arith_frab   )
setMethod("Arith"  , signature(e1="frab"   , e2="numeric"), frab_arith_numeric)
setMethod("Arith"  , signature(e1="numeric", e2="frab"   ), numeric_arith_frab)
setMethod("Arith"  , signature(e1="ANY"    , e2="frab"   ), frab_arith_frab   )
setMethod("Arith"  , signature(e1="frab"   , e2="ANY"   ), frab_arith_frab    )

setMethod("Compare", signature(e1="frab"    , e2="frab"   ),    frab_compare_frab   )
setMethod("Compare", signature(e1="frab"    , e2="numeric"),    frab_compare_numeric)
setMethod("Compare", signature(e1="numeric" , e2="frab"   ), numeric_compare_frab   )

`rfrab` <- function(n=9,v=seq_len(5),symb=letters[seq_len(9)]){
  frab(setNames(sample(v,n,replace=TRUE),sample(symb,n,replace=TRUE)))
}
`rfrabb` <- function(n=100,v=-5:5,symb=letters){rfrab(n=n,v=v,symb=symb)}

`rfrabbb` <- function(n=5000,v=-10:10,symb=letters,i=3){
  rfrab(n=n,v=v,symb=apply(expand.grid(rep(list(symb),i)),1,paste,collapse=""))
}
  
setMethod("show", "frab", function(object){frab_print(object)})

"frab_print" <- function(object){
    x <- as.namedvector(object)
    if(is.empty(object)){
      if(isTRUE(getOption("frab_print_hash"))){
        cat("The empty frab object with hash", hashcal(x), "\n")
      } else {
        cat("The empty frab object\n")
      }
    } else {
      if(isTRUE(getOption("frab_print_hash"))){
        cat("A frab object with hash", hashcal(x), "and entries\n")
      } else {
        cat("A frab object with entries\n")
      }
      print(x)
    }
    return(invisible(object))
}

setMethod("[", signature("frab", i="character",j="missing"),
          function(x,i,j){
              s <- elements(names(x))
              p <- elements(values(x))
              wanted <- s %in% i
              frab(setNames(p[wanted],s[wanted]))
          } )

setMethod("[", signature("frab", i="disord",j="missing"),
          function(x,i,j){
              frab(setNames(elements(values(x)[i]), elements(names(x)[i])))
          } )

setMethod("[",  # x[]
          signature("frab", i="missing",j="missing"),
          function(x,i,j){x})

setMethod("[",
          signature("frab", i="ANY",j="missing"),
          function(x,i,j,drop){stop("not implemented")}
          )

setMethod("[",
          signature("frab", i="disindex",j="missing"),
          function(x,i,j,drop){
              frab(setNames(elements(values(x)[i]),elements(names(x)[i])))
          } )

setReplaceMethod("[",signature(x="frab",i="character",j="missing",value="numeric"),
                 function(x,i,j,value){
                     s <- names(x)
                     p <- values(x)
                     p[s %in% i] <- value
                     new_names <- i[!(i %in% s)]
                     return(
                         as.frab(list(values=elements(p),names=elements(s))) + 
                         setNames(rep(value,length(new_names)),new_names)
                     )
                 })

setReplaceMethod("[",signature(x="frab",i="character",j="missing",value="logical"),
                 function(x,i,j,value){
                     x[i] <- as.numeric(value)  # the meat
                     return(x)
                 })

setReplaceMethod("[",signature(x="frab",i="disord",j="missing",value="numeric"),
                 function(x,i,j,value){
                     s <- names(x)
                     p <- values(x)
                     if(is.logical(i)){
                         p[i] <- value
                         return(frab(setNames(elements(p),s)))
                     } else {
                         i <- elements(i)
                         p[s %in% i] <- value
                         new_names <- i[!(i %in% s)]
                     }
                     return(
                         as.frab(list(values=elements(p),names=elements(s))) + 
                         setNames(rep(value,length(new_names)),new_names)
                     )
                 })

setReplaceMethod("[",signature(x="frab",i="disord",j="missing",value="logical"),
                 function(x,i,j,value){
                     x[i] <- as.numeric(value)  # the meat
                     return(x)
                 } )

setReplaceMethod("[",signature(x="frab",i="disord",j="missing",value="frab"),
                 function(x,i,j,value){
                   stop("not currently implemented.  Idiom such as x[x<0] <- -x[x<0] is disord-compliant [and meaningful] but not yet implemented")
                 } )

setReplaceMethod("[",signature(x="frab",i="disindex",j="missing",value="numeric"),
                 function(x,i,j,value){
                     p <- values(x)
                     p[i] <- value
                     return(frab(setNames(elements(p),names(x))))
                 } )

setReplaceMethod("[",signature(x="frab",i="missing",j="missing",value="numeric"),
                 function(x,i,j,value){
                   v <- values(x)
                   v[] <- value  # disord discipline violations trapped here
                   return(frab(setNames(v,names(x))))
                 } )

setReplaceMethod("[",signature(x="frab",i="missing",j="missing",value="frab"),
                 function(x,i,j,value){
                   stop("x[] <- y (with x, y frabs) does not make sense; try x <- y?")
                 } )

setReplaceMethod("[",signature(x="frab",i="missing",j="missing",value="ANY"),
                 function(x,i,j,value){
                   stop("frab,missing,missing,ANY-method not implemented")
                 } )


setReplaceMethod("[",signature(x="frab",i="ANY",j="ANY",value="ANY"),
                 function(x,i,j,value){
                   stop("replacement operation not defined in this case")
                 } )

setGeneric("which")
setMethod("which","frab",
          function(x){
              stop("which() not defined for frabs; did you mean something like which(x>0)?")
          } )

setGeneric("length")
setMethod("length","frab", function(x){length(values(x))})

setGeneric("sort")  # from the disordR package

`zero` <- function(...){as.frab(list(values=numeric(0),names=character(0)))}
`is.zero` <- function(x){x==zero()}
`is.empty` <- is.zero

setGeneric("pmax",function(...){standardGeneric("pmax")})
setGeneric("pmin",function(...){standardGeneric("pmin")})

`pmax_pair` <- function(F1,F2){
  as.frab(c_frab_pmax(elements(names(F1)), elements(values(F1)),
                      elements(names(F2)), elements(values(F2))))
}

`pmin_pair` <- function(F1,F2){ -pmax_pair(-F1,-F2)}

`pmax_dots` <- function(x,...){
  if(nargs()==1){
    return(x)
    } else if(nargs()<3){
    return(pmax_pair(x, ...))
  } else {
    return(pmax_pair(x, pmax_dots(...)))
  }
}

`pmin_dots` <- function(x,...){
  if(nargs()==1){
    return(x)
  } else if(nargs()<3){
    return(pmin_pair(x, ...))
  } else {
    return(pmin_pair(x, pmin_dots(...)))
  }
}

setMethod("pmax",signature("..."="frab"), function(...){pmax_dots(...)} )
setMethod("pmin",signature("..."="frab"), function(...){pmin_dots(...)} )

setMethod("pmax",signature("..."="ANY"),function(...,na.rm=FALSE){base::pmax(..., na.rm=na.rm)})
setMethod("pmin",signature("..."="ANY"),function(...,na.rm=FALSE){base::pmin(..., na.rm=na.rm)})

setGeneric("is.na")
setMethod("is.na","frab",function(x){which(is.na(values(x)))})
setGeneric("is.na<-")
setReplaceMethod("is.na",signature("frab",value="disord"),
                 function(x,value){
                   v <- values(x)
                   is.na(v) <- value # the meat
                   return(frab(setNames(elements(v),elements(names(x)))))
                 } )

setGeneric("is.notna",function(x){standardGeneric("is.notna")})
setMethod("is.notna","frab",function(x){which(!is.na(values(x)))})

setMethod("Summary", "frab",
          function(x, ..., na.rm=FALSE){
            switch(.Generic,
                   max    = max(values(x)),
                   min    = min(values(x)),
                   range  = c(min(values(x)),max(values(x))),
                   sum    = sum(values(x)),
                   stop(gettextf("Summary function %s not implemented on frabs", dQuote(.Generic)))
                   )
          }
          )




