setClass("frab",slots=c(x="numeric"))  # x is a named vector

`is.frab` <- function(x){inherits(x,"frab")}

`as.namedvector` <- function(object){object@x}  # should be a disord (?)

`is.namedvector`   <- function(v){is.vector(v) && is.numeric(v) && !is.null(names(v))}
`is.unnamedvector` <- function(v){is.vector(v) && is.numeric(v) &&  is.null(names(v))}

`is.namedlogical`   <- function(v){is.vector(v) && is.logical(v) && !is.null(names(v))}
`is.unnamedlogical` <- function(v){is.vector(v) && is.logical(v) &&  is.null(names(v))}

`ch4nv` <- function(v){  # check for named vector
  if(is.namedvector(v)){
    stop("named vector not admissible here")
  } else {
    return(v)
  }
}

`symbols` <- function(object){
  if(is.frab(object)){
    return(disord(names(object@x),h=hashcal(object@x)))
  } else {
    stop()
  }
}

`powers` <- function(object){
  if(is.frab(object)){
    return(disord(as.numeric(object@x),h=hashcal(object@x))) # no occurrences of "@" below this line; accessor methods end
  } else {
    stop()
  }
}

`nv_to_frab` <- function(x){ # nv == named vector
  stopifnot(is.namedvector(x))
  jj <- c_frab_identity(names(x),x)
  new("frab", x=setNames(jj$powers,jj$symbols))  # this is the only place new() is called
}

`list_to_frab` <- function(L){
  nv_to_frab(setNames(L$powers,L$symbols))
}

`frab` <- function(x){
  if(is.namedvector(x)){ 
    return(nv_to_frab(x))
  } else if(is.list(x)){
    return(list_to_frab(x))
  } else if(is.frab(x)){
    return(x)
  } else {
    stop("argument not recognised; did you mean to pass a _named_ numeric vector?")
  }
}

`nv_to_yesno` <- function(x){stop("not yet implemented.  Makes sense but i have not got round to it")}
`list_to_yesno` <- function(x){stop("not yet implemented.  Makes sense but i have not got round to it")}

`yesno` <- function(x){
  if(is.namedlogical(x)){
    return(nv_to_yesno(x))
  } else if(is.list(x)){
    return(list_to_yesno(x))
  } else if(is.yesno(x)){
    return(x)
  } else {
    stop("argument not recognised; did you mean to pass a _named_ logical vector?")
  }
}

`frab_negative` <- function(x){frab(setNames(elements(-powers(x)),elements(symbols(x)))) }
`frab_plus_frab` <- function(F1,F2){
  frab(c_frab_add(elements(symbols(F1)), elements(powers(F1)),
                  elements(symbols(F2)), elements(powers(F2))))
}
 
`frab_multiply_numeric` <- function(e1,e2){frab(setNames(elements(powers(e1)*ch4nv(e2)),elements(symbols(e1))))}
`frab_power_numeric`    <- function(e1,e2){frab(setNames(elements(powers(e1)^ch4nv(e2)),elements(symbols(e1))))}
`numeric_multiply_frab` <- function(e1,e2){frab(setNames(elements(ch4nv(e1)*powers(e2)),elements(symbols(e2))))}
`numeric_power_frab`    <- function(e1,e2){frab(setNames(elements(ch4nv(e1)^powers(e2)),elements(symbols(e2))))}

`frab_unary` <- function(e1,e2){
  switch(.Generic,
         "+" = e1,
         "-" = frab_negative(e1),
         stop(gettextf("unary operator %s not implemented on frabs", dQuote(.Generic)))
         )
}

`frab_arith_frab` <- function(e1,e2){
  switch(.Generic,
         "+" = frab_plus_frab(e1, e2),
         "-" = frab_plus_frab(e1, frab_negative(e2)),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`frab_arith_numeric` <- function(e1,e2){ # e1 frab, e2 numeric; e2 might be a named vector.
  switch(.Generic,
         "+" = frab_plus_frab(e1, frab(e2)),
         "-" = frab_plus_frab(e1, frab_negative(frab(e2))),
         "*" = frab_multiply_numeric(e1,e2),
         "^" = frab_power_numeric(e1,e2),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`numeric_arith_frab` <- function(e1,e2){ # e1 numeric, e2 frab; e2 _might_ be a named vector.
  switch(.Generic,
         "+" = frab_plus_frab(frab(e1),  e2),
         "-" = frab_plus_frab(frab(e1), -e2),
         "*" = numeric_multiply_frab(e2,e1),  # note swap
         "^" = numeric_power_frab(e1,e2),
         stop(gettextf("binary operator %s not implemented on frabs", dQuote(.Generic)))
         ) }

`frab_eq` <- function(e1,e2){
  c_frab_eq(elements(symbols(e1)), elements(powers(e1)),
            elements(symbols(e2)), elements(powers(e2)))
}


`frab_compare_frab` <- function(e1,e2){
  switch(.Generic,
         "==" = frab_eq(e1, e2),
         stop(gettextf("comparison '%s' not for frabs", dQuote(.Generic)))
         )
}

`frab_eq_num` <- function(e1,e2){disord(symbols(e1)[powers(e1) == e2],h=hashcal(as.namedvector(e1)))}
`frab_gt_num` <- function(e1,e2){disord(symbols(e1)[powers(e1) >  e2],h=hashcal(as.namedvector(e1)))}
`frab_ge_num` <- function(e1,e2){disord(symbols(e1)[powers(e1) >= e2],h=hashcal(as.namedvector(e1)))}
`frab_lt_num` <- function(e1,e2){disord(symbols(e1)[powers(e1) <  e2],h=hashcal(as.namedvector(e1)))}
`frab_le_num` <- function(e1,e2){disord(symbols(e1)[powers(e1) <= e2],h=hashcal(as.namedvector(e1)))}

`frab_compare_numeric` <- function(e1,e2){  # rfrab() > 3
  switch(.Generic,
         "==" = frab_eq_num(e1, e2),
         ">"  = frab_gt_num(e1, e2),
         ">=" = frab_ge_num(e1, e2),
         "<"  = frab_lt_num(e1, e2),
         "<=" = frab_le_num(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }
    

`num_eq_frab` <- function(e1,e2){disord(symbols(e2)[powers(e2) == e1],h=hashcal(as.namedvector(e2)))}
`num_gt_frab` <- function(e1,e2){disord(symbols(e2)[powers(e2) >  e1],h=hashcal(as.namedvector(e2)))}
`num_ge_frab` <- function(e1,e2){disord(symbols(e2)[powers(e2) >= e1],h=hashcal(as.namedvector(e2)))}
`num_lt_frab` <- function(e1,e2){disord(symbols(e2)[powers(e2) <  e1],h=hashcal(as.namedvector(e2)))}
`num_le_frab` <- function(e1,e2){disord(symbols(e2)[powers(e2) <= e1],h=hashcal(as.namedvector(e2)))}

`numeric_compare_frab` <- function(e1,e2){  # 4 <= rfrab()
  switch(.Generic,
         "==" = frab_eq_num(e1, e2),
         ">"  = frab_gt_num(e1, e2),
         ">=" = frab_ge_num(e1, e2),
         "<"  = frab_lt_num(e1, e2),
         "<=" = frab_le_num(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }

`frab_compare_error` <- function(e1,e2){
  stop(gettextf("binary operator %s not implemented in this case", dQuote(.Generic)))
}
 
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

setMethod("show", "frab", function(object){frab_print(object)})

"frab_print" <- function(x){
    x <- as.namedvector(x)
    cat("A frab object with hash", hashcal(x), "and entries\n")
    print(x)
    return(invisible(x))
}

setMethod("[", signature("frab", i="character",j="missing"),
          function(x,i,j){
              s <- elements(symbols(x))
              p <- elements(powers(x))
              wanted <- s %in% i
              frab(list(
                  symbols = s[wanted],
                  powers  = p[wanted]
              ))
          } )

setMethod("[", signature("frab", i="disord",j="missing"),
          function(x,i,j){x[elements(i)]})


setMethod("[",  # x[]
          signature("frab", i="missing",j="missing"),
          function(x,i,j){x})

setMethod("[",
          signature("frab", i="ANY",j="missing"),
          function(x,i,j,drop){stop("hotblack")}
          )

setReplaceMethod("[",signature(x="frab",i="character",j="missing",value="numeric"),
                 function(x,i,j,value){
                     s <- symbols(x)
                     p <- powers(x)
                     p[s %in% i] <- value
                     new_symbols <- i[!(i %in% s)]
                     return(
                         frab(list(powers=elements(p),symbols=elements(s))) + 
                         setNames(rep(value,length(new_symbols)),new_symbols)
                     )
                 })

setReplaceMethod("[",signature(x="frab",i="ANY",j="ANY",value="ANY"),
                 function(x,i,j,value){stop("replacement operation not defined in this case")}
                 )
