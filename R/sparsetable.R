setClass("sparsetable",
         representation = representation(index="matrix",values="numeric"),
         prototype      = list(index=matrix(),values=numeric()),
         )

setGeneric("index",function(x){standardGeneric("index")})
setMethod("index","sparsetable",function(x){x@index})
setMethod("values","sparsetable",
          function(x){
              disord(as.numeric(x@values),h=hashcal(list(x@index,x@values)))
              ## no occurrences of "@" below this line; accessor methods end
          } )

setMethod("names","sparsetable",
          function(x){
              stop("sparsetable objects do not have a 'names' attribute; try dimnames()")
          } )

setMethod("dimnames","sparsetable",function(x){colnames(index(x))})
setReplaceMethod("dimnames","sparsetable",function(x,value){
  I <- index(x)
  colnames(I) <- value # the meat
  return(sparsetable(I,values(x)))
} )
  
setGeneric("nterms",function(x){standardGeneric("nterms")})
setMethod("nterms","sparsetable",function(x){nrow(index(x))})

setValidity("sparsetable",function(object){
    i <- index(object)
    v <- values(object)
    if(!is.character(i)){
        stop("not a character, we need a character vector")
    } else if(nrow(i) != length(v)){
        stop("length of values must match number of rows of index")
    } else {
        return(TRUE)
    }
} )

setGeneric("is.empty",function(x){standardGeneric("is.empty")})
setMethod("is.empty","sparsetable",function(x){nrow(index(x))==0})
setGeneric("arity",function(x){standardGeneric("arity")})
setMethod("arity","sparsetable",function(x){ncol(index(x))})
setMethod("dim","sparsetable",function(x){apply(index(x),2,function(x){length(unique(x))})})

setGeneric("as.array")
setMethod("as.array","sparsetable",function(x){sparsetable_to_array(x)})

`sparsetable_to_array` <- function(x){
  if(is.empty(x)){return(array(0,rep(0,arity(x))))}
  I <- apply(index(x),2,function(x){as.numeric(as.factor(x))})
  dims <- apply(I,2,max)
  out <- array(0,dims)
  out[I] <- values(x)
  L <- apply(index(x),2,function(x){levels(as.factor(x))},simplify=FALSE)
  names(L) <- colnames(I)
  dimnames(out) <- L
  return(out)
}

`array_to_sparsetable` <- function(x){

    I <- which(x != 0,arr.ind=TRUE)
    J <- I
    colnames(J) <- names(dimnames(x))
    for(i in seq_len(ncol(I))){
        J[,i] <- (dimnames(x)[[i]])[I[,i]]  # the meat
    }
    return(sparsetable(J,x[x!=0]))
}

`sparsetable_to_frab` <- function(x){
  frab(setNames(elements(values(x)),apply(index(x),1,paste,collapse="_")))
}

setMethod("show", "sparsetable", function(object){print_sparsetable_matrixform(object)})

`print_sparsetable_matrixform` <- function(S){
    if(is.empty(S)){
        cat(paste('empty sparsetable with ', arity(S), ' columns\n',sep=""))
    } else if((arity(S)==2)  && !isFALSE(getOption("print_2dsparsetables_as_matrices"))){
        print(sparsetable_to_array(S))
    } else {
        jj <-
            data.frame(index(S),symbol= " = ", val=round(elements(values(S)),getOption("digits")))
        mdc <- colnames(index(S))
        if(is.null(mdc)){
            colnames(jj) <- c(rep(" ",arity(S)+1),'val')
        } else {
            colnames(jj) <- c(mdc[seq_len(arity(S))],' ','val')
        }
        print(jj,row.names=FALSE)
    }
    return(invisible(S))
}

`sparsetable` <- function(i,v=1){
    if(length(v)==1){v <- rep(v,nrow(i))}
    stopifnot(nrow(i) == length(v))
    jj <- sparsetable_maker(i,v)
    if(is.null(jj$index)){
        jj$index <-matrix(character(0),0,ncol(i))
        jj$value <- numeric(0)
    }
    colnames(jj$index) <- colnames(i)
    new("sparsetable",index=jj$index,values=jj$value)} # This is the only time new("sparsetable",...) is called

`is.sparsetable` <- function(x){inherits(x,"sparsetable")}

`as.sparsetable` <- function(x){
    if(is.sparsetable(x)){
        return(x)
    } else if(is.frab(x)){
        return(sparsetable(cbind(elements(names(x))),elements(values(x))))
    } else if(is.list(x)){
        return(sparsetable(x$index,x$value))
    } else if(is.table(x)){
        return(array_to_sparsetable(as.array(x)))
    } else if(is.array(x)){
      return(array_to_sparsetable(x))
    }
}

`sparsetable_negative` <- function(S){
    if(is.zero(S)){
        return(S)
    } else {
        return(sparsetable(index(S),-values(S)))
    }
}

`sparsetable_eq_sparsetable` <- function(S1,S2){
    if(arity(S1) != arity(S2)){
        return(FALSE)
    } else if(nterms(S1) != nterms(S2)){
        return(FALSE)
    } else {
        return(sparsetable_equality(index(S1),values(S1),index(S2),values(S2)))
    }
}

`rspar` <- function(n=15,l=3,d=3){
  sparsetable(
      matrix(
          letters[sample(seq_len(l),n*d,replace=TRUE)],
          n, d, dimnames=list(NULL,month.abb[seq_len(d)])),seq_len(n))
}

`rspar2` <- function(n=15,l=6){
    sparsetable(as.matrix(data.frame(
        foo=letters[sample(seq_len(l),n,replace=TRUE)],
        bar=LETTERS[sample(seq_len(l),n,replace=TRUE)])),
        seq_len(n))
}

`rsparr` <- function(n=20,d=6,l=5,s=4){
  I <- sapply(seq_len(d),function(d){sample(replicate(s,paste(sample(letters[seq_len(l)],d,replace=TRUE),collapse="")),n,replace=TRUE)})
  colnames(I) <- month.abb[seq_len(d)]
  return(sparsetable(I,seq_len(n)))
}

`sparsetable_negative` <- function(x){sparsetable(index(x), -values(x))}
`sparsetable_reciprocal` <- function(x){stop("inverse not implemented")}
`sparsetable_plus_sparsetable` <- function(F1,F2){
  stopifnot(arity(F1) == arity(F2))
  out <- (
      sparsetable_add(
          index(F1),values(F1),
          index(F2),values(F2)
      ))
  if(is.null(out$index)){
      out$index <- index(F1)[FALSE,]
      out$value <- numeric(0)
  }
  out <- as.sparsetable(out)
  if(is.null(dimnames(F1))){
    dimnames(out) <- dimnames(F2)
    } else {
      dimnames(out) <- dimnames(F1)
    }
  return(out)
}

`sparsetable_multiply_sparsetable` <- function(F1,F2){
    stop("multiplication not implemented")
}

`sparsetable_multiply_numeric` <- function(e1,e2){sparsetable(index(e1),values(e1)*e2)}
`sparsetable_power_numeric`    <- function(e1,e2){stop("sparsetable power not implemented")}
`numeric_multiply_sparsetable` <- function(e1,e2){sparsetable_multiply_numeric(e2,e1)}
`numeric_power_sparsetable`    <- function(e1,e2){stop("sparsetable power not implemented")}

`sparsetable_unary` <- function(e1,e2){
  switch(.Generic,
         "+" = e1,
         "-" = sparsetable_negative(e1),
         stop(gettextf("unary operator %s not implemented on sparsetables", dQuote(.Generic)))
         )
}

`sparsetable_arith_sparsetable` <- function(e1,e2){
  e1 <- as.sparsetable(e1)
  e2 <- as.sparsetable(e2)
  switch(.Generic,
         "+" = sparsetable_plus_sparsetable(e1, e2),
         "-" = sparsetable_plus_sparsetable(e1, sparsetable_negative(e2)),
         "*" = sparsetable_multiply_sparsetable(e1, e2),
         stop(gettextf("binary operator %s not implemented on sparsetables", dQuote(.Generic)))
         ) }

`sparsetable_arith_numeric` <- function(e1,e2){ # e1 sparsetable, e2 numeric; e2 might be a named vector.
  switch(.Generic,
         "*" = sparsetable_multiply_numeric(e1,e2),
         "/" = sparsetable_multiply_numeric(e1,1/e2),
         "^" = sparsetable_power_numeric(e1,e2),
         stop(gettextf("binary operator %s not implemented in this case", dQuote(.Generic)))
         ) }

`numeric_arith_sparsetable` <- function(e1,e2){ # e1 numeric, e2 sparsetable; e2 _might_ be a named vector.
  switch(.Generic,
         "*" = numeric_multiply_sparsetable(e1,e2), 
         "/" = numeric_multiply_sparsetable(e1,sparsetable_reciprocal(e2)), 
         "^" = numeric_power_sparsetable(e1,e2),
         stop(gettextf("binary operator %s not implemented in this case", dQuote(.Generic)))
         ) }


`sparsetable_compare_sparsetable` <- function(e1,e2){
  switch(.Generic,
         "==" =  sparsetable_eq_sparsetable(e1, e2),
         "!=" = !sparsetable_eq_sparsetable(e1, e2),
         stop(gettextf("comparison '%s' not for sparsetables", dQuote(.Generic)))
         )
}

`sparsetable_eq_num` <- function(e1,e2){values(e1) == e2}
`sparsetable_gt_num` <- function(e1,e2){values(e1) >  e2}
`sparsetable_ge_num` <- function(e1,e2){values(e1) >= e2}
`sparsetable_lt_num` <- function(e1,e2){values(e1) <  e2}
`sparsetable_le_num` <- function(e1,e2){values(e1) <= e2}

`sparsetable_compare_numeric` <- function(e1,e2){  # rsparsetable() > 3
  switch(.Generic,
         "==" =  sparsetable_eq_num(e1, e2),
         "!=" = !sparsetable_eq_num(e1, e2),
         ">"  =  sparsetable_gt_num(e1, e2),
         ">=" =  sparsetable_ge_num(e1, e2),
         "<"  =  sparsetable_lt_num(e1, e2),
         "<=" =  sparsetable_le_num(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }

`num_eq_sparsetable` <- function(e1,e2){e1 == values(e2)}
`num_gt_sparsetable` <- function(e1,e2){e1 >  values(e2)}
`num_ge_sparsetable` <- function(e1,e2){e1 >= values(e2)}
`num_lt_sparsetable` <- function(e1,e2){e1 <  values(e2)}
`num_le_sparsetable` <- function(e1,e2){e1 <= values(e2)}

`numeric_compare_sparsetable` <- function(e1,e2){  # 4 <= rsparsetable()
  switch(.Generic,
         "==" = num_eq_sparsetable(e1, e2),
         ">"  = num_gt_sparsetable(e1, e2),
         ">=" = num_ge_sparsetable(e1, e2),
         "<"  = num_lt_sparsetable(e1, e2),
         "<=" = num_le_sparsetable(e1, e2),
         stop(gettextf("Comparison operator %s not implemented in this case", dQuote(.Generic)))
         ) }

setMethod("Arith"  , signature(e1="sparsetable"   , e2="missing"), sparsetable_unary        )
setMethod("Arith"  , signature(e1="sparsetable"   , e2="sparsetable"   ), sparsetable_arith_sparsetable   )
setMethod("Arith"  , signature(e1="sparsetable"   , e2="numeric"), sparsetable_arith_numeric)
setMethod("Arith"  , signature(e1="numeric", e2="sparsetable"   ), numeric_arith_sparsetable)
setMethod("Arith"  , signature(e1="ANY"    , e2="sparsetable"   ), sparsetable_arith_sparsetable   )
setMethod("Arith"  , signature(e1="sparsetable"   , e2="ANY"   ), sparsetable_arith_sparsetable    )

setMethod("Compare", signature(e1="sparsetable"    , e2="sparsetable"   ),    sparsetable_compare_sparsetable   )
setMethod("Compare", signature(e1="sparsetable"    , e2="numeric"),    sparsetable_compare_numeric)
setMethod("Compare", signature(e1="numeric" , e2="sparsetable"   ), numeric_compare_sparsetable   )

setMethod("[",
          signature(x="sparsetable",i="ANY",j="ANY"),
          function(x,i, ...){
              if(is.matrix(i)){
                  out <- sparsetable_accessor(index(x),values(x), i)
              } else {
                  out <- sparsetable_accessor(index(x),values(x), matrix(c(i,j,unlist(list(...))),nrow=1))
              }
              return(out)
          } )

setMethod("[",signature(x="sparsetable",i="disord",j="missing"),
          function(x,i){
              sparsetable(index(x)[i,,drop=FALSE],values(x)[i])  # the meat
                 } )

setMethod("[",signature(x="sparsetable",i="disindex",j="missing"),
          function(x,i,j){
              vx <- frab::values(x)
              vi <- disordR::values(i)
              sparsetable(index(x)[vi,,drop=FALSE], vx[i]) # the meat
          } )

setReplaceMethod("[",signature(x="sparsetable",i="disord",j="missing",value="numeric"),
                 function(x,i,j,value){
                     v <- values(x)
                     stopifnot(consistent(v,i))
                     v[i] <- value # the meat
                     sparsetable(index(x),v)
                 } )

setReplaceMethod("[",signature(x="sparsetable",i="disindex",j="missing",value="ANY"),
                 function(x,i,j,...,value){
                     stopifnot(identical(hash(values(x)),hash(i)))
                     if(is.disord(value)){stop("replace methods for disindex do not take disords")}
                     jj <- values(x)
                     jj[disordR::values(i)] <- value # the meat
                     return(sparsetable(index(x),jj))
                 } )

setReplaceMethod("[",signature(x="sparsetable",value="ANY"),
                 function(x,i,j,...,value){
                     if(missing(i)){ # S[] <- something
                         if(is.sparsetable(value)){
                             return(
                                 as.sparsetable(sparsetable_overwrite(
                                     index(x    ),values(x    ),
                                     index(value),values(value))))
                         } else {
                             stop("replacement method  not defined (disord discipline violation?)")
                             ## return(sparsetable(index(x),value))
                         }
                     }
                     
                     if(is.matrix(i)){
                         M <- i
                     } else if(is.sparsetable(i)){
                         warning("possible disord discipline violation")
                         M <- index(i)
                     } else {
                         if(missing(j)){j <- NULL}
                         M <- as.matrix(expand.grid(c(list(i), j, list(...))))
                     }
                     if(ncol(M) != arity(x)){
                         stop("incorrect number of dimensions specified")
                     }
                     
                     if(length(value)==1){value <- rep(value,nrow(M))}
                     stopifnot(length(value)==nrow(M))
                     return(as.sparsetable(sparsetable_setter(index(x),values(x),M,value)))
                 }
                 )

setMethod("drop","sparsetable",function(x){frab(setNames(disordR::elements(values(x)),c(index(x))))})

`pmax_pair_sparsetable` <- function(F1,F2){
    as.sparsetable(sparsetable_pmax(
        index(F1),elements(values(F1)),
        index(F2),elements(values(F2))
        ))
}

`pmin_pair_sparsetable` <- function(F1,F2){
 -pmax_pair_sparsetable(-F1,-F2)}

`pmax_dots_sparsetable` <- function(x,...){
  if(nargs()==1){
    return(x)
    } else if(nargs()<3){
    return(pmax_pair_sparsetable(x, ...))
  } else {
    return(pmax_pair_sparsetable(x, pmax_dots_sparsetable(...)))
  }
}

`pmin_dots_sparsetable` <- function(x,...){
  if(nargs()==1){
    return(x)
  } else if(nargs()<3){
    return(pmin_pair_sparsetable(x, ...))
  } else {
    return(pmin_pair_sparsetable(x, pmin_dots_sparsetable(...)))
  }
}

setMethod("pmax",signature("..."="sparsetable"), function(...){pmax_dots_sparsetable(...)} )
setMethod("pmin",signature("..."="sparsetable"), function(...){pmin_dots_sparsetable(...)} )

setGeneric("asum",function(S,dims){standardGeneric("asum")})
setMethod("asum",signature(S="sparsetable"), function(S,dims){asum_sparsetable(S,dims)})

`asum_sparsetable` <- function(S, dims){
  if(is.character(dims)){
    dims <- which(dimnames(S) %in% dims)
  } else {
    if(is.logical(dims)){ dims <- which(dims)  }
    stopifnot(all(dims <= arity(S)))
    stopifnot(all(dims >0))
    stopifnot(all(dims == round(dims)))
  }
  jj <- sparsetable_asum_include(index(S),elements(values(S)),dims)
  I <- jj$index[,-dims,drop=FALSE]
  colnames(I) <- dimnames(S)[-dims]
  return(sparsetable(I,jj$value))
}

`asum_exclude_sparsetable` <- function(S,dims){
  if(is.character(dims)){
    dims <- which(!(dimnames(S) %in% dims))
  } else if(is.logical(dims)){
    dims <- !dims
  } else {
    dims <- which(!(seq_len(arity(S)) %in% dims))
  }
  return(asum_sparsetable(S, dims))
}
