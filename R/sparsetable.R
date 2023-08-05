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

setMethod("dimnames","sparsetable",function(x){dimnames(index(x))})

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

setMethod("show", "sparsetable", function(object){print_sparsetable_matrixform(object)})
`print_sparsetable_matrixform` <- function(S){
    if(is.empty(S)){
        cat(paste('empty sparsetable with ', arity(S), ' columns\n',sep=""))
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

sparsetable <- function(i,v){
    jj <- sparsetable_maker(i,v)
    new("sparsetable",index=jj$index,values=jj$value)}

sparsetable_negative <- function(S){
    if(is.zero(S)){
        return(S)
    } else {
        return(sparsetable(index(S),-values(S)))
    }
}

`sparsetable_times_scalar` <- function(S,x){
    stopifnot(length(x)==1)
    return(sparsetable(sparsetable(index(S), x*coeffs(S)),arity=arity(S)))
}

sparsetable_plus_sparsetable <- function(S1,S2){
  stopifnot(arity(S1)==arity(S2))
  if(is.zero(S1)){
        return(S2)
    } else if(is.zero(S2)){
        return(S1)
      }

    return(sparsetablemaker(sparsetable_add(
    index(S1),coeffs(S1),
    index(S2),coeffs(S2)
        ),arity=arity(S1)))
}


`sparsetable_eq_sparsetable` <- function(S1,S2){
    if(arity(S1) != arity(S2)){
        return(FALSE)
    } else if(nterms(S1) != nterms(S2)){
        return(FALSE)
    } else {
        return(sparsetable_equality(index(S1),coeffs(S1),index(S2),coeffs(S2)))
    }
}

