test_that("Test suite aaa.R",{
  x <- frab(c(a=1,b=2,c=3,i=4))
  expect_true(is.frab(x))
  expect_true(is.frab(as.frab(x)))
  expect_true(x == frab(c(b=2,a=1,i=4,c=3)))
  expect_true(x + c(a=1) == frab(c(a=2,b=2,i=4,c=3)))
  expect_true(c(a=1) + x == frab(c(a=2,b=2,i=4,c=3)))
  expect_error(x + 1:4)
  expect_error(x * 1:4)
  expect_error(x ^ 1:4)

  jjx <- x
  jjx[] <- 1
  expect_true(jjx == 1/jjx)
  expect_error(as.frab(1:5))

  expect_error(x > as.frab(c(a=3,b=4)))
  
  expect_true(is.namedvector(as.namedvector(x)))
  expect_true(is.namedlogical(as.namedvector(x)>2))
  expect_true(is.unnamedvector(as.vector(as.namedvector(x))))
  expect_true(x^2 == frab(c(a=1,b=4,c=9,i=16)))
  expect_error(2^x)
  expect_error(x^c(r=3))

  doublex <- frab(c(c=6,a=2,i=8,b=4))
  minusx <- frab(c(c=-3,a=-1,i=-4,b=-2))
  expect_true(2*x == doublex)
  expect_true(x*2 == doublex)

  expect_error(x * c(a=1))
  expect_error(c(a=1) * x)

  expect_true(x/0.5 == doublex)
  expect_true(1/frab(c(a=1,b=1,i=1)) == frab(c(a=1,i=1,b=1)))

  expect_true(x ==  x)
  expect_true(x == +x)

  expect_false(x != x)
  expect_false(x != +x)

  expect_true(-x ==  minusx)
  expect_true( x == -minusx)

  expect_true(x+5 ==  5+x)
  expect_true(x-5 == -5+x)

  expect_true(-x+5 ==   5-x)
  expect_true(-x-5 ==  -5-x)

  expect_true(sum(x == 2) == 1)
  expect_true(sum(x != 2) == 3)
  expect_true(sum(x >  2) == 2)
  expect_true(sum(x >= 2) == 3)
  expect_true(sum(x <  2) == 1)
  expect_true(sum(x <= 2) == 2)

  expect_true(sum(2 == x) == 1)
  expect_true(sum(2 != x) == 3)
  expect_true(sum(2 <= x) == 3)
  expect_true(sum(2 >  x) == 1)
  expect_true(sum(2 >= x) == 2)
  expect_true(sum(2 <  x) == 2)
  
  expect_silent(y <- frab(c(i= -4,a=2,b=1)))
  expect_true(x+y == frab(c(a=3,b=3,c=3)))
  expect_true(x-y == frab(c(i=8, a=-1, b=1, c=3)))
                          

  expect_output(print(x))
  options("frab_print_hash" = TRUE)
  expect_output(print(x))
  expect_output(print(x*0))
  options("frab_print_hash" = FALSE)
  expect_output(print(x))
  options("frab_print_hash" = 233)
  expect_output(print(x))
  options("frab_print_hash" = NULL)
  expect_output(print(x))


  expect_true(x["a"] == 1)
  xnew <- x
  xnew["e"] <- 7
  expect_true(xnew == frab(c(a=1,b=2,e=7,c=3,i=4)))

  xnew["e"] <- 6 
  expect_true(xnew == frab(c(a=1,b=2,e=6,c=3,i=4)))

  xnew["e"] <- 0
  expect_true(xnew == x)

  expect_true(all(sort(values(x[c("a","b")])) == 1:2))

  expect_true(x[x>2] == frab(c(c=3,i=4)))
  expect_true(x[x<2] == frab(c(a=1)))

  xnew <- x
  xnew[xnew>2] <- 33

  jj <- frab(c(b=2,a=1,c=33,i=33))
  expect_true(xnew == jj)

  xnew["i"] <- 4
  xnew["c"] <- 3

  expect_true(xnew == x)

  xnew <- x
  xnew[c("c","i")] <- 33
  expect_true(xnew == jj)

  expect_true(x[which(x>2)] == frab(c(i=4,c=3)))

  xnew <- x
  expect_silent(xnew[which(xnew>2)] <- 33)
  expect_true(xnew == jj)

  d <- disordR::disord(c("b","c"))
  expect_error(ignore <- x[d])
  xnew <- x
  expect_silent(xnew[d] <- 77)
  jj <- frab(c(a=1,b=77,c=77,i=4))
  expect_true(xnew == jj)

  xnew <- x
  expect_error(x[d] <- 1:2)
  expect_error(x[c("a","b")] <- 1:2)
  expect_error(x[x>2] <- 1:2)

  xnew <- x
  expect_error(xnew[] <- 1:4)

  xnew <- x
  xnew[] <- 5
  expect_true(xnew == frab(c(a=5,b=5,c=5,i=5)))

  xnew <- x
  f <- function(x){x^2}
  expect_error(xnew[f])
  expect_error(xnew[f] <- 3)
  expect_error(xnew[]  <- f)

  expect_error(which(x))
  expect_silent(ignore <- x[])
  expect_true(length(x) == 4)

  expect_true(as.frab(as.table(x)) == x)

  rm(x)


  expect_true(is.zero(zero()))
  expect_true(zero()  ==  -zero())
  expect_true(is.zero(zero() + zero()))
  expect_true(is.zero(zero() - zero()))
  
  expect_true(frab(c(a=1,b=0,c=3)) == frab(c(a=1,c=3)))
  expect_false(frab(c(a=1,b=2,c=3)) == frab(c(a=1,b=3,c=3)))
  expect_false(frab(c(a=1,b=2,c=3)) == frab(c(a=1,b=2)))
  expect_true(frab(c(c=1,b=3,c=-1)) == frab(c(b=3)))
  expect_error(values(c(c=1,b=3,c=-1)))   # values() needs a frab

  expect_error(c_frab_identity(letters[1:6],1:5))

  expect_output(print(zero()))
  expect_output(print(frab(c(a=1,b=3,c=5))*0))
  
  jj <- c(a=3,b=1,d=3)
  expect_false(is.namedlogical(jj))
  expect_false(is.unnamedlogical(jj))
  expect_true(is.namedlogical(jj>2))
  expect_false(is.unnamedlogical(jj>2))

  jj <- as.vector(jj)
  expect_false(is.namedlogical(jj))
  expect_false(is.unnamedlogical(jj))
  expect_false(is.namedlogical(jj>2))
  expect_true(is.unnamedlogical(jj>2))

  x <- frab(c(b=6,a=3,y=8))
  expect_error(values(x) <- 1:3)
  values(x) <- 3
  expect_true(x == frab(c(a=3,b=3,y=3)))

  x <- frab(c(b=6,a=3,y=8))
  values(x) <- values(x)^2
  expect_true(x == frab(c(a=9,b=36,y=64)))
  
  x <- frab(c(b=6,a=3,y=8))
  expect_error(values(x) <- disord(letters[1:3]))


  x <- frab(c(b=6,a=3,y=8))
  names(x) <- toupper(names(x))
  expect_true(x == frab(c(Y=8 , B=6 , A=3)))

  expect_error(names(x) <- "o")
  expect_error(names(x) <- letters)

  x <- frab(c(u=9))
  names(x) <- "foo"
  expect_true(x == frab(c(foo=9)))

  x <- frab(c(a=6,b=3,c=2))
  y <- frab(c(b=1,c=5,x=3,yy=8))
  expect_true(y*x == frab(c(b=3,c=10)))
  x <- frab(c(a=6,b=3,c=2))
  y <- frab(c(b=1,c=5,x=3,yy=8))
  expect_true(x*y == frab(c(b=3,c=10)))
  expect_true(x*y == y*x)

  x1 <- frab(c(b=6,a=3,y=8))
  x2 <- frab(c(a=6,b=3,c=2))
  x3 <- frab(c(b=1,c=5,x=3,yy=8))
  x4 <- frab(c(yy=-3,a=5,c=3))
  expect_true(pmax(x1,x2,x3,x4) == frab(c(a=6,b=6,c=5,x=3,y=8,yy=8)))
  expect_true(pmin(x1,x2,x3,x4) == frab(c(yy=-3)))
  
  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  x["b"] <- NA
  x[is.na(x)] <- 33
  expect_true(x == frab(c(a=1,b=33,c=2,d=5,e=6,f=-9)))

  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  x[c("b","e")] <- NA
  x[is.na(x)] <- 34
  expect_true(x == frab(c(a=1,b=34,c=2,d=5,e=34,f=-9)))

  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  x[x<0] <- NA
  x[is.na(x)] <- 35
  expect_true(x == frab(c(a=1,b=35,c=2,d=5,e=6,f=35)))

  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  expect_silent(is.na(x) <- x>0)
  expect_true(x["b"] == -4)
  
  expect_error(list_to_frab(list(names=frab::values(rfrab()))))

  expect_true(is.frab(rfrabb()))
  expect_true(is.frab(rfrabbb()))
  expect_output(print(rfrabb()*0))

  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  is.na(x) <- x>0
  expect_true(all(x[is.notna(x)] < 0))

  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  expect_true(max(x) == 6)
  expect_true(min(x) == -9)
  expect_true(sum(x) == 1)
  expect_true(all(range(x) == c(-9,6)))
  expect_error(prod(x))
  values(x) <- 1
  expect_true(x == 1/x)

  expect_true(all(pmax(1:5,5:1) == c(5,4,3,4,5)))
  expect_true(all(pmin(1:5,5:1) == c(1,2,3,2,1)))



  x <- frab(c(a=1,b=-4,c=2,d=5,e=6,f=-9))
  expect_error(x[] <- x)
  expect_error(x[x != 6] <- x)
  expect_error(x[values(x)] <- x)
  expect_error(x[x<5] <- 6*x[x<5])
  
})
