test_that("Test suite aaa.R",{
  x <- frab(c(a=1,b=2,c=3,i=4))
  expect_true(is.frab(x))
  expect_true(x == frab(c(b=2,a=1,i=4,c=3)))
  expect_true(x + c(a=1) == frab(c(a=2,b=2,i=4,c=3)))
  expect_true(c(a=1) + x == frab(c(a=2,b=2,i=4,c=3)))
  expect_true(is.namedvector(as.namedvector(x)))
  expect_true(is.namedlogical(as.namedvector(x)>2))
  expect_true(is.unnamedvector(as.vector(as.namedvector(x))))
  expect_true(x^2 == frab(c(a=1,b=4,c=9,i=16)))
  expect_true(2^x == frab(c(a=2,b=4,c=8,i=16)))

  doublex <- frab(c(c=6,a=2,i=8,b=4))
  minusx <- frab(c(c=-3,a=-1,i=-4,b=-2))
  expect_true(2*x == doublex)
  expect_true(x*2 == doublex)

  expect_true(x/0.5 == doublex)
  expect_true(1/frab(c(a=1,b=1,i=1)) == frab(c(a=1,i=1,b=1)))

  expect_true(x ==  x)
  expect_true(x == +x)

  expect_false(x != x)
  expect_false(x != +x)

  expect_true(-x ==  minusx)
  expect_true( x == -minusx)

  expect_error(x+5)
  expect_error(x-5)
  expect_error(2+x)
  expect_error(2-x)

  expect_true(sum(x == 2) == 1)
  expect_true(sum(x >  2) == 2)
  expect_true(sum(x >= 2) == 3)
  expect_true(sum(x <  2) == 1)
  expect_true(sum(x <= 2) == 2)

  expect_true(sum(2 == x) == 1)
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


  
})
