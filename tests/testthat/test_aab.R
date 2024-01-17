## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.

test_that("Test suite aab.R",{

checker1 <- function(A){
  expect_true(A == A)
  expect_true(A == +A)    
  expect_true(A + zero() ==  A)
  expect_true(A - zero() ==  A)
  expect_true(zero() + A ==  A)
  expect_true(zero() - A == -A)
  expect_true(is.zero(A-A))
  expect_true(A == -(-A))
  expect_true(A+A == 2*A)
  expect_true(A+A == A*2)

  expect_true(is.zero(A-A))
  expect_true(A+A+A == 3*A)
  expect_true(A+A+A == A*3)

  expect_true(A/2 + A/2 == A)

  expect_error(A&A)

  expect_true(pmax(A)==A)
  expect_true(pmin(A)==A)

  expect_true(pmax(A,A) == A)
  expect_true(pmin(A,A) == A)

  expect_true(A[NULL] + A == A)
  

  dA <- as.data.frame(A)
  Adash <- as.frab(dA)
  expect_true(is.data.frame(dA))
  expect_true(is.frab(Adash))
  expect_true(A == Adash)

}   # checker1() closes
  
checker2 <- function(A,B){
  expect_true(A+B == B+A) # 1.1
  expect_true(2*A+B == A+A+B)
  expect_true(A+2*B == B+B+A)

  expect_true(pmax(A,B) == pmax(B,A))
  expect_true(pmin(A,B) == pmin(B,A))

  expect_true(all(pmax(A,B)-A >= 0))
  expect_true(all(pmax(A,B)-B >= 0))

  expect_true(all(pmin(A,B)-A <= 0))
  expect_true(all(pmin(A,B)-B <= 0))

  expect_true(length(A+B) <= length(A) + length(B))
  expect_true(length(A-B) <= length(A) + length(B))

  x <- as.namedvector(A)
  y <- as.namedvector(B)

  expect_true(frab(c(x,y)) == frab(x) + frab(y))


}   # checker2() closes

checker3 <- function(A,B,C){
  expect_true(A+(B+C) == (A+B)+C)  # addition is associative; 1.2

  expect_true(pmax(A,pmax(B,C)) == pmax(pmax(A,pmax(B,C))))
  expect_true(pmin(A,pmin(B,C)) == pmin(pmin(A,pmin(B,C))))

  expect_true(pmax(A,B,C) == pmax(pmax(A,pmax(B,C))))
  expect_true(pmin(A,B,C) == pmin(pmin(A,pmin(B,C))))

}  # checker3() closes
  

for(i in seq_len(10)){
  A <- rfrab()
  B <- rfrab()
  C <- rfrab()
  
  checker1(A)
  checker1(zero())
  checker2(A,B)
  checker2(A,zero())
  checker3(A,B,C)
  checker3(A,B,zero())
}

checker2(frab(c(a=1,b=2,c=3)),frab(c(c=6,x=6,z=9)))
checker2(frab(c(a=1,b=2,c=3)),frab(c(c=6,x=6,c=-3)))
checker3(frab(c(a=1)),frab(c(a=-1,b=4)),frab(c(b=-4,c=3)))


} )
