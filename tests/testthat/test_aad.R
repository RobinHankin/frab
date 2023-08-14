##   Function checker1() has one argument, checker2() two, and
## checker3() has three.

test_that("Test suite aad.R",{

checker1 <- function(A){
  expect_true(A == +A)    
  expect_true(A == -(-A))
  expect_true(A+A == 2*A)
  expect_true(A+A == A*2)

  expect_true(is.empty(A-A))
  expect_true(A+A+A == 3*A)
  expect_true(A+A+A == A*3)

  expect_true(A/2 + A/2 == A)

  expect_error(A&A)

  expect_true(pmax(A)==A)
  expect_true(pmin(A)==A)

  expect_true(pmax(A,A) == A)
  expect_true(pmin(A,A) == A)

  expect_true(sum(values(asum(A,1))) == sum(values(A)))




}   # checker1() closes
  
checker2 <- function(A,B){
  expect_true(A+B == B+A)
  expect_true(2*A+B == A+A+B)
  expect_true(A+2*B == B+B+A)

  expect_true(pmax(A,B) == pmax(B,A))
  expect_true(pmin(A,B) == pmin(B,A))

  expect_true(all(pmax(A,B)-A >= 0))
  expect_true(all(pmax(A,B)-B >= 0))

  expect_true(all(pmin(A,B)-A <= 0))
  expect_true(all(pmin(A,B)-B <= 0))

}   # checker2() closes

checker3 <- function(A,B,C){
  expect_true(A+(B+C) == (A+B)+C)  # addition is associative; 1.2

  expect_true(pmax(A,pmax(B,C)) == pmax(pmax(A,pmax(B,C))))
  expect_true(pmin(A,pmin(B,C)) == pmin(pmin(A,pmin(B,C))))

  expect_true(pmax(A,B,C) == pmax(pmax(A,pmax(B,C))))
  expect_true(pmin(A,B,C) == pmin(pmin(A,pmin(B,C))))

}  # checker3() closes
  

for(i in seq_len(10)){
  A <- rspar()
  B <- rspar()
  C <- rspar()
  
  checker1(A)
  checker2(A,B)
  checker3(A,B,C)

  checker1(rsparr())
}



} )
