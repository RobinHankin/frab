test_that("Test suite aac.R",{
    x_c <-new("sparsetable",
              index=structure(letters[c(1,1,1,2,2,2,3,3,3,3,3,1,2,3,1,2,3,1,1,2,2,3,3,2,3,2,3,2,1,3,1,2,3)],
                              dim=c(11,3),dimnames=list(NULL,c("Jan","Feb","Mar"))),
              values=c(8,6,16,21,15,9,11,3,15,3,13))

    x <- x_c
    expect_true(is.sparsetable(x))
    expect_true(x == x)
    expect_true(x+x == sparsetable(index(x),values(x)*2))
    expect_true(is.empty(x-x))
    expect_true(all(dim(as.array(x-x))==0))
    expect_error(names(x))
    expect_true(nterms(x) == 11)
    expect_true(all(dim(x) == 3))
    expect_true(as.sparsetable(as.array(x)) == x)
    
})
