test_that("Test suite aac.R",{
    x_c <-new("sparsetable",
              index=structure(letters[c(1,1,1,2,2,2,3,3,3,3,3,1,2,3,1,2,3,1,1,2,2,3,3,2,3,2,3,2,1,3,1,2,3)],
                              dim=c(11,3),dimnames=list(NULL,c("Jan","Feb","Mar"))),
              values=c(8,6,16,21,15,9,11,3,15,3,13))

    x <- x_c
    expect_true(is.sparsetable(x))
    expect_true(x == x)
    expect_true(x+x == 2*x)
    expect_true(x+x == x*2)
    expect_true(x+x == sparsetable(index(x),values(x)*2))
    expect_true(is.empty(x-x))
    expect_true(all(dim(as.array(x-x))==0))
    expect_error(names(x))
    expect_true(nterms(x) == 11)
    expect_true(all(dim(x) == 3))
    expect_true(as.sparsetable(as.array(x)) == x)
    expect_output(print(x))
    expect_output(print(x*0))

    x['a','a','c'] <- 100
    expect_true(x == sparsetable(
                         matrix(letters[c(1,1,1,2,2,2,3,3,3,3,3,1,2,3,1,2,3,1,1,2,2,3,3,2,3,2,3,2,1,3,1,2,3)],11,3),
                         c(100,6,16,21,15,9,11,3,15,3,13))
                )


    x['a','a','c'] <- 0
    expect_true(
        x == sparsetable(
                 matrix(letters[c(
                     1, 1, 2, 2, 2, 3, 3, 3, 3, 3,
                     2, 3, 1, 2, 3, 1, 1, 2, 2, 3,
                     2, 3, 2, 3, 2, 1, 3, 1, 2, 3   )],10,3), c(
                     6,16,21,15, 9,11, 3,15, 3,13   ))
    )

    x <- x_c
    expect_true(as.frab(x) == 
                frab(c(a_a_c =  8, a_b_b =  6, a_c_c = 16, b_a_b = 21, 
                       b_b_c = 15, b_c_b =  9, c_a_a = 11, c_a_c =  3,
                       c_b_a = 15, c_b_b =  3, c_c_c = 13)))


    options("print_2dsparsetables_as_matrices" = TRUE)
    expect_output(print(sparsetable(
        matrix(c("a", "a", "b", "b", "b", "c", "c", "d", "e", "e", 
                 "e", "f", "A", "E", "B", "C", "D", "B", "D", "D", "A", "D", "E", 
                 "C"), 12, 2, dimnames = list(NULL, c("foo", "bar"))),
        1:12)
    ))

    options("print_2dsparsetables_as_matrices" = FALSE)
    expect_output(print(sparsetable(
        matrix(c("a", "a", "b", "b", "b", "c", "c", "d", "e", "e", 
                 "e", "f", "A", "E", "B", "C", "D", "B", "D", "D", "A", "D", "E", 
                 "C"), 12, 2, dimnames = list(NULL, c("foo", "bar"))),
        1:12)
    ))


    options("print_2dsparsetables_as_matrices" = NULL)

    I <- index(x)
    colnames(I) <- NULL
    expect_output(print(sparsetable(I,1:11)))

    expect_output(print(sparsetable(matrix(letters[1:12],4,3),1)))
    expect_true(as.sparsetable(frab(c(a=1,b=2,c=3))) == sparsetable(cbind(letters[1:3]),1:3))

    expect_true(as.sparsetable(table(data.frame(fish=letters[1:4],chips=LETTERS[4:1]))) == sparsetable(matrix(c("a","D","b","C","c","B","d","A"),byrow=TRUE,ncol=2)))

    expect_error(x*(1:3))
    expect_error((1:3)*x)

    expect_false(x == x*2)

    expect_false(x == rspar2())

    x[c('x','y','a')] <- 99
    expect_false(x == x_c)

} )
