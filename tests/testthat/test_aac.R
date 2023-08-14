test_that("Test suite aac.R",{
    x_c <- sparsetable(
        i = matrix(letters[c(
            1,1,1,2,2,2,3,3,3,3,3,
            1,2,3,1,2,3,1,1,2,2,3,
            3,2,3,2,3,2,1,3,1,2,3
               )], ncol=3,dimnames=list(NULL,c("Jan","Feb","Mar"))),
        v = c(8,6,16,21,15,9,11,3,15,3,13))
    
    x <- x_c
    expect_true(is.sparsetable(x))
    expect_true(x == x)
    expect_true(x+x == 2*x)
    expect_true(x+x == x*2)
    expect_true(x+x == sparsetable(index(x),values(x)*2))
    expect_true(is.empty(x-x))
    expect_true(all(dim(as.array(x-x))==0))
    expect_false(x == x*0)
    expect_false(x == 0*x)
    
    expect_error(names(x))
    expect_true(nterms(x) == 11)
    expect_true(all(dim(x) == 3))
    expect_true(as.sparsetable(as.array(x)) == x)
    expect_output(print(x))
    expect_output(print(x*0))
    expect_error(1/x)
    expect_error(x*x)
    expect_error(x^x)
    expect_error(x^6)
    expect_error(6^x)

    x['a','a','c'] <- 100
    expect_true(x == sparsetable(
                         matrix(letters[c(1,1,1,2,2,2,3,3,3,3,3,1,2,3,1,2,3,1,1,2,2,3,3,2,3,2,3,2,1,3,1,2,3)],11,3),
                         c(100,6,16,21,15,9,11,3,15,3,13))
                )

    expect_false(sparsetable_equality(
        M1 = matrix(letters[1:9],3,3),
        d1 = 1:3,
        M2 = index(x_c),
        d2 = c(100,6,16,21,15,9,11,3,15,3,13))
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

    x <- x_c
    x['x','y','a'] <- 99
    expect_false(x == x_c)
    expect_true(x['x','y','a'] == 99)

# xpc = 'x plus zero'
    xpz <-sparsetable(    # the 10,10,10 == j,j,j entry is 77-77==0
        i=matrix(letters[c(
            1,1,1,2,2,2,3,3,10,3,3,3,10,  
            1,2,3,1,2,3,1,1,10,2,2,3,10,
            3,2,3,2,3,2,1,3,10,1,2,3,10
        )], ncol=3, dimnames=list(NULL,c("Jan","Feb","Mar"))),
              v=c(8,6,16,21,15,9,11,3,77,15,3,13,-77))

    expect_true(x_c == xpz)



    jj <-sparsetable(   
        i=matrix(letters[c(
            1,1,1,
            1,2,3,
            3,9,3
        )], ncol=3, dimnames=list(NULL,c("Jan","Feb","Mar"))),
        v=c(77,78,79))

    xpz[] <- jj
    expect_true(xpz == sparsetable(
                           i = matrix(c(
                               "a", "a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "c",
                               "a", "b", "b", "c", "a", "b", "c", "a", "a", "b", "b", "c",
                               "c", "b", "i", "c", "b", "c", "b", "a", "c", "a", "b", "c"
                           ),12, 3),
                           v = c(77,  6, 78, 79, 21, 15,  9, 11,  3, 15,  3, 13))
                )

    x <- x_c
    x[index(x)[1:4,]] <- 0
    expect_false(x == x_c)
    expect_false(x_c == x)

    x['c','a','c'] <- 334
    expect_false(x == x_c)
    expect_false(x_c == x)

    xas <- asum(x_c,"Feb")

    xas_correct <- sparsetable(matrix(
        c("a", "a", "b", "b", "c", "c", "c",
          "b", "c", "b", "c", "a", "b", "c"
          ), 7,2, dimnames = list(NULL, c("Jan", "Mar"))), c(6, 24, 30, 15, 26, 3, 16))

    expect_true(xas == xas_correct)


})


