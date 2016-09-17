Homework 2
================

For the following questions, please save your R code in a file called `HW2_lastname_firstname.R` and then email it to <jeremy.d.oldfather@frb.gov> and cc to <william.ampeh@frb.gov>.

#### 1. Vectors

1.  Using the `letters` vector, spell out your name in a character vector.
2.  Find the sum of integers between 1 and 100, ie. 1:100.
3.  Find the sum of *even* integers between 1 and 100.
4.  `3! = 1 * 2 * 3 = 6`. Using `factorial()` and `as.integer()`, what is the largest factorial you can represent in R as an integer? (Hint: It is less than 20!.)
5.  What is the largest factorial you can represent as a double?

#### 2. Matrices

1.  Given the matrices `one` and `two` create a matrix of threes using each of the pointwise operators: `+`,`-`,`*`,`/`,`^`. Feel free to use more than one operator at a time, but don't reuse the examples below:

``` r
ones<-matrix(1,nrow=2,ncol=2)
twos<-matrix(2,nrow=2,ncol=2)
ones + twos
```

    ##      [,1] [,2]
    ## [1,]    3    3
    ## [2,]    3    3

``` r
3*twos / 2*ones
```

    ##      [,1] [,2]
    ## [1,]    3    3
    ## [2,]    3    3

1.  Try making a symmetric matrix (must be square and equal to its transpose) from an asymmetric one. For example:

``` r
A<-matrix(rnorm(9),nrow=3) # A is random. Definitely not symmetric.
B<-A+t(A)
B==t(B)                    # B is symmetric 
```

    ##      [,1] [,2] [,3]
    ## [1,] TRUE TRUE TRUE
    ## [2,] TRUE TRUE TRUE
    ## [3,] TRUE TRUE TRUE

1.  Create the following matrix anyway that you like other than by typing it in by hand. Try using pointwise operations and don't forget about `diag()`, `upper.tri()`, `lower.tri()`, and `t()`.

``` r
> M
      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
 [1,]    0    1    2    3    4    5    6    7    8     9    10
 [2,]    1    0    1    2    3    4    5    6    7     8     9
 [3,]    2    1    0    1    2    3    4    5    6     7     8
 [4,]    3    2    1    0    1    2    3    4    5     6     7
 [5,]    4    3    2    1    0    1    2    3    4     5     6
 [6,]    5    4    3    2    1    0    1    2    3     4     5
 [7,]    6    5    4    3    2    1    0    1    2     3     4
 [8,]    7    6    5    4    3    2    1    0    1     2     3
 [9,]    8    7    6    5    4    3    2    1    0     1     2
[10,]    9    8    7    6    5    4    3    2    1     0     1
[11,]   10    9    8    7    6    5    4    3    2     1     0
```

#### 3. Data Management

1.  Following the example with unemployement data from Lecture 2. Load the unemployment data and plot the unemployment rate over time. Feel free to reuse the code from the lecture notes.
2.  There is no total population variable included in the data. Figure out how to calculate it and then plot it over time as well.
3.  Which year had the lowest unemployment rate? (Hint: read about `which.min()`) What about the year with the highest unemployment rate?
