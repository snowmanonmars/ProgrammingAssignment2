## makeCacheMatrix is a function that creates a list of closures, 
## functions made by a function.
## In specific, 1) it sets a matrix, 2) get the values of it,
## 3) sets an inverse of the matrix (by help of the function cacheSolve in this assignment)
## and 4) get the values of it. 
## By assigning the call of makeCacheMatrix to an symbol, 
## e.g. m1 <-makeCacheMatrix(matrix(1:4,2,2)), 
## you create a list of functions defined
## in the execution environment of makeCacheMatrix 
## with the execution environment fixed for further reference, 
## i.e. containing the variables such as x and inv.
## (while normally the execution environment should disappear after evaluation 
## of the function, here it becomes the enclosing environment of the four closures)
## Alternatively, you can start with m2 <- makeCacheMatrix(), which will make NULL x and inv,
## and use subsetting such as m2$setmatrix(matrix(5:8, 2, 2)) for settings.


makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    setmatrix <- function(y){
        x <<- y
        inv <<- NULL ## this statement prevents previous cached inverses from coming up after resetting the original matrix x
    }
    getmatrix <- function()x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinv = setinv, 
         getinv = getinv )
}

## By passing m1 to cacheSolve,
## the function cacheSolve accesses the functions in the list of m1 and use them 
## to check if a cached inv value exists in the reference environment, i.e. the enclosing environment of the above four closures, 
## and return the cached inv if it exists.
## Otherwise the following statments will calculate and assign a inverse to inv in the reference environment.
## Look the mentions inside the code

cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        ## Return a matrix that is the inverse of 'x' when it is cached in the
        ## referenece environment.
    }
    matrix <- x$getmatrix()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
        ## Calculate the inverse of the matrix, assign it as inv in the reference 
        ## environment, and show it.
}
