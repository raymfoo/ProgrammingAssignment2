##  Date Created :   2015-10-13
##  Craeted By   :   Raymond Foo
##
##  This R script contains 2 functions (makeCacheMatrix, cacheSolve). 
##  The two functions are used in tandem in order to cache the inverse
##  of a matrix. Once the inverse has been cached, it can be retrieved 
##  without incurring the compuatation cost of inverseing the 
##  original matrix everytime it is retrieved. 
##
##
##  Usage example:
##
##  source("cachematrix.R")
##  b <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
##  a <- makeCacheMatrix(b)
##  cacheSolve(a)
##  a$getInverse()



## Description  :   This function acepts a matrix and returns a list of
##                  fuctions that exposes the getter and setter for 
##                  both the original matrix and its inverse.
## Parameter    :   matrix - matrix to be inversed
## Return       :   List of getter and setters function for both
##                  the original matrix as well as its inverse
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
  
    set <- function(newMatrix){
        matrix <<- newMatrix
        
        ##Inverse is reset each time new matrix is populated
        inverse <<- NULL
    }
    
    get <- function() matrix
    
    setInverse <- function(newInverse) inverse <<- newInverse
    
    getInverse <- function() {
        inverse
    }
        
    ## list of setters and getters returned and exposed
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Description  :   Accepts the list of functions returned from makeCacheMatrix and 
##                  populates inverse of the original matrix if has not been populated 
##                  via the exposed list of functions from makeCacheMatrix
## Parameter    :   x - list of functions exposed by makeCacheMatrix
## Return       :   inverse of the stored matrix
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    ##return cached inversed if it is not null
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    ##else calculate the cache inverse
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    
    ##return calculated inverse after caching
    inverse
}