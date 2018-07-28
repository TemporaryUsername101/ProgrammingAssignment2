## Written based on the example given in Week 3 for the R 
## programming course (https://www.coursera.org/learn/r-programming/)
##
## This snippet contains the makeCacheMatrix function and the cacheSolve function.
## The makeCacheMatrix function returns a list of functions 
## and stores the matrix in that list.
## The cacheSolve function solves this makeCacheMatrix (determines its inverse)
## and caches the result so it doesn't require calculation again.
##

## Creates a list of functions for the matrix 'x' that allows caching of its inverse.
## Note: it will reset the inverse (i) if the matrix is set again.
## - Input - The same as any accepted input for the R matrix() function
## - Output - A list of functions that allow saving the state of the cached solve result
##
## Functions defined:
## set - changes the matrix input of makeCacheMatrix
## get - returns the matrix input of makeCacheMatrix
## setinverse - stores the inverse of the matrix
## getinverse - returns the stored inverse of the matrix if available
makeCacheMatrix <- function(x = matrix()) {
  ## initialize i as NULL
  i <- NULL
  
  set <- function(y) {
    ## this sets the x and i outside of this function
    x <<- y
    i <<- NULL
  }
  
  ## this returns x stored in the makeCacheMatrix
  get <- function() x
  
  ## this sets i to inverse outside the setinverse function
  ## https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/assignOps
  ## the inverse variable is only available inside the setinverse environment
  setinverse <- function(inverse) i <<- inverse
  
  ## this again looks up i in the makeCacheMatrix environment
  getinverse <- function() i
  
  ## any functions added to the list here will be local to that list only ('=')
  ## in case you were wondering, that was meant to be an equal sign.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse matrix of a makeCacheMatrix 'x'.
## Note that it will cache the result after calculating it the first time.
## The next time the stored inverse is returned and a message displayed.
cacheSolve <- function(x, ...) {
  
  ## try to get the inverse
  i <- x$getinverse()
  
  ## if the calculation has not been done before i == NULL
  ## return the value if i != NULL
  if (!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  
  ## if no inverse calculation was done previously, do it now
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}