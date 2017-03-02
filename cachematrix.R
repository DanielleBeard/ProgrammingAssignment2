## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets teh value of the inverse
## 4. gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  INVERSE <- NULL
  set <- function(y) {
    x <<- y
    INVERSE <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) INVERSE <<- solve
  getinverse <- function() INVERSE 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## calculates the inverse of the special "matrix" created with the above function
##first, checks to see if the inverse has already been calculated
## if so, gets the inverse from the cache and skips the calculation
## otherwise, it calculates the inverse of the data and sets the value of the mean 
## in the cache via the setinverse function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INVERSE <- x$getinverse()
  if(!is.null(INVERSE)) {
    message("getting cached data")
    return(INVERSE)
  }
  MATRIX <- x$get()
  INVERSE <- solve(MATRIX, ...)
  x$setinverse(INVERSE)
  INVERSE
}
