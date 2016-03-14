## Put comments here that give an overall description of what your
## functions do

## This function takes the matrix as an input and it contains functions that can be called 
## to set the inverse, get the inverse, set matrix and get matrix
## Using this function we can store the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix. The functions first checks the cache,
## if cache has the inverse then the function does not caluculate the cace, it simply
## fetches it. However, if cache is empty, then it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting inverse matrix from cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
