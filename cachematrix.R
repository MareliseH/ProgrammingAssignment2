## Marelise's functions for Assignment 2 of R-programming. 

## Calculate a matrix inversee and storing the results, 
## so if the matrix is again inversed, computation time is saved by retrieving only the saved answer  
## for the specific matrix and not recalculating the same matrix. 


## makeCacheMatrix takes an matrix, calculates the inverse and then cache the inverse(or answer). 

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x 
  setInverse <- function(inverse) minv <<- inverse
  getInverse <- function() minv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## CacheSolve checks if the matrix's invese has been already calculated (cached) and if so, returns the answer 
## without calculating the inverse again. If it is a new matrix, the inverse is calculated and cach'ed'. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  minv <- x$getInverse()
  
  if(!is.null(minv)) {
    message("Getting cached data")
    return(minv)
  }
  
  cache <- x$get()
  minv <- solve(cache, ...)
  x$setInverse(minv)
  
  minv
  
}