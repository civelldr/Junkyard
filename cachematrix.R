## R Programming 007, Assignment #2

## The following pair of functions illustrate the benefit of caching computationally expensive procedures.  
## The first function 'makeCacheMatrix(x=a matrix)' creates a special matrix list which stores the inverse 
## of itself. The next function 'cacheSolve(x = a cacheMatrix)' can retrieve this value if it's been cached,
## otherwise it will calculate the inverse and store  result.
##
## civelldr 9/20/14
##
####################################################

# creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special matrix list returned by makeCacheMatrix function 
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("fetching cashed inversion")
    return(inverse)
  } 
  message("calculating inversion")
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  return(inverse)
}

SmakeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(y){
    M <<- y
    I <<- NULL 
  }
  get <- function() M
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve computes the inverse of a given 'cacheMatrix' cM created by the function
# makeCacheMatrix(). It returns the inverse Matrix of M, where cM=makeCacheMatrix(M).

ScacheSolve <- function(cM, ...) {
  ## Return a matrix that is the inverse of cM=makeCacheMatrix(M)
  I <- cM$getinv()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- cM$get()
  I <- solve(data, ...)
  cM$setinv(I)
  I
}

## TESTING
m1 <- matrix(data = rnorm(n = 16, mean=5, sd = 3), nrow = 4, ncol=4)
z <- makeCacheMatrix(x=m1)
z$get()  ## returns matrix m1
z$getInverse()  ## is NULL
cacheSolve(z)  ## calculating inversion
cacheSolve(z)  ## fetching cached inversion
z$getInverse()  ## no longer NULL, returns its inverse

