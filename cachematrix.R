
##
##Programming Assignment2

## the two functions makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix
## caching the inverse of a matrix rather than compute it repeatedly can be very handy 

##
##makeCacheMatrix

## is a function that creates a special "matrix" object that
## can cache its inverse
## the matrix consist of a list of functions
## these are commented in the function

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL                   
  set <- function(y) {                                #sets the matrix
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x                                 #returns the matrix
  setinverse <- function(solve) x_inv <<- solve       #sets the inverse
  getinverse <- function() x_inv                      #returns the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##
##cachesolve

## is a function that computes the inverse of
## the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  x_inv <- x$getinverse()                       #query x_inv the x matrix's cache 
  if(!is.null(x_inv)) {                         #if there is a cache
    message("getting cached data")
    return(x_inv)                                 #return the cache already computated
  }   
  
  data <- x$get()
  x_inv <- solve(data, ...)                      #compute the cache
  
  x$setinverse(x_inv)                            #save the result back to x's cache
  
  x_inv                                          #return the inverse
}


##testing if the codes actually work
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
##the matrix defined

class(a$get)
##function

a$getinverse()
##NULL

class(a$getinverse)
##function

cacheSolve(a)
cacheSolve(a)
##getting cached data should be written the 2nd time
a$getinverse()
##same inverse