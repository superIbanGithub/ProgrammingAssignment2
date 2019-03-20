## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of
##functions that cache the inverse of a matrix.


## Write a short comment describing this function
##  function creating a "matrix" object that cache its inverse
## the function take the argument with default mode of "matrix"

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inv as NULL; will hold value of matrix inverse 
    inv <- NULL
    ## define the set function to assign new 
    ## take y as parameter inverse to x
    set <- function(y) {                    
        x <<- y                            
        inv <<- NULL                       
    }
    ## define the get function - returns value of the matrix argument
    get <- function() x                     
    ## assigns value of inv
    setinverse <- function(inverse) inv <<- inverse  
    ## gets the value of inv where called
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

