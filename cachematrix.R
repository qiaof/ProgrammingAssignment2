## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##s a function to set the vector, x, to a new vector, y, and resets the solve, m, to NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## returns the vector, x
    get <- function() x
    
    ## sets the solve, s, to solve
    setsolve <- function(solve) s <<- solve
    
    ## returns the solve, s
    getsolve <- function() s
    
    ## returns the 'special vector' containing all of the functions just defined
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    

}


## This function computes the inverse of the matrix returned by the "makeCacheMatrix" function. 
## If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      
    ## Return a matrix that is the inverse of 'x' 
    s <- x$getsolve()
    
    ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## getting the matrix from our object
    data <- x$get()
    
    ## calculating the inverse by using matrix multiplication
    s <- solve(data, ...)
    
    ## storing the inverse to the object to future usage
    x$setsolve(s)
    
    ## returning a matrix that is the inverse of 'x'
    s
}
