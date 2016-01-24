## The following two functions are meant for calculating the inverse version of a matrix
## with caching the result to eventually save the computation time

## This function serves the purpose to enable us to store a matrix and eventually
## its inverse version (after having been calculated) and retrieve those values as needed.

makeCacheMatrix <- function(x = matrix()) {
      ## the cached value is initially set to NULL
      m <- NULL
      
      ## (re)set the value of the matrix - and clear its cached "inverse version"
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## return the value of the matrix
      get <- function() {
            x
      }
      
      ## set the cached "inverse version" of the matrix
      setInverse <- function(solve) {
            m <<- solve
      }
      
      ## return the cached "inverse version" of the matrix
      getInverse <- function() {
            m
      }
      
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the inverse of the original matrix,
## after having checked if it's been previously calculated (i.e., the cache variable is populated)
## - depending on this it will rerieve and return the cached version OR it will
## calculate the inverse matrix, store it in the cache and finally return it

cacheSolve <- function(x, ...) {
      ## first we attempt to get the inverse matrix from the chache
      m <- x$getInverse()
      
      ## if the inverse matrix has already been calculated (i.e., the m object is not NULL),
      ## we return it and do not proceed further with another unnecessary computation
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## if we're here, there is no previously computed inverse matrix in our "cache",
      ## so we get on with the computation - first we need to get the original matrix
      data <- x$get()
      
      ## now we calculate the inverse matrix from the original matrix
      m <- solve(data, ...)
      
      ## ...and store it in our "cache" for later reusal
      x$setInverse(m)
      
      ## Return a matrix that is the inverse of 'x'
      m
}
