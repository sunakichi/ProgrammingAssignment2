##intialize the matrix x as a function arguement##
##initialise INV to be used later##
##incase a new matrix is defined cache the value of y into x such that this new value is stored in the global environemt##
## Also re-initialise inv to NUll and cache that so that it can overwrite/reset the inv value previously calculated on prior vector##
##define the function 'get' to get the cached value of x from global environment##
##function to cache the inverse into the object inv##
#define the function 'getinverse' to get the cached value of inv##
#define each of the function above as element/object within a list and make them available in the global environemnt( just as cached value).  
##This enables the object to be used later by cachesolve function below##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x

        setInverse <- function(inverse) inv <<- inverse

        getInverse <- function() inv

        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

  ## gets inverse of 'x'if it is already cached##
##If the inverse of matrix is not available in the global environemnt as a cached value from before, x$get gets the new matrix the solve function calculates the inverse of matrix x####

cacheSolve <- function(x, ...) {
      
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
    mat <- x$get()

        inv <- solve(mat, ...)
        x$setInverse(inv)B1
        inv
}








