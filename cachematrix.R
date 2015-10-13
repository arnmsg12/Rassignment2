## Below are two functions that are used to create 
## a special object that stores a matrix and cache's its inverse
## assuming that the inverse matrix exists

## -1- the following function makeCacheMatrix creates a "special"
## matrix which is a list of the 4 following functions:
## 'set' used to set the value of the matrix
## 'get' used to get the value of the matrix
## 'setInverse' used to set the value of the matrix inverse
## 'getInverse' used to get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inverseM <<- inverseMatrix
        getInverse <- function() inverseM
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## -2- The following function calculates the inverse matrix
## of the cacheMatrix created with the above function.
## However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse
## matrix value from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the source
## matrix and sets the value of the inverse in the cache via 
## the setInverse function.
cacheSolve <- function(x, ...) {
        inverseM <- x$getInverse()
        if(!is.null(inverseM)) {
                message("getting cached data")
                return(inverseM)
        }
        
        data <- x$get()
        
        # check that x underlying matrix is a square matrix
        if (!is.matrix(data) || nrow(data)!=ncol(data)) {
                stop('source data is not a square matrix')
        }
        
        # check that x underlying matrix is inversible
        if (det(data) == 0) {
                stop('matrix inverse does not exist')
        }
        inverseM <- solve(data, ...)
        x$setInverse(inverseM)
        ## Return a matrix that is the inverse of 'x'
        inverseM
}
