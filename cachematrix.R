## Pair of functions that cache the inverse of a matrix

## Following function creates special "matrix" that can cache its inverse
makeCachematrix <- function(x = matrix()) {
        
        ## set matrix in cache to null
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get matrix
        
        ## set inverse in cache
        setinverse <- function(inverse) m <<- inverse
        
        ## get inverse from cache
        getinverse <- function() m
        
        ## return list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Following funtion computes inverse of "matrix" created by makeCachematrix
## or retrieves inverse from cache if it has already been computed
cacheSolve <- function(x, ...) {

        ## retrieve a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## if m is not NULL, retrieve inverse from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # otherwise compute inverse of matrix
        data <- x$get()
        m <- solve(data, ...)
        
        # store inverse of matrix in cache
        x$setinverse(m)
        
        ## return inverse of matrix
        m
}

