## These 2 functions are used to invert a square matrix and cache the result in memory.
## If the results are in memory then they are used otherwise the matrix is inverted and
## stored in memory

## This function makeCacheMatrix creates a list containing a function to -
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
#stores matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
#retrives matrix       
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
## builds list
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}

## This function checks to see the inverse is available in memory.  If it is it uses it.
## if not, it calculates the inverse and caches the result

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
## Use results if they are in memory
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## Else calculate the inverse and cache the results
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
