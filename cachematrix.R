## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #setting inverse to NULL as a position holder
        inv <- NULL
        #making matrix a global environment variable when setting it to y
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Getting the matrix data from cache
        get <- function() x
        #Setting setinverse variable to the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        #if the inverse was calculated earlier go to memory where it is stored as cached
        getInverse <- function() inv
        #define functions to their memory space
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #set inv to inverse of cachematrix
        inv <- x$getInverse()
        #if inv is null, do nothing if not null pull the inverse from allocated memory
        if (!is.null(inv)) {
                message("bringing cached data")
                return(inv)
        }
        #if not null, pull the matrix stored in x$get
        mat <- x$get()
        #solve for the inverse
        inv <- solve(mat, ...)
        #after solving, set inverse in cache memory
        x$setInverse(inv)
        #Return a matrix that is the inverse of x
        inv
}
}
