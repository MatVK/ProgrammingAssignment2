## These functions creates a special object that can store a matrix and that can compute its inverse. 

#The first function, makeCacheMatrix, is used to set and get the value of the matrix, and to set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) a <<- inverse
        getinverse <- function() a
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of the matrix from above. 

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if (!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
