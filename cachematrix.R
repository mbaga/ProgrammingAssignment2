## The following 2 functions cache a matrix and its inverse into a 
## special object (makeCacheMatrix) and retrieve it (cacheSolve)


## The following function creates the object comprising of the matrix 
## and its matrix

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function creates the inverse of the matrix from the 
## special object created from (makeCacheMatrix), but only after checking
## if the inverse isn't already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
