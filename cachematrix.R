## The following functions take a square matrix as input and output inverse of the matrix
## The inverse matrix is cached so that it doesn't have to be calculated everytime

## makeCacheMatrix caches a matrix in memory or returns the matrix that is cached

makeCacheMatrix <- function(x = matrix()) {
        x<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve function creates an inverse of the matrix and returns it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	mymatrix <- x$get()
	m <- solve(mymatrix)
	x$setmatrix(m)
	m
}
