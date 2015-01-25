## This program contains a pair of functions that facilitate creating a matrix object and solving for it
## while utilizing the caching mechanism in R.

## makeCacheMatrix - Takes a matrix as an argument and builds the supporting functions for caching

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve - Takes a first argument 'x' which is a cached matrix and solves it. It will use the cached value if available. The additional arguments
## are passed to the solve function if they are provided.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	# Check if a cached value exists
	if(!is.null(inverse)) {
		message("Returning cached data.")
		return(inverse)
	}
	# No cached value found, proceed with the calculation
	data <- x$get()
	inverse <- solve(data,...)
	x$setInverse(inverse)
	inverse
}
