## This file contains functions that allow the calculated inverse of a
## matrix to be cached

## This Function creates an object that allows for the caching of
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse 
	getinverse <- function() inv
	set(x)
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## Use the caching abilities from the function above to store
## the calculated inverse of matrixes

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv	
}
