## The makeCacheMatrix function takes in a matrix as argument
## and returns a list with functions that can set and change 
## a matrix and its inverse.  The cacheSolve function takes in
## a 'matrix' list created by makeCacheMatrix, checks if the
## inverse of the cached matrix was calculated, calculates the
## inverse if it was not stored, stores it inside the 'matrix', and 
## returns the inverse. 

## Creates a list that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(newMatrix) {
		x <<- newMatrix
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse,
	     getInverse=getInverse)

}


## Calculates and stores the inverse of a cached matrix 
## if inverse was not already cached, and returns the inverse 

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("returning cached inverse")
		return(inv)
	}
	cacheMatrix <- x$get()
	cacheInverse <- solve(cacheMatrix)
	x$setInverse(cacheInverse)
	cacheInverse
}
