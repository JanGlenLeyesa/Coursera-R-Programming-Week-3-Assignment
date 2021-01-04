## These functions helps compute the inversion in matrix

## The function makeCacheMatrix creates a cache for an inverse matrix 

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) {inv <<- inverse}
	getInverse <- function(){inv}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function cacheSolve returns the inverse matrix from the previous function

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}

