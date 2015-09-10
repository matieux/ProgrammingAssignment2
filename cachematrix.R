## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix(x): Creates an object caching the matrix x and its inverse;
#					  If no argument, x is an empty matrix
#					  The inverse cache is not set by default

makeCacheMatrix <- function(cache_matrix = matrix()) {
		# Initialization of inverse matrix cache
		cache_inverse <- NULL
		# If another matrix is set, caches it and reset the inverse cache
		set <- function(y) {
			cache_matrix  <<- y
			cache_inverse <<- NULL
		}
		get <- function() cache_matrix
		setinverse <- function(y) {
			# 2DO: add check matrix * inverse returns Identity matrix
			cache_inverse <<- y
		}
		getinverse <- function() cache_inverse
		list(set = set, 
			 get = get, 
			 setinverse = setinverse, 
			 getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		} else {
			inv <- solve(x$get(), ...)
			x$setinverse(inv)
			return(inv)
		}
}
