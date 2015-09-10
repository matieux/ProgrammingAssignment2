#
# Example:
#
# > A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), nrow = 3, ncol = 3)
# > m <- cacheMatrix(A)
# > cacheSolve(m)
# [,1]       [,2] [,3]
# [1,] -0.6666667 -0.6666667    1
# [2,] -1.3333333  3.6666667   -2
# [3,]  1.0000000 -2.0000000    1
# > cacheSolve(m)
# getting cached data
# [,1]       [,2] [,3]
# [1,] -0.6666667 -0.6666667    1
# [2,] -1.3333333  3.6666667   -2
# [3,]  1.0000000 -2.0000000    1

#
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
		# Returns the cache matrix
		get <- function() cache_matrix
		
		# Set the matrix inverse in cache
		# Extra: Also check that the input inverse matrix is correct
		setinverse <- function(y) {
			# 2DO: add check matrix * inverse returns Identity matrix
			row = nrow(y)
			col = ncol(y)
			identity_matrix = diag(1, row, col)
			if(identical(cache_matrix %*% y, identity_matrix)) {
				cache_inverse <<- y
			} else {
				message("Error: the given inverse matrix is not correct")
			}
		}
		getinverse <- function() cache_inverse
		list(set = set, 
			 get = get, 
			 setinverse = setinverse, 
			 getinverse = getinverse)
}

#
# cacheSolve(x): Takes as argument a matrix object created by makeCacheMatrix()
#				 Returns the inverse of the matrix stored in the cache object
#				 Skips the computation of the inverse if it's cached

cacheSolve <- function(x, ...) {
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
