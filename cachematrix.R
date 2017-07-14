## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		## Initialize cache for the matrix as NULL
		i <- NULL
		## Set matrix contents with argument
		set <- function(y) {
			x <<- y
			i <<- NULL
		}
		## Get matrix contents
		get <- function() x
		## Set cache of the inverse
		setinv <- function(inv) i <<- inv
		## Get cache of the inverse
		getinv <- function() i
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
		## if there matrix x has the inverse cached, it is returned
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		## if no cache is found, the inverse is calculated
		data <- x$get()
		i <- solve(data, ...)
		## Once the inverse is computed, the cache for the matrix is set
		x$setinv(i)
		i
}
