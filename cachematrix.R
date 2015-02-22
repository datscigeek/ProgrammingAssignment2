#
# This function takes 'x' as a parameter which has to be a matrix.
makeCacheMatrix <- function(x = matrix()) {
# reset 's' which is the solved matrix
		s <- NULL
# set the new matrix
		set <- function(y) {
				x <<- y
				s <<- NULL
		}
#
# get back the matrix
		get <- function() x
# set the solved matrix
		setsolve <- function(solve) s <<- solve
# get the stored version of the solved matrix
		getsolve <- function() s
# build an array of functions
		list(	set				=		set,
				get				=		get,
				setsolve		=		setsolve,
				getsolve		=		getsolve)

}
#
# return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
# fetch stored calculation
		s <- x$getsolve()
		if(!is.null(s)) {
		message("getting cached data")
# if there is one, returns it
		return(s)
		}
		data <- x$get()
# if not, calculate it
		s <- solve(data, ...)
# stores the result to cache
		x$setsolve(s)
# return result
		s

}