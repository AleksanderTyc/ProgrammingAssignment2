## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  This function creates a list of 4 functions:
#  * set the value of matrix, stores source matrix as internal variable
#  * get the value of matrix
#  * set the value of solved matrix
#  * get the value of solved matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                          # represents solved matrix
        set <- function(y) {
                x <<- y                    # represents source matrix
                m <<- NULL                 # initially solved matrix is empty
        }
        get <- function() x
        setsolved <- function(solved) {
                m <<- solved }             # solved matrix is stored
        getsolved <- function() m          # solved matrix is returned
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


## Write a short comment describing this function
#  This function calculates solved matrix and stores result as cache.
#  At repeated invocation, solved matrix is retrieved from cache
#  rather than recomputed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#  Retrieve cached solved matrix.
        m <- x$getsolved()
	#  If it is null, then it has never been calculated.
        if(!is.null(m)) {
	#  If it is not null, then it may be retrieved from cache...
                message("getting cached solved matrix")
	#  ...and returned.
                return(m)
        }
	#  Obtain source matrix...
        data <- x$get()
	#  ...and calculate solved matrix.
        m <- solve(data, ...)
	#  Store calculated solved matrix in cache.
        x$setsolved(m)
	#  Return solved matrix.
        m
}

