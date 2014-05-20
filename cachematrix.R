## This file contains TWO functions "makeCacheMatrix" 
## ... and ... "cacheSolve"
## "makeCacheMatrix" creates a special matrix object.

## "cacheSolve" first checks if matrix inverse exists
## ..... if (isNull) is True - it implies that it does not exist
## ........ therefore calculates the inverse of the matrix.
## ..... Returns inverse

If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
# - First one Creates a Matrix x that is stored in cache
# -- x <- matrix(rnorm(16), nrow = 4) // Create a matrix x
# --- then a "special matrix"
# ----- cx <- makeCacheMatrix(x) // Create our special matrix
# ----- cx$get() // Return the matrix

# makeCacheMatrix: returns a matrix.
# This functions first Sets the value of the matrix
# Then Gets the value of the matrix
# After which, it Sets the value of the inverse
# Finally, it Gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	# inv stores the cached inverse matrix
	inv <- NULL
	# Setter for the matrix
	set <- function(y) {
    x <<- y
    inv_x <<- NULL
	}

	# Getter for the matrix
	get <- function() x
	# Setter for the inverse
	setinv <- function(inverse) inv_x <<- inverse
	# Getter for the inverse
	getinv <- function() inv_x
	# Return the matrix with our newly defined functions
	list(set = set, get = get, setinv = setinv, 
       getinv = getinv)
}

## "cacheSolve" is a function that returns the inverse of a matrix A created with
## ... created by "makeCacheMatrix" function.
## If the cached-inverse is available, cacheSolve retrieves it ...
## Else it computes inverse and caches it
## Returns Inverse
cacheSolve <- function(x, ...) {
  ## Return/add to a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()     ## store value of element-x of matrix / create Null element-x for matrix
  if (is.null(inv_x)) {       ## cached inverse for x Does Not Exist
    inv_x <- solve(x$get())   ## calculate inverse
    x$setinverse(inv_x)       ## update matrix
  }
  return(inv_x)               ## return inverse of x
}
