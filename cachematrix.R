## Set of functions for matrixes with stored cache

## Function to create special object "matrix"
## This object also can contain the inverse matrix and two functions to work with it
makeCacheMatrix <- function(x = matrix()) {
	inv <- matrix()
	set <- function(y) {
		x <<- y
		inv<<- matrix()
	}
	get <- function(y) {
		x
	}
	setInv <- function(i) {
		inv <<- i
	}
	getInv <- function() {
		inv
	}
	## Return the final list of functions
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## First of all we check cache
## If it's empty we calculate inverse matrix and cache it

cacheSolve <- function(x, ...) {
        ## Cache check
	inv <- x$getInv()
	if (!all(is.na(inv))){
		message("Getting inverse matrix from cache")
		return(inv)
	}
	## Inverse operation
	mat <- x$get()
	inv <- solve(mat)
	x$setInv(inv)
	inv
}
