## This function creates a list containing functions to set/get value of the matrix, set/get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This funciton calculates the inverse of matrix. If already calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message('getting cached data')
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)        

	## Return a matrix that is the inverse of 'x'
	inv
}
