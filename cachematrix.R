## This function creates a list containing functions to set/get value of the matrix, set/get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## Function to set values for the matrix and set inverse to null
	set <- function(y){ 
		x <<- y
		inv <<- NULL
	}
	## Function to get value of the matrix
	get <- function() x
	
	## Function to set value of the inverse
	setinv <- function(inverse) inv <<- inverse
	
	## Function to get value of the inverse
	getinv <- function() inv
	
	## Return a list of the four defined functions 
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of matrix. If already calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv() # Retrieve cached value of inverse
	
	if(!is.null(inv)){
		message('getting cached data')
		return(inv)
	}
	
	## Calculate and cache the inverse
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)        
  
	## Return the inverse
	inv
}
