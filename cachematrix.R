## The following functions calculate the inverse of a invertible/nonsingular matrix OR retrieve the inverse of that matrix from the cache 

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. This contains a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of matrix
##4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set.matrix <- function (y) {
		x <<- y 
		m <<- NULL 
	}
	get.matrix <- function() x
	set.inverse <- function(solve) m <<- solve
	get.inverse <- function() m
	list(set.matrix = set.matrix,
		get.matrix = get.matrix,
		set.inverse = set.inverse,
		get.inverse = get.inverse)

}

## The following function calculates the inverse of the special "matrix", which is the input of cacheSolve, returned by the above makeCacheMatrix.
## But, it first checks to see if the inverse has already been calculated (while the matrix has not changed).
## If so, it retrieves the inverse from the cache and skips the calculation
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the  set.inverse function  

cacheSolve <- function(x, ...) {
	m <- x$get.inverse() 
        if(!is.null(m)) {
		message("getting cached data") 
                return(m) 
        }
        data <- x$get.matrix() 
        m <- solve(data, ...) 
        x$set.inverse(m)
        m 
       
}
