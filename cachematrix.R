## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	inverseMat <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverseMat <<- inverse
	getinverse <- function() inverseMat
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMat <- x$getinverse()
	if(!is.null(inverseMat)) {
		message("getting cached data")
		return(inverseMat)
	}
	data <- x$get()
	inverseMat <- solve(data, ...)
	x$setinverse(inverseMat)
	inverseMat
}
