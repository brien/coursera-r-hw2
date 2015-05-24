## Put comments here that give an overall description of what your
## functions do

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
