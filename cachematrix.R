## cachematrix stores a returns matrix and its inverse

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## a global scoped variable will keep a record of the matrix and it's inverse
	m <- x 
	i <- NULL
	set <- function(y) {
		##check to see if the matrix has changed
		if(!identical(m,y)) {m <<- y}
	}
	get <- function() {m}
	setInverse <- function(y) {
		## If the matrix is unchanged 
		## and the inverse has already been calculated
		## then retrieve the inverse from the makeCacheMatrix.
		if(!identical(m,y) || is.null(i)) {
			m <<- y
			i <<- solve(y)
		##} else {print("inverse already cached")} ##testing line
	}
	getInverse <- function() {i} 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
	#check the cached value for the inverse
	i <- x$getInverse()
	if(is.null(i)) {
		## Inverse is not cached, update the cached matrix and calc the inverse
		m <- x$get()
		x$setInverse(m)
		i <- x$getInverse()
	}
	## Return a matrix that is the inverse of 'x'
	i
}
