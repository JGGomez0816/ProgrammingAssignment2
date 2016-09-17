
##CACHING THE INVERSE OF A MATRIX
##The cachematrix.R contains two functions, makeCacheMatrix() and cacheSolve().
##These functions cache the inverse of a matrix.

## makeCacheMatrix()
##The first function, makeCacheMatrix(), creates an R object that stores 
##a matix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    A.inv <- NULL
    set <- function(y) {
        x <<- y
        A.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) A.inv <<- inverse
    getinverse <- function() A.inv
    list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## cacheSolve
##The second function, cacheSolve(), requires an argument that is returned 
##by makeCacheMatrix() in order to retrieve the matrix inverse from the cached 
##value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
    A.inv <- x$getinverse()
    if(!is.null(A.inv)) {
        message("getting cached data")
        return(A.inv)
    }
    data <- x$get()
    A.inv <- solve(data)
    x$setinverse(A.inv)
    A.inv
}


##Example:

> a <- makeCacheMatrix(matrix(c(1,7,8,9),nrow = 2))

##now call the cacheSolve twice to show retrieval from the cache

> cacheSolve(a) 
           [,1]       [,2]
[1,] -0.1914894  0.1702128
[2,]  0.1489362 -0.0212766
> cacheSolve(a) 
getting cached data
           [,1]       [,2]
[1,] -0.1914894  0.1702128
[2,]  0.1489362 -0.0212766


