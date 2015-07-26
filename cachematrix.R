## makeCacheMatrix function creates a special "matrix" and
## cacheSolve function calculates the inverse of created matrix
## for example:
## > matrix <- makeCacheMatrix(matrix(c(1,6,7,5), nrow=2, ncol=2))
## > cacheSolve(matrix)
## will give the following result:
##            [,1]        [,2]
## [1,] -0.1351351  0.18918919
## [2,]  0.1621622 -0.02702703

## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse matrix
## 4. get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmatrix <<- inverse
        getinverse <- function() invmatrix
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of matrix created by function makeCacheMatrix.
## First it checks to see if inverse matrix has already been calculated. 
## If so - gets inverse matrix from the cache and skips the computation.
## Otherwise it calculates inverse of the matrix and sets the value of inverse matrix in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)){
                message("getting cached data")
                return (invmatrix)
        }
        matrix <- x$get()
        invmatrix <- solve(matrix)
        x$setinverse(invmatrix)
        invmatrix ## Matrix that is the inverse of 'x' is returned
}
