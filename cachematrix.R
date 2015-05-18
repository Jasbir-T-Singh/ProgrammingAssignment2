## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse

## This function, 'makeCacheMatrix' creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the values of the square matrix
## 2. get the values of the square matrix
## 3. set the values of the inverse of the square matrix
## 4. get the values of the inverse of the square matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvMatrix <- function(solve) inv <<- solve
        getinvMatrix <- function() inv
        list(set=set,get=get, setinvMatrix=setinvMatrix, getinvMatrix=getinvMatrix)
}


## The following function computes the inverse of the special "matrix",
## that was created in the previous function, 'makecachematrix'.
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinvMatrix()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvMatrix(inv)
        inv
}

