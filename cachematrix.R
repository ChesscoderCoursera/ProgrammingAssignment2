# Programming Assignment 2 - Lexical Scoping

# The following functions implements a special kind
# of matrix that has the ability to store its inverse,
# avoiding to calculate it repeatedly when the data
# of the matrix has not changed

# makeCacheMatrix declares the specification of a data
# type that has methods to get and set a given matrix
# and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # variable to store the inverse
    inv <- NULL
    # function to set the matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # function to get the matrix
    get <- function() x
    # function to set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    # function to get the inverse of the matrix
    getinverse <- function() inv
    # return the methods available for this data type
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

# cacheSolve is a function that receives an object
# of the previous type and returns its inverse from the
# cache if present or computing it if not
cacheSolve <- function(x, ...) {
    # getting the inverse from the cache
    inv <- x$getinverse()
    # returning the inverse if present in the cache
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # getting the matrix to calculate its inverse
    data <- x$get()
    # calculating the inverse if not present in the cache
    inv <- solve(data)
    # setting the calculated inverse to the cache
    x$setinverse(inv)
    # returning the inverse from the previuos calculation
    inv
}