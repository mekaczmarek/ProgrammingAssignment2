## These functions store a matrix and compute its inverse, and store the inverse. 

## This function takes a matrix as an argument, which can be stored for use with the CacheSolve function. 
## It makes use of <<- to assign a new matrix and clear the potentially stored inverse matrix, i.
## The function then defines a set of functions which makes use of lexical scoping, because they are able to retrieve variables from the parent function like the matrix, x. 
## Finally, the list which assigns names to each of the functions allows for easy extraction, used in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)
}

## This function takes a makeCacheMatrix object, inverts the matrix and stores the inversion.
## First, it looks for a stored inverted matrix using the getinverse function. If it does not find one, it will look for the matrix in x, and invert it. 
## The last part "sets the inverse" so that it is printed and stored in the parent environment.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        data <-x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

