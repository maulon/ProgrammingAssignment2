## R Programming
## Programming Assignment 2: Lexical Scoping
## Cache what may be a time consuming calculation
## example console useage
## > m <- matrix(nrow=2,data=c(4,2,7,6))    # matrix to test with
## > cm <- makeCacheMatrix(m)               # cached matrix
## > cacheSolve(cm)                         # calculate and show the result
## > cacheSolve(cm)                         # prints "getting cached data"
                                            # and the cached result

## Create a vector that contains NULL when the calculation hasn't been done

makeCacheMatrix <- function(x = numeric()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Use the vector to cache the result of calculating the inverse of a matrix

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
