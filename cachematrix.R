# This function puts a matrix into a cache variable.
# It can perform the solve() function on the matrix and if asked
# to solve again on the same matrix, it will retrieve the already
# solved-for solution instead of recomputing.

makeCacheMatrix <- function(x = matrix()) {
    cacheinv <- NULL
    set <- function(y) {
        x <<- y
        cacheinv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) cacheinv <<- inverse
    getinv <- function() cacheinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# This function solves for the inverse of a stored (cached) matrix

cachesolve <- function(x, ...) {
    functioninv <- x$getinv()
    if(!is.null(functioninv)) {
        message("getting cached data")
        return(functioninv)
    }
    data <- x$get()
    functioninv <- solve(data, ...)
    x$setinv(functioninv)
    functioninv
}