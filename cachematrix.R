# The first function is designed to create a matrix and cache its inverse, and the second
# function retrieves the cached inverse or performs the actual inverse action on said matrix.

# The makeCacheMatrix function is specifically designed to create a matrix, and then proceeds
# to calculate the inverse using the solve() function. It stores this inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# The cacheSolve function will calculate the inverse of the specified matrix. However, prior
# to completing this action, it will determine if this inverse has already been caculated. If it
# has, it will retrieve the result from cache rather than calcuating it again. If it is not
# available, it will perform the inverse of the matrix and return that value.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

