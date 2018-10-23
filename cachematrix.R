## The below functions will cache the inverse of a matrix to avoid recomputing it, if no changes were made to the original matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    set_minv <- function(solve) m_inv <<- solve
    get_minv <- function() m_inv
    list(set = set, get = get,
         set_minv = set_minv,
         get_minv = get_minv)
}

## cacheSolve: This function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$get_minv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$set_minv(m_inv)
    m_inv
}