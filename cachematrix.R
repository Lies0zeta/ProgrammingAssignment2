## these functions allow to cache the inverse of a matrix



## This function creates a special "matrix" object containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setSolve <- function(solve) m <<- solve
            getSolve <- function() m
            list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function calculates the matrix inverse of the special "matrix"
## created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getSolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setSolve(m)
            m
}
