## The 2 functions here are for caching the inverse of a matrix, 
## which the computation is usually costly, so there is benefit of caching 
## the inverse of a matrix rather than computing it repeatedly

## makeCacheMatrix creates a special "matrix" object that can cache its inverse,
## which is really a list that contains function to 
## 1. set the matrix x
## 2. get the matrix x
## 3. set the inverse of the matrix x
## 4. get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    # the inverse of the matrix is set to NULL initially
    i <- NULL
    
    # the set function set the matrix x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # the get function get the matrix x
    get <- function() x
    
    # the setinverse function set the inverse of the matrix
    setinverse <- function(inv) i <<- inv
    
    # the getinverse function get the inverse of the matrix
    getinverse <- function() i
    
    matrix(c(set, get, setinverse, getinverse), 
           dimnames=list(c("set", "get", "setinverse", "getinverse"), 
                         c("function")))
}


## cacheSolve "calculates" the inverse of the special matrix 
## it first uses makeCacheMatrix's getinverse function to try to get 
## the inverse of the special matrix x,
## if the return is null, it calls solve() to calculate the inverse of the matrix,
##   then it caches the inverse of the special matrix x by using 
##   makeCacheMatrix's setinverse function
## if it gets the inverse of the matrix from makeCacheMatrix's getinverse function,
##   it prints out a message saying it is using the cached inverse matrix
## in both cases the inverse of the matrix (cached or solve) is then returned

cacheSolve <- function(x, ...) {
    # try to get the cached inverse matrix 
    i <- x[["getinverse","function"]]()

    if (is.null(i)) {
        # cached inverse matrix is null
        # get the special matrix
        data <- x[["get","function"]]()
        # calculate the inverse of the matrix
        i <- solve(data)
        # set the inverse of the matrix
        x[["setinverse","function"]](i)
    } else {
        message("getting cached inverse matrix")
    }
    # return the inverse of the matrix
    return(i)
}
