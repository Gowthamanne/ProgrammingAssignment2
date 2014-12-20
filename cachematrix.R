## Matrix inversion is usually a costly computation and 
## there may be some benefit to cache the inverse of a matrix 
## rather than compute it repeatedly. 
## The functions here create a wrapper around the Matrix and provide
## mecahnism to cache the inverse and save time in future inverse computations  


## creates a special "Matrix", which is really a list containing functions to
## Get/Set the value of the underlying Matrix
## Get/Set the inverse of the underlying Matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes the inverse of the special "Matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting from the cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
