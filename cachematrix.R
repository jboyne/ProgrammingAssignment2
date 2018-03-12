## The functions save a cached matrix  and when it is needed it use the version from cache instead of creating it again. 
## This process consumes a lot of memory and resources for large matrices and it is good to implement a mechanism like the one below. 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL    
        set <- function(y) {        
                x <<- y        
                inverseMatrix <<- NULL    
        }    
        get <- function() x    
        setinverse <- function(inverse) inverseMatrix <<- inverse    
        getinverse <- function() inverseMatrix    
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if the inverse has already been computed.
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {    
        inverseMatrix <- x$getinverse()    
        if(!is.null(inverseMatrix)) {        
                message("getting cached Inverse Matrix")        
                return(inverseMatrix)    
        }    
        data <- x$get()    
        inverseMatrix <- solve(data)    
        x$setinverse(inverseMatrix)    
        inverseMatrix
}

##Example:

##> t = matrix(c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1),4,4)
##> cachedmatrix = makeCacheMatrix(t)
##> cacheSolve(cachedmatrix)
##      [,1] [,2] [,3] [,4]
##[1,]  0.25  0.0    0    0
##[2,]  0.00 -1.0    1    0
##[3,]  0.00  0.5    0    0
##[4,] -0.25  0.0    0    1
##> cachedmatrix$get()
##     [,1] [,2] [,3] [,4]
##[1,]    4    0    0    0
##[2,]    0    0    2    0
##[3,]    0    1    2    0
##[4,]    1    0    0    1
##> cacheSolve(m)
##getting cached Inverse Matrix
##      [,1] [,2] [,3] [,4]
##[1,]  0.25  0.0    0    0
##[2,]  0.00 -1.0    1    0
##[3,]  0.00  0.5    0    0
##[4,] -0.25  0.0    0    1




