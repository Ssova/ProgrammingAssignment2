## These functions apply the lexical scoping of R to the use of a cache
## for potentially time-consuming calculations on infrequently changing input.
## Given a matrix, its inverse is stored in a cache which can be accessed
## without re-calculating. Subsequent input changes reset the cache to NULL 
## which triggers a recalculation of the inverse.

## The first function, makeMatrix, creates a list of four functions which:
## 1. set the values of the matrix "mat". 
##    ("new_matrix" values can be added here in subsequent iterations.)
## 2. get the values of the matrix "mat" as input
## 3. set the values of the inverted matrix in the cache with a superassignment operator
## 4. get the values of the inverted matrix from the cache.
## Every time makeMatrix is called, it resets the cache to NULL

makeMatrix <- function(mat = matrix()) {
        inv_mat <- NULL
        set <- function(new_matrix) {
                mat <<- new_matrix
                inv_mat <<- NULL
        }
        get <- function() mat
        setinverse <- function(inverted) inv_mat <<- inverted
        getinverse <- function() inv_mat
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function, cacheSolve, builds the values of the inverted matrix
## using the functions created in makeMatrix. 
## First, it checks if there is a matrix in the cache ("inv_mat"), using the getinverse() function. 
## If there is an existing inverted matrix in the cache, it is returned and
## the function skips the solve() calculation.
## Otherwise, it uses the get() function to get the input matrix ("mat");
## the solve() function to calculate a new inverted matrix; and 
## the setinverse() function to set this in the cache; and 
## outputs the inverted matrix.

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv_mat <- mat$getinverse()
        if(!is.null(inv_mat)) {
                message("getting inversed matrix")
                return(inv_mat)        
        }
        data <- mat$get()
        inv_mat <- solve(data, ...)
        mat$setinverse(inv_mat)
        inv_mat
}
