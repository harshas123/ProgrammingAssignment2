## Put comments here that give an overall description of what your
## functions do
# This R script contains two functions:
# NOTE: For the purposes of this script matrix inversion will be referred to as 
#       solve as solve() function in R is used to compute the inverse of a 
#       matrix. 
# 1. makeCacheMatrix -- Creates a special kind of Matrix which can cache its inverse.
                        #Essentially its inverse is computed only when required 
#                        and the matrix has change 
# 2. cacheSolve -- Computes/Fetches the inverse of a matrix defined using
#                   makeCacheMatrix() function.

# AUTHOR: Harshavardhan Sundar
# Modified from the code given by Prof. Roger D Peng for the makeVector() and
# cachemean() function. Created as part of the assignment for Cousera Course
# on R Programming.

## Write a short comment describing this function
# This function provides the following 4 methods to interact with this matrix:
# 1. set -- To set the value of the matrix
# 2. get -- Get the current value of the matrix
# 3. setsolve -- Sets the matrix inverse to a value given by the user
# 4. getsolve -- Gets the current value of the inverse of the matrix

#Arguments:
# 1. x -- Any square invertible matrix.
#Returns:
# The special matrix which is essentially a list with the above 4 methods.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv<- NULL
        set <- function(y = matrix()) {
                x <<- as.matrix(y)
                mat_inv <<- NULL
        }
        get <- function() x
        setsolve <- function(mat_inv_val) mat_inv <<- as.matrix(mat_inv_val)
        getsolve <- function() mat_inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
# The following function is used to get the inverse of a matrix given the matrix defined
# using the makeCacheMatrix() function. This function first checks if the 
# inverse as already been computed (in turn checks if the value of the special
# matrix has been set). If it has been computed, then it just gets this value 
# from a cache, else it computes the inverse.
# Arguments to this function:
# 1. x -- A matrix created using makeCacheMatrix() function

# Returns:
# The inverse of x. Also makes this inverse available via the getsolve() method
# in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getsolve()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setsolve(mat_inv)
        mat_inv
}
