## These two functions (makeCacheMatrix, cacheSolve) provide an example of how Lexical Scoping works in R.
## The makeCacheMatrix functions create a global input matrix object and calculates its inverse
## once and caches the result into a global output matrix.  If the input matrix changes, then the 
## global variables are reset and the inverse is calculated and cached.  If the input matrix doesn't change,
## its inverse if fetched from the cache (global output matrix)
##
## NOTE:  The global Variables are only avaible in the makeCacheMatrix's environment.
##
## makeCacheMatrix creates a list containing 4 functions:
##
## 1.   reset_mx_data:  resets the global input matrix and resets the inverse global variable.  (not used)
## 2.   get_mx_data:    Returns the input matrix that was passed to it.
## 3.   get_mx_inverse: Returns the inverse of the input matrix.
## 4.   set_mx_inverse: Caches the inverse global variable.
## 

makeCacheMatrix <- function(mx_data_in = matrix()) {
        
        ## Reset Local inverse of the matrix
        mx_inverse_out <- NULL   
        
     
        reset_mx_data <- function(y) {
                mx_data_in <<- y
                mx_inverse_out <<- NULL
        }

        get_mx_data   <- function() mx_data_in
        
        set_mx_inverse <- function(solve) mx_inverse_out <<- solve

        get_mx_inverse <- function() mx_inverse_out
        
        list(reset_mx_data=reset_mx_data,
             get_mx_data=get_mx_data,
             set_mx_inverse=set_mx_inverse,
             get_mx_inverse=get_mx_inverse)
}

## cacheSolve accepts a matrix and MakeCacheMatrix as an argument and returns it's inverse.
##
## This is how to execute this at command line:
##
##      1. Set up matrix example:  
##         - input_mx <- rbind(c(4,7), c(2,6))            
##      
##      2. Setup variable containing argment required argment for cacheSolve.
##         -  x1 <- makeCacheMatrix(input_mx)             
##
##      3.  The 1st time execution of MakeCacheMatrix, the inverse will be calculated because cache is NULL.
##         - cacheSolve(x1)                               
##
##      4.  Execute 2nd time with same input matrix, inverse is fetched from the cache.
##         -  cacheSolve(x1)

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        mx_inverse_out<-x$get_mx_inverse()
        
        if(!is.null(mx_inverse_out)){
                message("Getting Cached Data")
                return(mx_inverse_out)
        }
        matrix_in <- x$get_mx_data()
        mx_inverse_out <- solve(matrix_in)
        x$set_mx_inverse(mx_inverse_out)
        return(mx_inverse_out)
}
