
## First of all, we will set the cache data of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        sample_matrix <- NULL
        set <- function(y) {
                # y will be the initial matrix from me. And it will be stored in global_x.
                global_x <<- y
                # set global_m for NULL.
                global_m <<- NULL
        }
        # get function will return a matrix stored by set().
        get <- function() return(global_x)
        
        # a matrix is stored as global value matrix.
        set_global_m <- function(sample_matrix) global_m <<- sample_matrix
        
        # And a matrix stored by set_global_m() will returned
        get_global_m <- function() return(global_m)   
        list(set = set, get = get,
             set_global_m = set_global_m,
             get_global_m = get_global_m)
}



## And from here, it will compute the inverse of matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sample_matrix <- x$get_global_m()
        if(!is.null(sample_matrix)) { 
                message("getting cached data")
                return(sample_matrix)
        }
        # if sample_matrix is NULL, the inverse of matrix is computed by solve() function.
        data <- x$get()               
        inverse_matrix <- solve(data)   
        x$set_global_m(inverse_matrix)             
        return(inverse_matrix)                            
}

