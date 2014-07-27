## Both these functions provide caching facilities for matrix inversion operations.
## A simple run for both of these functions in action should look like
## > source("cachematrix.R")
## > cm = makeCacheMatrix(matrix(c(2,2,3,2),2,2))
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## >

## Implements simple caching functionality for a matrix

makeCacheMatrix <- function(invertible_matrix = matrix()) {
    solved_matrix <- NULL
    set <- function(input_matrix) {
        invertible_matrix <<- input_matrix
        solved_matrix <<- NULL
    }
    get <- function(){
        invertible_matrix
    }
    #set_solved_matrix <- function(input_solved_matrix) solved_matrix <<- input_solved_matrix
    #get_solved_matrix <- function() solved_matrix
    get_solved_matrix <- function(){
        solved_matrix
    }
    set_solved_matrix <- function(input_solved_matrix){
        solved_matrix <<- input_solved_matrix
    }
    list(set = set, get = get,
                 get_solved_matrix = get_solved_matrix,
                 set_solved_matrix = set_solved_matrix)
}

## Logic layer above makeCacheMatrix. Determines when to compute and when to serve from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solved_matrix <- x$get_solved_matrix()
    if(!is.null(solved_matrix)) {
            message("getting cached data")
            return(solved_matrix)
    }
    data <- x$get()
    solved_matrix <- solve(data, ...)
    x$set_solved_matrix(solved_matrix)
    solved_matrix
}
