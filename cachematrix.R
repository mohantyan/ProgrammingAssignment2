# This function creates a matrix object that can cache its inverse.
# This function contains a list to do the following functions:
# 1. set(initialize) the matrix
# 2. get(retrieve) the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

# Input x to this function should be always square matrix (n*n)

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


# This function computes the inverse of a matrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then this function retrieves the inverse from the cache.

# Matrix supplied to this function is always a square matrix and invertible.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Data for Matric Inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}

# Usage:
# > source("cachematrix.R")
# > a <- c(1,2)
# > b <- c(3,4)
# > x <- rbind(a,b)
# > r <- makeCacheMatrix(x)
# > cacheSolve(r)
# a    b
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > cacheSolve(r)
# Getting Cached Data for Matric Inverse
# a    b
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > 