# Objective:
## a pair of functions that cache the inverse of a matrix
## these functions work together to create a cache 
## for matrix inversion calculations

## this function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # defines a function named makeCacheMatrix that 
  # takes one argument, x, with a default value of an empty matrix
  inv <- NULL # initializes a variable inv to hold the inverse of the matrix, it's initially 
  # set to NULL, indicating that the inverse is not computed yet
  set <- function(y) { # defines a nested function set within the makeCacheMatrix function
    # this function allows you to set the value of the matrix x and resets the cached inverse inv to NULL
    # the <<- operator is used to assign values in the parent environment (a form of assignment that works 
    # within a function)
    x <<- y
    inv <<- NULL
  }
  get <- function() x # defines another nested function get within the makeCacheMatrix function
  # this function simply returns the matrix x
  setinverse <- function(inverse) inv <<- inverse # defines a nested function setinverse within the 
  # makeCacheMatrix function, this function is used to set the cached inverse value, it takes an argument 
  # inverse, which is the computed inverse, and assigns it to the inv variable using the <<- operator
  getinverse <- function() inv # defines a nested function getinverse within the makeCacheMatrix 
  # function, this function returns the cached inverse value
  list(set = set, get = get, # returns a list containing the four nested functions: set, get, 
       # setinverse, and getinverse, this effectively creates a closure where the inner functions 
       # have access to the variables defined in the outer function
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.  If the inverse has already
## been calculated (and has not changed), then the
## cacheSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) { # defines a function named cacheSolve that takes an argument x, 
  # which is expected to be an object returned by the makeCacheMatrix function, the ... indicates that 
  # additional arguments can be passed to the function (these are typically passed to the solve function)
  inv <- x$getinverse() # retrieves the cached inverse inv from the object x using the getinverse function
  # if the inverse is not NULL, it means that the inverse has been computed before and cached, in this case, 
  # a message is printed indicating that cached data is being used, and the cached inverse is immediately returned
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # if the cached inverse is NULL, it means that the inverse hasn't been computed yet, the matrix data 
  # is retrieved from the object x using the get function, then, the matrix is inverted using the 
  # solve function, with additional arguments passed through .... the computed inverse is stored in the inv 
  # variable and also cached in the object x using the setinverse function, 
  # finally, the computed inverse is returned
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Summary:
## this pair of functions allows you to create a matrix object using makeCacheMatrix, cache its inverse, 
## and then retrieve the inverse using cacheSolve, if the inverse has been computed before, cacheSolve will 
## use the cached value instead of recalculating it

# Use:
# example of how you can use the makeCacheMatrix and cacheSolve functions to create a matrix object that 
# can cache its inverse

# create a matrix
A <- matrix(c(2, 1, 1, 3), nrow = 2, ncol = 2)

# create a cache matrix object using makeCacheMatrix
cache_A <- makeCacheMatrix(A)

# calculate the inverse using cacheSolve
inverse_A <- cacheSolve(cache_A)

# display the calculated inverse
print("Inverse of A:")
print(inverse_A)

# retrieve the inverse again from the cache
cached_inverse_A <- cacheSolve(cache_A)

# the second time, the cached inverse will be used
print("Cached Inverse of A:")
print(cached_inverse_A)
