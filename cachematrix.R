## makeCacheMatrix is a helper function that performs the following tasks:
## 1. Assign/re-assign the matrix and its inverse to a variable
## 2. Return the value of the matrix
## 3. Assign the the inverse of the matrix to a variable
## 4. Return the value of the matrix inverse

## cacheSolve is a a function that performs the following tasks:
## 1. Determines whether the inverse value of the matrix has been set
## 2. If the value has been set, return the cached value from memory
## 3. If the value has not been set, calculate the matrix inverse and set it to i.

## makeCacheMatrix is a helper function - see docblock above for details.

makeCacheMatrix <- function(x = matrix()) {
  # set the inverse variable to null 
  i <- NULL
  # set the value of both the matrix (for a new value y) and inverse (to NULL)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # return the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  # return the value of the inverse
  getinverse <- function() i
  # return a list of each function and functionality
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is a function for getting/setting the inverse of a matrix using the
## makecachematrix helper function

cacheSolve <- function(x, ...) {
  # retrieve the value of the getinverse function
  i <- x$getinverse()
  # check if the inverse exists (or is null). If not null, return the value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # if the inverse does not exist, retrieve the matrix from the helper function...
  data <- x$get()
  # and calculate the inverse
  i <- solve(data, ...)
  # set the value of the matrix inverse
  x$setinverse(i)
  i
}
