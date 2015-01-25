## Below are two functions that are used to create a special object that stores a matrix 
## and cache its inversion

## makeCacheMatrix return a list of following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inversion matrix 
## 4. get the inversion matrix

makeCacheMatrix <- function(x = matrix()) {
  inversion.matrix <- NULL
  set <- function(new.x) {
    x <<- new.x
    inversion.matrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  set.inversion <- function(new.inversion.matrix) {
    inversion.matrix <<- new.inversion.matrix
  }
  
  get.inversion <- function() {
    inversion.matrix
  }
  
  list(set = set, get = get, set.inversion = set.inversion, get.inversion = get.inversion)
}


## cacheSolve calculate the inversion matrix of the speical input "matrix" x created 
## with the above function. However, it first checks if the inversion matrix is already 
## cached. If so, it gets the value from the cache and return it,
## Otherwise, it calculates and store the inversion matrix into the cache.

cacheSolve <- function(x, ...) {
  inversion.matrix <- x$get.inversion()
  if (is.null(inversion.matrix)) {
    message('Calculate inversion for the first time.')
    data.matrix <- x$get()
    inversion.matrix <- solve(data.matrix)
    x$set.inversion(inversion.matrix)
  }
  inversion.matrix
}