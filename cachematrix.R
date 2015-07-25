#The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse by:
#1. Setting the value of the matrix
#2. Getting the value of the matrix
#3. Setting the value of the inverse of the matrix
#4. Getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}