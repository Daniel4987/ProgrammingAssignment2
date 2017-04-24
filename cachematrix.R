###This Function calculates the inverse of a matrix

## First we create an object that allows to calculate the inverse matrix
makeCacheMatrix <- function( l = matrix() ) {
  
  N <- NULL
  
  # Set of the matrix
  set <- function( matrix ) {
    l <<- matrix
    N <<- NULL
  }
  
  # Get the matrix
  get <- function() {
    l
  }
  
  ## Set the inverse of the matrix
  setinv <- function(inverse) {
    N <<- inverse
  }
  
  # Get the inverse of the matrix
  getinv <- function() {
    N
  }
  
  # Return a list of the methods
  list(getinv = getinv,
       setinv = setinv,
       set = set, get = get)
}



## Then the inverse of the matrix returned in the first part is calculated. 
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse.

cacheSolve <- function(x, ...) {
  
  # Return the matrix inverse of X
  l <- x$getinv()
  
  if( !is.null(l) ) {
    message("Getting calculated Data")
    return(l)
  }
  
  # Get the matrix from the object
  Data <- x$get()
  
  l <- solve(Data) %*% Data
  
  x$setinv(l)
  
  l
}

makeCacheMatrix

