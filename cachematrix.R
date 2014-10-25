## Return a object (so a list) that holds a matrix (x), default is am empty matrix
# and that has a set function "set" that stores a new matrix
# and that has a get function "get" that returns the stored matrix
# and that has a set function "setInverse" that stores a given mean
# and that has a get function "getInverse" that returns the stored mean
#
# The object resets the value given by "getmean" to "NULL" everytime "set" is called
makeCacheMatrix <- function(x = matrix()) {
  # create a variable ot store the inverted matrix
  i <- NULL
  
  # A function to set a new matrix
  set <- function(y) {
    x <<- y
    
    #Clear the stored value of m (from setmean)
    i <<- NULL
  }
  
  # a function to return teh stored matrix
  get <- function() x
  
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  
  #Return object with functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse matrix of x, x must be a object created by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  # Check if there is a stored inverse matrix
  if(!is.null(i)) {
    message("getting cached data")
    # Since it's cached, just return the cashed version
    return(i)
  }
  
  # There wasn't a stored mean so calulate it and store it
  
  # calculate the inverse matrix
  i <- solve(x$get(), ...)
  
  # store the inverse matrix in the object
  x$setInverse(i)
  
  # return the calculated inverse matrix
  i
}
