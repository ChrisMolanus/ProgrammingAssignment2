## Return a object (so a list) that holds a matrix (x), default is am empty matrix
# and that has a set function "set" that stores a new matrix
# and that has a get function "get" that returns the stored matrix
# and that has a set function "setmean" that stores a given mean
# and that has a get function "getmean" that returns the stored mean
#
# The object resets the value given by "getmean" to "NULL" everytime "set" is called
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    
    #Clear the stored value of m (from setmean)
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Return the mean of x, x must be a object created by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  
  # Check if there is a stored mean
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # There wasn't a stored mean so calulate it and store it
  
  # get the matrix
  data <- x$get()
  
  # calculate the mean
  m <- mean(data, ...)
  
  # store the mean in the object
  x$setmean(m)
  
  # return the calculated mean
  m
}
