# Assign a matrix to input variable 'x'
# Set the initial value of 'solved' as null
# Replace all occurrences of "mean" with "solve" throughout the code
makeCacheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)) {
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) solved <<- solve
  getSolve <- function() solved
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
#
# Similarly, replace "mean" with "solve" and "m" with "s"
cacheSolve <- function(x, ...) {
  solved <- x$getSolve()
  if (!is.null(solved)) {
    message("Getting the inverted matrix")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setSolve(solved)
  solved
}
