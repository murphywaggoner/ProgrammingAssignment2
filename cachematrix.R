## Together these two functions create a 'matrix' that caches it own inverse
## with the purpose of reducing computational overhead when working with
## large matrices

# makeCacheMatrix() creates a special "matrix", 
# which is really a list containing functions to
# set the value of the matrix (setmat)
# get the value of the matrix (getmat)
# set the value of the inverse of the matrix (setinv)
# get the value of the inverse of the matrix (getinv)

makeCacheMatrix <- function(x = numeric()) {
  
  
  inv <- NULL                # initialize the inverse to NULL in this environment
  
  setmat <- function(y) {    # deep assign the matrix to the input and 
    x <<- y                  # deep assign the inverse to NULL to clear it
    inv <<- NULL             # when a new matrix has been stored
  }
  
  getmat <- function() x     # return the matrix from the deep assignment
  
  setinv <- function(minv) inv <<- minv # deep assign the inverse to the input 
  # called from cacheSolve after
  # cacheSolve has calculated the inverse
  # the purpose of called setinv(minv)
  # is to do the deep assign in the right 
  # environment
  
  getinv <- function() inv    # return the inverse from the deep assignment
  
  
  list(setmat = setmat,    
       getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve() calculates the inverse of the special
# "matrix" created with makeCacheMatrix. 
# cacheSolve() first checks to see if the matrix has and 
# inverse and if that inverse has 
# already been calculated. If the matrix has already been cached, 
# it gets the inverse 
# from the cache and skips the computation. If the matrix
# is not invertible it returns NULL and a message. Otherwise, 
# it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  # 
  # 
  inv <- x$getinv()       # m is currently calculated inverse
  if(!is.null(inv)) 
  {     
    # if the inverse is not null, return it with message
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$getmat()    # otherwise get the matrix
  
  if(is.matrix(data) && 
     if(dim(data)[1] == dim(data)[2]) {det(data) != 0} else {FALSE}) 
  {
    # if it is invertible
    inv <- solve(data)    # calculate the inverse
    x$setinv(inv)         # set the inverse
    return(inv)           # return the inverse
  }
  else
  {
    # otherwise return NULL with a message
    message("matrix not invertible")
    return(NULL)
  }
  
  
}
