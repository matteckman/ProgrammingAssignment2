## THE FUNCTIONS BELOW CREATE A SPECIAL MATRIX (IT ALLOWS FOR CACHING OF THE INVERSE OF THE GIVEN MATRIX),
## AND ALSO ALLOW FOR DOING THE INVERSING OPERATION

## -----------------------------------------------------------------
## THE "makeCacheMatrix" FUNCTION SETS UP THE SPECIAL MATRIX (see above)
## -----------------------------------------------------------------
## NOTE: it can be confusing that x and y both seem to hold the incoming matrix that will be inversed, I 
## think that this structure allows for initializing the function with a matrix, and then using the "set"
## operation to update that matrix. It's the scoping that allows for this, as x is defined in one environment
## and y is defined in a separate environment (within the function); this means that x won't change 
## within the function's environment, unless <<- is used.

makeCacheMatrix <- function(x = matrix()) {
  ## i is the variable that holds the inverse of x
  i <- NULL
  
  ## "set" sets the matrix that will be 'inversed'
  ## y is the variable that holds that matrix (see above)
  set <- function(y) {
  
    ## if x already has a matrix in another environment, then y won't overwrite x (??)
    x <<- y
    
    ## if i has a value in another environment, it will be replaced in that environment, otherwise, 
    ## it only changes locally (??)
    i <<- NULL
  }
  
  ## returns the value of the current matrix (non-inversed)
  get <- function(){
    return(x)
  }
  
  ## sets the value of the inverse of the matrix in the list
  ## NOTE: 'i' and 'solveVar' have different scopes
  setinverse <- function(solveVar){ 
    i <<- solveVar
  }
  
  ## returns the value of the inversed matrix 
  getinverse <- function(){
    return(i)
  }
  
  ## as the last lie of the function, this is what gets returned to the user (??)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## -------------------------
## THE "cacheSolve" FUNCTION
## -------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', 'x' is the special matrix above
  
  ## Gets the inverse from the previous function
  ## NOTE: 'i' in this scope (this function's environment) is different then the 'i' in the function above
  i <- x$getinverse()

  ## if 'i' is NULL (i.e. is empty), then the inverse operation (solve) needs to be run
  ## NOTE: the "return" function here stops the rest of the function from completing, it essentially skips
  ## the inverse operation. The same could be accomplished by using 
  ## if(!is.null(i)) {
  ##  ...
  ## }else{
  ##  [do the lines below]
  ## }
  ## i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the original matrix (non-inversed) and put into the variable 'data'
  data <- x$get()
  
  ## do the actual inversing of the matrix, and set to the variable 'i'
  i <- solve(data, ...)
  
  ## save the inversed matrix to the list
  x$setinverse(i)
  
  ## equivalent of "return(i)"
  i
}


## QUESTIONS
## The 2nd function is pretty straightforward, but I don't get the 1st function. Is it too concise?
## Will "list" get returned to user in 1st function?