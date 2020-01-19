## 'makeCacheMatrix' creates two objects 'x' & 'inv' within the function environment and returns a list of four functions('set', 'get', 'setinverse','getinverse') as an 
#output. We have given the NULL value to "inv" object to make sure that whenever a new data is put up through the makeCacheMatrix functn, the previous 'inv'
#value is cleared and is not used for caching purpose.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  
  # the matrix_data used in the arguement of of makeCacheMatrix is assigned to the variable 'x' here.  This object is used to define 'get' function, which inturn is used 
  # by the cacheSolve functn to calculate the inverse of the required data. We've put the value of 'inv' variable to be NULL in the global variable again to make sure 
  #that whenever the matrix data is changed using "x$get(newmatrix_data)", then the object 'inv' is set to NULL and the previous value of  'inv' as set by cacheSolve
  #function is not used again for the new data.  
  
  set <- function(matrix_data) {
    x <<-matrix_data
    inv <<- NULL  
  }
  
  
  # here we have defined the 'get' function which will be used by the cacheSolve fn to extract the matrix data for inverse calculation. it is a fn with no formal variable
  # and returns 'x' (which contains the matrix data) as an output. 
  
  get <- function() x
  
  
  # this function is defined to be used by 'cacheSolve' fn to assign the value of calculated inverse to the "inv" object in the global environment so that it can be used 
  #later for caching purposes if the same data is passed to 'cacheSolve' fn. 
  
  setinverse <- function(inv_data){ 
    inv <<- inv_data
  }
  
  
  # here we have defined 'getinverse' function with no formal arguements to be used by 'cacheSolve' fn for caching purposes.
  
  getinverse <- function() inv
  
  
  #the final output of 'makeCachematrix' fn as list containing the four functions with each element named for subsetting purposes
  
  list(get=get,set=set, setinverse=setinverse, getinverse=getinverse)

}


## We have defined cacheSolve function to either cache the previously calculated inverse or to calculate the new inverse value.

cacheSolve <- function(z,...){
  inv <- z$getinverse()
  
  # if 'inv' is NULL, means it has not been calculated before for the given data, then we will extract the data using 'get' function, and the calculate the inverse and 
  #assign its value to 'inv' variable in the both the environment so that it can be used for caching purposes.
  
  if(is.null(inv)) {
    matrix <- z$get()
    inv <- solve(matrix)
    z$setinverse(inv)
    inv
  }
  
  
  # if 'inv' is not NULL, it means the inverse has been calculated before for the same data and we will just print the message and the already calculated inverse
  
  else {
    print("getting cached result")
    inv 
  }
}
