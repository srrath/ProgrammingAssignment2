## Put comments here that give an overall description of what your
## A  pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(inputMtrx = matrix()) {
 
  invMtrx <- NULL
  # This inner funtion set the value of the matrix of the matrix object. Anytime a  inverse of a new matrix is required 
  # this setMtrx() will be called first
  setMtrx <- function(rstMtrx){
      
      inputMtrx <<- rstMtrx
      invMtrx <<- NULL
      
  }
  # get the matrix object
  getMtrx <- function ()inputMtrx
  
  # setting the vairable that holds the inverse of the matrix object passed to the functon 
  # or set through set method
  setInvMtrx <- function (inputInvMtrx) invMtrx <<- inputInvMtrx
  
  # returns the inverse of the matrix object passed or set through set function
  getInvMtrx <- function () invMtrx
  
  # the output of the function is a list of functions
  list(setMtrx=setMtrx, getMtrx=getMtrx,setInvMtrx=setInvMtrx,getInvMtrx=getInvMtrx   )
    
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## It takes the list object returned by function makeCacheMatrix()

cacheSolve <- function(cacheMtrx, ...) {
    ## Return a matrix that is the inverse of matrix object
    
    
    ## check for the inverse of the matrix by accessing the list element returned by  makeCacheMatrix(). if presents
    ## return it from cache and exit. Else compute by calling Solve() function and then cache it 
    ## before retuning it  as function output
    ##
    invMatrix <- cacheMtrx$getInvMtrx()
    
    ## if the 
    if (!is.null(invMatrix)){
        message("Getting Cached value")
        return(invMatrix)
        
    }
    
    invMatrix <- solve(cacheMtrx$getMtrx())
    cacheMtrx$setInvMtrx(invMatrix)
    
    invMatrix
    

}

# Test the cache functions####################################
m_input <- cbind (1:2, 3:4)
m_input
cache_m_input <-makeCacheMatrix(m_input)
m_input_inverse <- cacheSolve(cache_m_input)
m_input_inverse


# second call will retrive from Cache
m_input_inverse <- cacheSolve(cache_m_input)
m_input_inverse

#Lets get the inverse of another matrix object
m_input_nw <- cbind (5:6, 7:8)
cache_m_input$setMtrx(m_input_nw)
m_input_inverse <- cacheSolve(cache_m_input)
m_input_inverse

# End of Test cache functions##################################

