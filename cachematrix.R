## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(inputMtrx = matrix()) {
 
  invMtrx <- NULL
  
  setMtrx <- function(rstMtrx){
      
      inputMtrx <<- rstMtrx
      invMtrx <<- NULL
      
  }
  
  getMtrx <- function ()inputMtrx
  setInvMtrx <- function (inputInvMtrx) invMtrx <<- inputInvMtrx
  getInvMtrx <- function () invMtrx
  list(setMtrx=setMtrx, getMtrx=getMtrx,setInvMtrx=setInvMtrx,getInvMtrx=getInvMtrx   )
    
}
## Write a short comment describing this function

cacheSolve <- function(cacheMtrx, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invMatrix <- cacheMtrx$getInvMtrx()
    
    if (!is.null(invMatrix)){
        message("Getting Cached value")
        return(invMatrix)
        
    }
    invMatrix <- solve(cacheMtrx$getMtrx())
    cacheMtrx$setInvMtrx(invMatrix)
    invMatrix
    

}

# Test the cache functions
m_input <- cbind (1:2, 3:4)
m_input
cache_m_input <-makeCacheMatrix(m_input)
m_input_inverse <- cacheSolve(cache_m_input)
m_input_inverse
# second call will retrive from Cache
m_input_inverse <- cacheSolve(cache_m_input)
m_input_inverse

# End of Test cache functions

