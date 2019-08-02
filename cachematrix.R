#### CasheMatrix by Anjana
> ##This function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
> #get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
> #can cache its own object. 
> 
> #<<- operator is used to assign a value to an object in an environment that is different 
> #from the current environment 
> makeCasheMatrix <- function(x=matrix()){
+ invMatrix <- function(x=matrix()){
+ invMatrix<-NULL
+ 
+ #Set the value of Matrix
+ 
+ setMatrix<- function(y){
+ x<<-y
+ invMatrix<<-NULL
+ }
+ 
+ # get the value of the Matrix
+ 
+ getMatrix <- function()x
+ # set the value of invertible Matrix
+ setInverse <- function(inverse)invMatrix<<-inverse
+ 
+ #get the value of invertible matrix
+ getInverse <- function() invMatrix
+ 
+ list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
+ }
+ 
+ casheSolve <- function(x,...){
+ #get the value of the invertible matrix from the makeCasheMatrix function
+ 
+ invMatrix <- x$getInverse()
+ 
+ if(!is.null(invMatrix)){  #if inverse Matrix is not NULL
+ message("Getting Cashed Invertible Matrix")## Display this message
+ return(invMatrix)  ####return the invertible matrix
+ }
+ 
+ MatrixData <-x$getMatrix()  #get the original Matrix Data
+ invMarix <- solve(MatrixData,...)## use solve function to inverse the matrix
+ x$setInverse(invMatrix)##set the invertible matrix
+ return(invMatrix)# return the invertible matrix
+ }


###############################################################

Test_Marix<- matrix(c(1,2,3,4)2,2)
Test_Matrix

CacheMatrix <- makeCacheMatrix(Test_Matrix)
+ CacheMatrix$getMatrix()
+ CacheMatrix$getInverse()
+ 
+ cacheSolve(CacheMatrix)
+ cacheSolve(CacheMatrix)
###########################################################################
Test_Matrix <- matrix(1:9,3,3)
Test_Matrix

CacheMatrix <- makeCacheMatrix(Test_Matrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

