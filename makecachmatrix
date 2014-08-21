## This function make a matrix object and calculate the inverse if it's a 
## square matrix. The inverse is cached. 
## Write a short comment describing this function


makeCacheMatrix <- function(mat = matrix(numeric())){
  if((nrow(mat)!=ncol(mat))||(det(mat)==0)){
    print("This matrix is not inversible")
  }
  else{
    matInvers<-NULL
    set<- function(matInit){
      if((nrow(matInit)!=ncol(matInit))||(det(matInit)==0)){
        print("This matrix is not inversible")
      }
      else{
        mat<<- matInit
        matInvers<<- NULL      
      }
    }
    get<- function() mat
    invmat<- solve(mat)
    setInvers<- function(invmat){
      matInvers <<- invmat
    }
    getInvers<- function(){
      matInvers
    }
    list(set=set,get=get,setInvers=setInvers,getInvers=getInvers)
    
  }
 
}
## This function return the inverse of a matrix, before calculate
## the inverse it surch the inverse in the cache. If it's not in 
## the environnement the inverse is return
cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat'
  matInvers <- mat$getInvers()
  if(!is.null(matInvers)){
    print("getting cached data")
    return(matInvers)
  }
  data<- mat$get()
  matInvers<- solve(data)
  mat$setInvers(matInvers)
  matInvers
  
}
