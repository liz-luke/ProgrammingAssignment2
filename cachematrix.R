##Objective is to write a pair of functions that cache the inverse of a matrix.
##Consists of two functions,makeCacheMatrix and CacheSolve
#To make the matrix
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){               #Setting the matrix
    x<<-y
    m<<-NULL                      #Giving null value to inverse(m is inverse here)
  }
  get<-function(){x}
  setinverse<-function(inverse){  #Setting inverse
    m<<-inverse
  }
  getinverse<-function(){m}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
#To compute inverse of a matrix
cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){              #Checking if inverse is null
    message("getting cached data")  
    return(m)
  }
  Matrix<-x$get()
  m<-solve(Matrix,...)         #Solving to find inverse
  x$setinverse(m)
  return(m)                    #Returns the inverse
}