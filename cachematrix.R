## to calculate the Inverse of a Matrix I use de function solve().
## solve() performs the following operation a%*%x=b, true matrix product,
## and give us the coefficients matrix in our case the inverse of the matrix 'a'
## In order to do this operation I will implement two functions: makeCacheMatrix() and cacheSolve()
## The operation would be x%*%inv=id. 'x' is the matrix which i want to know the inverse
## inv is the inverse of 'x', and id is the identity matrix.


## In this first function I obtein a especial vector which is a list containing a function
## to:
## 1) set and get the value of the matrix 'x'
## 2) set and get the value of the inverse matrix
## 3) set and get the value of the identity matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    id<<-NULL
  }
  get<-function() x
  setinv<-function(calcinv) inv<<-calcinv
  getinv<-function() inv
  setid<-function(){
    row_id<-nrow(x)
    id<-diag(row_id) ## identity matrix
  }
  getid<-function() id
  list(set=set, get=get, setinv=setinv, getinv=getinv, setid=setid, getid=getid)
}


## Return a matrix that is the inverse of 'x' by means solve()

cacheSolve <- function(x, ...) {
  
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }        
  m_inv<-x$get()
  m_id<-x$getid()
  ## In the two lines above we get the values of the proble matrix and identity matrix
  inv<-solve(m_inv, m_id, ...)
  x$setinv(inv) ## stablish inv as the new inv in makeCacheMatrix
  inv
}

