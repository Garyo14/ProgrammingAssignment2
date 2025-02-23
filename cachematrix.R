## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: Membuat objek matriks khusus yang bisa menyimpan inversnya
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Menyimpan nilai invers
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache jika matriks berubah
  }
  get <- function() x  # Mengambil matriks
  setInverse <- function(inverse) inv <<- inverse  # Menyimpan invers
  getInverse <- function() inv  # Mengambil invers dari cache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Menghitung atau mengambil invers dari cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Mengambil dari cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)  # Menghitung invers
  x$setInverse(inv)  # Menyimpan ke cache
  inv
}
