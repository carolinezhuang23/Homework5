## HW5 Class/Methods
library(ggplot2)

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

# validation
setValidity(
  Class = "sparse_numeric",
  method = function(object){
    if(anyNA(object@value))
      return("Values must not contain NA values.")
    if(anyNA(object@pos) || any(object@pos < 1L))
      return("Can not have negative or NA position.")
    if(length(object@value) != length(object@pos))
      return("Length of values must be equal to length of positions.")
    if(length(object@length) != 1L || is.na(object@length) || object@length < 1)
      return("Length must be one single positive integer.")
    if(any(object@pos > object@length))
      return("Position must not be greater than length of sparse vector.")
    TRUE
  }
)

# coerce sparse_vector --> numeric
setAs(
  from = "sparse_numeric",
  to = "numeric",
  def = function(from){
    n <- as.integer(from@length)
    out <- numeric(n)
    if(length(from@pos))
      out[from@pos] <- from@value
  }
)

# coerce numeric --> sparse_vector
setAs(
  from = "numeric",
  to = "sparse_numeric",
  def = function(from){
    n <- as.integer(length(from))
    values <- c()
    positions <- c()
    for(i in seq_len(n)){
      if(from[i] != 0){
        values <- append(values, from[i])
        positions <- append(positions, i)
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = n)
    return(new_vector)
  }
)
      
# Show method 
setMethod("show", "sparse_numeric",
          definition = function(object){
            j <- 1L
            pos <- object@pos
            val <- object@value
            for(i in seq_len(object@length)){
              if(j <= length(pos) && i == pos[j]){
                cat(sprintf("Vector has value %g at position %d\n", val[j], pos[j]))
                j <- j + 1L
              }
              else {
                cat(sprintf("Vector has value 0 at position %d\n", i))
              }
            }
})

# plot method
setMethod("plot",
          signature = c(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("length mismatch in plot.sparse_numeric")
            
            idx <- intersect(x@pos, y@pos)
            xv <- x@value[match(idx, x@pos)]
            yv <- y@value[match(idx, y@pos)]
            
            df <- data.frame(x = xv, y = yv, pos = idx)
            p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point() +
              ggplot2::labs(
                title = "Overlapping non-zero entries",
                x = "x value (at overlapping positions)",
                y = "y value (at overlapping positions)"
              )
            print(p)
          }
)

# Generic add function
setGeneric(
  "sparse_add",
  function(x, y, ...) standardGeneric("sparse_add")
)
# Custom add function
setMethod(
  "sparse_add",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L
    
    while(i <= length(x_pos) || j <= length(y_pos)){
      if(j > length(y_pos) || (i <= length(x_pos) && x_pos[i] < y_pos[j])){
        values <- append(values, x_value[i])
        positions <- append(positions, x_pos[i])
        i <- i + 1L
      } else if(i > length(x_pos) || y_pos[j] < x_pos[i]){
        values <- append(values, y_value[j])
        positions <- append(positions, y_pos[j])
        j <- j + 1L
      } else{
        s <- x_value[i] + y_value[j]
        if(s != 0){
          values <- append(values, s)
          positions <- append(positions, x_pos[i])
        }
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)

setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_add(e1, e2))
          }
)

# Generic subtraction method x - y)
setGeneric(
  "sparse_sub",
  function(x, y, ...) standardGeneric("sparse_sub")
)
# Custom subtraction function
setMethod(
  "sparse_sub",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L
    
    while(i <= length(x_pos) || j <= length(y_pos)){
      if(j > length(y_pos) || (i <= length(x_pos) && x_pos[i] < y_pos[j])){
        values <- append(values, x_value[i])
        positions <- append(positions, x_pos[i])
        i <- i + 1L
      } else if(i > length(x_pos) || y_pos[j] < x_pos[i]){
        values <- append(values, -y_value[j])
        positions <- append(positions, y_pos[j])
        j <- j + 1L
      } else{
        s <- x_value[i] - y_value[j]
        if(s != 0){
          values <- append(values, s)
          positions <- append(positions, x_pos[i])
        }
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)

setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_sub(e1, e2))
          }
)

# Generic multiplication method
setGeneric(
  "sparse_mult",
  function(x, y, ...) standardGeneric("sparse_mult")
)
# Custom multiplication function
setMethod(
  "sparse_mult",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L
    
    while(i <= length(x_pos) || j <= length(y_pos)){
      if(x_pos[i] < y_pos[j]){
        i <- i + 1L
      }
      else if(y_pos[j] < x_pos[i]){
        j <- j + 1L
      }
      else{
        values <- append(values, x_value[i] * y_value[j])
        positions <- append(positions, x_pos[i])
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)

setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_mult(e1, e2))
          }
)

# Generic cross-multiplication method
setGeneric(
  "sparse_crossprod",
  function(x, y, ...) standardGeneric("sparse_crossprod")
)
# Custom multiplication function
setMethod(
  "sparse_crossprod",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_value <- x@value
    y_value <- y@value
    values <- c()
    i <- 1L
    
    totals <- 0
    while(i <= length(x_value)){
      xv <- x_value[i]
      j <- 1L
      while(j <= length(y_value)){
        yv <- y_value[j]
        sum <- xv * yv
        totals <- totals + sum
        j <- j + 1L
      }
      i <- i + 1L
    }
    return(as.numeric(totals))
  }
)

# Last Generic Method
setGeneric(
  "sparse_div",
  function(x, y, ...) standardGeneric("sparse_div")
)
# Custom multiplication function
setMethod(
  "sparse_div",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L
    
    while(i <= length(x_pos) || j <= length(y_pos)){
      if(x_pos[i] < y_pos[j]){
        i <- i + 1L
      }
      else if(y_pos[j] < x_pos[i]){
        j <- j + 1L
      }
      else{
        values <- append(values, as.integer(floor(x_value[i] / y_value[j])))
        positions <- append(positions, x_pos[i])
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)

setMethod("/",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_div(e1, e2))
          }
)


x <- as(c(2, 4, 1, 0, 3), "sparse_numeric")
y <- as(c(1, 1, 0, 0, 2), "sparse_numeric")

# Addition
result_add <- sparse_add(x, y)
result_add
x + y

# Subtraction
result_sub <- sparse_sub(x, y)
result_sub
x - y 

# Multiplication
result_mult <- sparse_mult(x, y)
result_mult
x * y 

# Cross multiplication
x <- as(c(2, 0, 0, 0, 3), "sparse_numeric")
y <- as(c(1, 1, 0, 0, 2), "sparse_numeric")
result_cross <- sparse_crossprod(x, y)
result_cross

# Division
x <- as(c(2, 0, 0, 0, 5), "sparse_numeric")
y <- as(c(1, 1, 0, 0, 2), "sparse_numeric")
result_div <- sparse_div(x, y)
result_div
x / y
