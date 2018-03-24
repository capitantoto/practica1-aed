## Funciones de sooporte
sum2 <- function(vec) {
  acum <- 0
  for(i in 1:length(vec)) {
    acum <- + vec[i]
  }
  return(acum)
}
    
sum3 <- function(vec) {
  acum <- 0
  for(i in vec) {
    acum <- acum + i
  }
  return(acum)
}

assertthat::are_equal(sum2(23999), sum3(23999))

## Ejercicios

ej_1 <- function(n = 1000) {
  res <- 0
  for (i in 1:n) {
    res <- res + i
  }
  return(res)
}

assertthat::are_equal(ej_1(), ej_1(1000))
assertthat::are_equal(ej_1(), sum(1:1000))

ej_2 <- function(i_lim, j_lim) {
  res <- 0
  for (i in 1:i_lim) {
    for (j in 1:j_lim) {
      res <- res + i / (i + j^2 + 2)
    }
  }
  return(res)
}

ej_2(20, 10)

ej_3 <- function(i_lim) {
  res <- 0
  for (i in 1:i_lim) {
    for (j in 1:i) {
      res <- res + i / (i + j^2 + 2)
    }
  }
  return(res)
}

ej_3(15)

ej_4 <- function(min_suma) {
  intento <- 1
  while(ej_1(intento) <= min_suma) {
    intento <- intento + 1
  }
  return(intento)
}
  
ej_4(10e6)

help(runif)

# identico a runif(100)
ej_5 <- function() {
  sum(runif(100, min  = 0, max = 1))
}

ej_5()

ej_6 <- function(no_divisible_por = 7, hasta = 500) {
  acum <- 0
  for(i in 1:hasta) {
    acum <- acum + ifelse(i %% no_divisible_por != 0, i, 0)
  }
  return(acum)
}

ej_6()

ej_7 <- function(sumar_hasta = 500) {
  elem <- 1
  acum <- list()
  while(acum < 500) {
    acum[elem] <- runif(1) 
    elem 
  }
}

