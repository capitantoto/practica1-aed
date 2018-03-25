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
  acum <- c()
  while(sum(acum) < 500) {
    acum <- c(acum, runif(1))
  }
  print(paste("Suma:", sum(acum), "(", length(acum), "números acumulados)"))
  return(acum)
}

sumarPositivos <- function(lista) {
  positivos <- lista[lista > 0]
  return(ifelse(is.null(positivos), 0, sum(positivos)))
}
ej_8 <- sumarPositivos

l <- c(4,3,2)
assertthat::are_equal(sumarPositivos(l), 9)
assertthat::are_equal(sumarPositivos(2*l), 18)
assertthat::are_equal(sumarPositivos(l-3), 1)

filtrarPositivos <- function(lista) {
  return(lista[lista > 0])
}
ej_9 <- filtrarPositivos

l <- c(4,3,2)
assertthat::are_equal(filtrarPositivos(l), l)
assertthat::are_equal(filtrarPositivos(-l), numeric(0))
# numeric(n) crea un vector numerico (double) de longitud n, con 0 en todas sus posiciones
# asumo que por eso una lista que se vacia al filtrarla es 'numeric(0)' y no NULL, como c()
assertthat::are_equal(filtrarPositivos(l-3), c(1))

cuadradosDePositivos <- function(lista) {
  return(filtrarPositivos(lista)**2)
}

cuadrado <- cuadradosDePositivos

assertthat::are_equal(
  cuadradosDePositivos(c(1,-1,2,-2,3,-3)),
  c(1,4,9))


grilla <- seq(-50, 50, length.out = 1000)


### Con las funciones de base-r
plot(grilla, sin(grilla), "l", col="red", xlab='x', ylab='f(x)')
lines(grilla, cos(grilla), type = "l", col="green")
lines(grilla, cos(grilla)**2, type = "l", col="blue")
# en realidad el ejercicio pide graficar cos(grilla**2), pero el gráfico que muy fiero sino
# lines(grilla, cos(grilla**2), type = "line", col="blue")
title("sin(x), cos(x), cox(x^2)")
legend("top", legend = c("sin(x)", "cos(x)", "cos(x)^2"),
       col = c("red", "green", "blue"), lty=1)

dev.copy(pdf, 'ej11-base.pdf', width = 12, height = 4)
dev.off()

### Con ggplot2
library(tidyverse)

data.frame("x"=grilla,
          `sin_x`=sin(grilla),
          "cos_x"=cos(grilla),
          "cos2_x"=cos(grilla)**2) %>%
  gather(`sin_x`, `cos_x`, `cos2_x`,
         key = 'funcion', value = 'valor') -> df

ej11_ggplot2 <- ggplot(df, mapping=aes(x=x, y=valor, color=funcion)) +
  geom_line() +
  labs(title="sin(x), cos(x), cos^2(x)", y="f(x)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Función",
                       breaks = c("sin_x", "cos_x", "cos2_x"),
                       labels = c("sin(x)", "cos(x)", "cos^2(x)"))
  


autos <- read_delim("autos.txt", delim=" ")

autos %>% ggplot(mapping = aes(x=precio, y=calidad)) +
  geom_point() +
  labs(title="autos.txt - relación precio-calidad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = scales::comma)-> ej12

ggsave("ej12.pdf", ej12, width = 16, height = 9)

# Con dplyr
autos %>% arrange(precio) -> sort_dplyr
# Con base r
autos[order(autos$precio),] -> sort_base

all.equal(sort_dplyr, sort_base)

cumplesRepetidos <- function(alumnos = 20, dias = 265, sims = 100000) {
  exitos <- 0
  for (i in 1:sims) {
    muestra <- sample(1:dias, alumnos, replace=T)
    if (any(duplicated(muestra))) { exitos <- exitos + 1}
  }
  return(exitos/sims)
}
ej_14 <- cumplesRepetidos

# Hay una función específica para esto!
pbirthday(n = 20, classes = 365, coincident = 2)

#### Ejercicio 15 ####
juego <- function(capital_inicial = 100) {
  capital <- capital_inicial
  n <- 0
  while(capital > 0) {
    tirada <- sample(x = c("rojo", "negro", "verde"),
                     size = 1,
                     # `prob` es un vector de _pesos_, puede no sumar 1,
                     # `sample` debe normalizarlo internamente
                     prob = c(18, 18, 2),
                     # clave en ruleta que haya reposicion,
                     # sobre todo con el vector `prob` de pesos
                     replace = T)
    n <- n + 1
    capital <- capital + ifelse(tirada == "rojo", 1, -1)
  }
  return(n)
}

simularJuegos <- function(capital_inicial = 100, sims = 1000) {
  juegos <- c()
  for (i in 1:sims) {
    juegos <- c(juegos, juego(capital_inicial))
    if (i %% 100 == 0) {print(i)}
  }
  return(juegos)
}

set.seed(1991)
sims <- 1000
resultados_juegos <- simularJuegos(sims = sims)
media_juegos <- mean(resultados_juegos)

crearHistogramaSimulacion <- function(resultados, descripcion) {
  bw <- diff(range(resultados)) / (2 * IQR(resultados) / length(resultados)^(1/3))
  media <- mean(resultados)
  data.frame(x = resultados) %>% ggplot(mapping = aes(x = x)) +
    geom_histogram(binwidth = bw, alpha=0.6) +
    geom_vline(xintercept = media, color = "red", linetype = "dashed") +
    annotate(geom = "text", x = media, y = 0,
             label=paste("Media:", media), size=4, hjust = 0, vjust = 0) +
    labs(title = paste(descripcion, ": valor promedio e histograma para", length(resultados), "simulaciones."),
         x = descripcion, y = "Frecuencia") -> grafico
  return(grafico)
}

resultados <- simularJuegos(capital_inicial = 100, sims = 1000)
crearHistogramaSimulacion(resultados, descripcion = "Tiradas hasta la ruina") -> ej15
ggsave("ej15.pdf", ej15, width = 16, height = 9)

llenarAlbum <- function (figuritas_album = 500, figuritas_sobre = 5, sobres_con_repetidas = F) {
  album <- rep(0, figuritas_album)
  lleno <- ifelse(figuritas_album == 0, T, F)
  # album <- numeric(500) es equivalente
  n_sobres <- 0
  while(!lleno) {
    sobre_nuevo <- sample(1:figuritas_album, figuritas_sobre, replace = sobres_con_repetidas)
    for(figurita in sobre_nuevo) {
      album[figurita] <- album[figurita] + 1
    }
    n_sobres <- n_sobres + 1
    lleno <- all(album != 0)
  }
  return(n_sobres)
}

simularColecciones <- function (figuritas_album, figuritas_sobre, sobres_con_repetidas = F, sims = 1000) {
  colecciones <- c()
  for(i in 1:sims) {
    colecciones <- c(colecciones, 
                     llenarAlbum(figuritas_album = figuritas_album,
                                 figuritas_sobre = figuritas_sobre,
                                 sobres_con_repetidas = sobres_con_repetidas))
    if (i %% 100 == 0) {print(i)}
  }
  return(colecciones)
}

n_album <- 500
n_sobres <- 5
colecciones <- simularColecciones(figuritas_album = n_album,
                                  figuritas_sobre = n_sobre,
                                  sobres_con_repetidas = F,
                                  sims = 1000)
crearHistogramaSimulacion(colecciones, descripcion = "Sobres para completar colección") -> ej16
ej16 +
  labs(subtitle = paste("Resultados para un álbum de", n_album,
                        "y sobres de", n_sobres, "figuritas.")) -> ej16 
ggsave("ej16.pdf", ej16, width = 16, height = 9)

mat_ej17 <- matrix(c(1,1,2,4,3,0,5,1,4), nrow = 3, ncol = 3)
vec_ej17 <- c(1, -1, 3)

ej18 <- det(mat_ej17)

ej19 <- solve(mat_ej17, vec_ej17)


# Los ejercicios 20 (escalar), 21 (armarDiagonal) y 22 (obtenerDiagonal)
# son 3 de los 4 casos de uso de la funcion `diag`. Leyendo `?diag`:

# diag has four distinct usages:
#   1. x is a matrix, when it extracts the diagonal.
#      Equivalente a `obtenerDiagonal`
#   2. x is missing and nrow is specified, it returns an identity matrix.
#      no cubierto
#   3. x is a scalar (length-one vector) and the only argument, it returns a square identity matrix of
#     size given by the scalar.
#    `escalar(alfa, n)`` es equivalente a `diag(x = alfa, nrow = n)`
#   4. x is a numeric vector, either of length at least 2 or there were further arguments.
#     This returns a matrix with the given diagonal and zero off-diagonal entries.
#     Equivalente a `armarDiagonal`

normaFrobenius <- function(matriz) {
  matriz ** 2 %>% sum() %>% sqrt() -> norma
  return(norma)
}
assertthat::are_equal(normaFrobenius(mat_ej17), sqrt(73))

grilla <- function(desde, hasta, n) {
  return(seq(desde, hasta, length.out = n))
}

siguiente_aproximacion <- function(aprox) {
  nueva_aprox <- (aprox / 2) + (1 / aprox)
  return(nueva_aprox)
}

aprox1 <- function(n) {
  resultado <- 1
  for (i in 1:n) {
    resultado <- siguiente_aproximacion(resultado)
  }
  return(resultado)
}

aprox2 <- function(umbral) {
  aproximacion <- 1
  while(T) {
    siguiente <- siguiente_aproximacion(aproximacion)
    diferencia <- abs(siguiente-aproximacion)
    if (diferencia < umbral) {
      return(siguiente)
    } else {
      aproximacion <- siguiente
    }
  }
}

aprox3 <- function(n, umbral) {
  aproximacion <- 1
  for (i in 1:n) {
    siguiente <- siguiente_aproximacion(aproximacion)
    diferencia <- abs(siguiente-aproximacion)
    if (diferencia < umbral) {
      return(siguiente)
    } else {
      aproximacion <- siguiente
    }
  }
  return(aproximacion)
}
