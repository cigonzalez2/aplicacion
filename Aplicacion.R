
# Limpieza de ambiente y gráficos -----------------------------------------


rm(list = ls())
dev.off()


# Librerías ---------------------------------------------------------------


library(BLR)
library(openxlsx)
library(tidyverse)
library(rlist)
library(TSrepr)
library(MLmetrics)

# Carga de datos ----------------------------------------------------------


datos_clasicos <- read.xlsx('./Aplicacion/Resultados_Clasicos.xlsx')
prioris <- read.xlsx('./Aplicacion/Resultados_prioris.xlsx')
load('./Aplicacion/Ventas_Bayes.Rdata')
lista_cod_modelos <- Ventas %>% split(.$cod_modelo)


# Exploración de datos ----------------------------------------------------


# Los códigos de modelo son únicos en la base
datos_clasicos %>% select(cod_modelo) %>% dim()
datos_clasicos %>% select(cod_modelo) %>% unique() %>% dim()

# Elasticidad en general

elasticidad_clasica <- datos_clasicos %>% select(Elasticidad_Final) %>% 
  unlist() %>% as.numeric() 

media_el_cl <- datos_clasicos %>% 
  select(Elasticidad_Final) %>% unlist() %>% mean()

plot(
  density(elasticidad_clasica),
  xlim = c(-5, -0.25), 
  lwd = 3, 
  col = 'steelblue',
  bty = 'n')

curve(dnorm(x, 0, 0.62), -10, 0, add = T, col = 'red', lwd = 3)


# Hiper-parámetros --------------------------------------------------------

alfa <- -5
a <- 10^10
b <- a
mean <- b/(a-1)
var <- (b^2)/((a-1)^2 * (a-2))


# Regresión Bayesiana -----------------------------------------------------


# Vectores para guardar los coeficientes de cada código de modelo

lista_reg <- list()

# Un flujo para una regresión por cada código de modelo

for (k in c(1:(length(lista_cod_modelos)))) {
  
  test1 <- log(lista_cod_modelos[[k]]$Unidades_Vendidas)
  test2 <- log(lista_cod_modelos[[k]]$Precio_Promedio)
  
  lista_cod_modelos[[k]] = lista_cod_modelos[[k]][which(test1 != -Inf),]
  lista_cod_modelos[[k]] = lista_cod_modelos[[k]][which(!is.na(test1)),]
  
  lista_cod_modelos[[k]] = lista_cod_modelos[[k]][which(test2 != -Inf),]
  lista_cod_modelos[[k]] = lista_cod_modelos[[k]][which(!is.na(test2)),]
  
  cod_aux <- lista_cod_modelos[[k]] %>% 
    select(cod_linea) %>% unique() %>% as.character()
  
  var <- prioris %>% 
    filter(cod_linea == cod_aux) %>%
    select(c(Varianza_Elasticidad)) %>% as.numeric()
  
  if(!is.na(var)){
    M <- diag(c(10^(10), alfa * var), 2)  
  }
  
  else {
    M <- diag(c(10^(10), alfa * 0.1), 2)  
  }
  
  priori_aux <- prioris %>% 
    filter(cod_linea == cod_aux) %>%
    select(c(Elasticidad_Final)) %>% as.numeric()
  
  if(priori_aux == 0 | is.na(priori_aux)) {
    priori_aux <- media_el_cl
    m <- c(0, priori_aux)
  }
  
  else {
    m <- c(0, priori_aux)
  }
  
  reg <- posterior_nig(
    formula = 'log(Unidades_Vendidas) ~ log(Precio_Promedio) ', 
    data = lista_cod_modelos[[k]], 
    m = m, M = M, a = a, b = b
  )
  
  cod <- lista_cod_modelos[[k]]$cod_modelo[1]
  numero <- as.character(k)
  
  lista_aux <- list(
    'cod_modelo' = cod,
    'Priori' = priori_aux,
    coef_classic =  as.numeric(reg$classic_coef)[2],
    coef_bayes = as.numeric(reg$bayesian_coef)[2]
  )
  
  lista_reg <- list.append(lista_reg, lista_aux)
  
  avance <- paste(
    'Estado: ',
    as.character(round(100*k/length(lista_cod_modelos), 1)), 
    '%', 
    sep =''
    )
  
  print(avance)
}



# Vectores de coeficientes ------------------------------------------------


elasticidad_clasica <- NULL
vector_clasico <- NULL
vector_bayes <- NULL

# De esta manera las elasticidades quedan con los mismos índices 
# corresponden al mismo código de modelo

for (k in 1:length(lista_reg)) {
  aux <- datos_clasicos %>% 
    filter(cod_modelo == lista_reg[[k]]$cod_modelo) %>% 
    select(Elasticidad_Final) %>% as.numeric()
  elasticidad_clasica <- c(elasticidad_clasica, aux)
  vector_bayes <- c(vector_bayes, lista_reg[[k]]$coef_bayes)
  vector_clasico <- c(vector_clasico, lista_reg[[k]]$coef_classic)
  
  avance <- paste(
    'Estado: ',
    as.character(round(100*k/length(lista_reg), 1)), 
    '%', 
    sep =''
  )
  print(avance)
}


# Limpieza de NA ----------------------------------------------------------


indice1 <- which(!is.na(elasticidad_clasica))

elasticidad_clasica <- elasticidad_clasica[indice1]
vector_clasico <- vector_clasico[indice1]
vector_bayes <- vector_bayes[indice1]

indice2 <- which(!is.na(vector_clasico))

elasticidad_clasica <- elasticidad_clasica[indice2]
vector_clasico <- vector_clasico[indice2]
vector_bayes <- vector_bayes[indice2]


# Tratamiento de valores cero y positivos ---------------------------------


# OPCION 1: No se tocan las elasticidades

summary(vector_clasico)
summary(vector_bayes)
summary(elasticidad_clasica)


# OPCION 2: Valores positivos se dejan cero y lo menores a -15 se dejan -15

for (k in 1:length(vector_clasico)) {
  if(vector_clasico[k] > 0 ){
    vector_clasico[k] <- 0
  }
  
  if(vector_clasico[k] < -15 ){
    vector_clasico[k] <- -15
  }
}

for (k in 1:length(vector_bayes)) {
  if(vector_bayes[k] > 0 ){
    vector_bayes[k] <- 0
  }
  if(vector_bayes[k] < -15 ){
    vector_bayes[k] <- -15
  }
}

for (k in 1:length(elasticidad_clasica)) {
  if(elasticidad_clasica[k] < -15 ){
    elasticidad_clasica[k] <- -15
  }
}


summary(vector_clasico)
summary(vector_bayes)
summary(elasticidad_clasica)


# OPCION 3: Se eliminan los ceros y valores positivos

#vector_clasico2 <- NULL
#vector_bayes2 <- NULL 
#elasticidad_clasica2 <- NULL
#for (k in 1:length(vector_clasico)) {
#  if(vector_clasico[k] < 0.1 ){
#    vector_clasico2 <- c(vector_clasico2, vector_clasico[k])
#  }
#}
#
#for (k in 1:length(vector_bayes)) {
#  if(vector_bayes[k] < 0.1 ){
#    vector_bayes2 <- c(vector_bayes2, vector_bayes[k])
#  }
#}
#
#for (k in 1:length(elasticidad_clasica)) {
#  if(elasticidad_clasica[k] < 0 ){
#    elasticidad_clasica2 <- c(elasticidad_clasica2, elasticidad_clasica[k])
#  }
#}
#
#
#vector_clasico <- vector_clasico2
#vector_bayes <- vector_bayes2
#elasticidad_clasica <- elasticidad_clasica2
#
#min(vector_clasico)
#max(vector_clasico)
#mean(vector_clasico)
#
#min(vector_bayes)
#max(vector_bayes)
#mean(vector_bayes)
#
#min(elasticidad_clasica)
#max(elasticidad_clasica)
#mean(elasticidad_clasica)

# Gráficos  ---------------------------------------------------------------

# Histogramas

elasticidades <- data.frame(
  'Elasticidad' = c(
    vector_bayes,
    elasticidad_clasica, 
    vector_clasico
  ),
  'Modelo' = c(
    rep("Bayes", length(vector_bayes)),
    rep("Cadena", length(elasticidad_clasica)),
    rep("Clásico", length(vector_clasico))
  )
)

ggplot(elasticidades) +
  aes(x = Elasticidad, fill = Modelo) +
  geom_histogram(
    aes(y =..density..),
    position = "identity", 
    alpha = 0.6, 
    breaks = seq(-5, 0, 0.26),
    col = 1
    ) +
  ggtitle("Histogramas") +
  labs(y="Probabilidad", x = "Elasticidad") +
  theme_minimal()

elasticidades %>% 
  filter(Modelo == 'Cadena') %>% 
  ggplot() +
  aes(x = Elasticidad, y =..density..) +
  ggtitle("Histograma Cadena") +
  labs(y="Probabilidad", x = "Elasticidad") +
  theme_minimal() +
  geom_histogram(
    aes(y =..density..),
    position = "identity", 
    alpha = 0.6, 
    breaks = seq(-5, 0, 0.26),
    col = 1,
    fill = 3
  ) 

elasticidades %>% 
  filter(Modelo == 'Bayes') %>% 
  ggplot() +
  aes(x = Elasticidad, y =..density..) +
  ggtitle("Histograma Bayes") +
  labs(y="Probabilidad", x = "Elasticidad") +
  theme_minimal() +
  geom_histogram(
    aes(y =..density..),
    position = "identity", 
    alpha = 0.6, 
    breaks = seq(-5, 0, 0.26),
    col = 1,
    fill = 2
  ) 

elasticidades %>% 
  filter(Modelo == 'Clásico') %>% 
  ggplot() +
  aes(x = Elasticidad, y =..density..) +
  ggtitle("Histograma Clásico") +
  labs(y="Probabilidad", x = "Elasticidad") +
  theme_minimal() +
  geom_histogram(
    aes(y =..density..),
    position = "identity", 
    alpha = 0.6, 
    breaks = seq(-5, 0, 0.26),
    col = 1,
    fill = 4
  ) 

# Densidades

ggplot(elasticidades) +
  aes(x = Elasticidad, fill = Modelo) +
  geom_density(color = 0, alpha = 0.5) + 
  ggtitle("Gráfico de Densidades") +
  labs(y="Densidad", x = "Elasticidad") +
  theme_minimal() +
  xlim(-5, 0) + 
  ylim(0, 2)

elasticidades %>% 
  filter(Modelo == 'Cadena') %>% 
  ggplot() +
  aes(x = Elasticidad, fill = Modelo) +
  geom_density(color = 0, alpha = 0.6, fill = 3) + 
  ggtitle("Gráfico de Densidad Cadena") +
  labs(y="Densidad", x = "Elasticidad") +
  theme_minimal() +
  xlim(-5, 0) + 
  ylim(0, 1)

elasticidades %>% 
  filter(Modelo == 'Bayes') %>% 
  ggplot() +
  aes(x = Elasticidad, fill = Modelo) +
  geom_density(color = 0, alpha = 0.6, fill = 2) + 
  ggtitle("Gráfico de Densidad Bayes") +
  labs(y="Densidad", x = "Elasticidad") +
  theme_minimal() +
  xlim(-5, 0) + 
  ylim(0, 1)

elasticidades %>% 
  filter(Modelo == 'Clásico') %>% 
  ggplot() +
  aes(x = Elasticidad, fill = Modelo) +
  geom_density(color = 0, alpha = 0.6, fill = 4) + 
  ggtitle("Gráfico de Densidad Clásico") +
  labs(y="Densidad", x = "Elasticidad") +
  theme_minimal() +
  xlim(-5, 0) + 
  ylim(0, 1)
 


# t-test ------------------------------------------------------------------

t.test(
  elasticidad_clasica, vector_bayes, 
  paired = TRUE, 
  alternative = "two.sided"
)


# t-test referencia 
X1 <- elasticidad_clasica
X2 <- vector_bayes
n1 <- length(X1)
n2 <- length(X2)
var1 <- var(X1)
var2 <- var(X2)
u <- var2/var1
MX1 <- mean(X1)
MX2 <- mean(X2)
t <- (MX1 -MX2)/sqrt((var1/n1)+(var2/n2))
df <- ((1/n1)^2 + (u/n2)^2)/( (1/((n1-1)*n1^2)) +((u^2)/((n2-1)*n2^2)) )
p <- 2 * pt(t, df)

t.test(
  elasticidad_clasica, vector_clasico, 
  paired = TRUE, 
  alternative = "two.sided"
)


# MAPE --------------------------------------------------------------------

denominador <- abs(elasticidad_clasica)

num_clasico <- abs(
  vector_clasico - elasticidad_clasica
)
frac_clasico <- num_clasico/denominador
indices_clasico <- which(is.finite(frac_clasico))
MAPE_clasico <- round(mean(frac_clasico[indices_clasico]), 2)

num_bayes <- abs(
  vector_bayes - elasticidad_clasica
)
frac_bayes <- num_bayes/denominador
indices_bayes <- which(is.finite(frac_bayes))
MAPE_bayes <- round(mean((frac_bayes[indices_bayes])), 2)


df_mape <- data.frame(
  'Tipo' = c('Clasico', 'Bayes'),
  'MAPE' = c(MAPE_clasico, MAPE_bayes)
)

df_mape


# MAE ---------------------------------------------------------------------


MAE_bayes <- mae(y = vector_bayes, x = elasticidad_clasica)
MAE_clasico <- mae(y = vector_clasico, x = elasticidad_clasica)

df_mae <- data.frame(
  'Tipo' = c('Clasico', 'Bayes'),
  'MAE' = c(MAE_clasico, MAE_bayes)
)

df_mae

# MAAPE -------------------------------------------------------------------


MAAPE_clasico <-maape(x = elasticidad_clasica, y = vector_clasico)/100

MAAPE_bayes <- maape(x = elasticidad_clasica, y = vector_bayes)/100


df_maape <- data.frame(
  'Tipo' = c('Clasico', 'Bayes'),
  'MAAPE' = c(MAAPE_clasico, MAAPE_bayes)
)

df_maape


# Medidas -----------------------------------------------------------------


df_medidas <- data.frame(
  'Tipo' = c('Clasico', 'Bayes'),
  'MAE' = c(MAE_clasico, MAE_bayes),
  'MAPE' = c(MAPE_clasico, MAPE_bayes),
  'MAAPE' = c(MAAPE_clasico, MAAPE_bayes)
)

df_medidas

