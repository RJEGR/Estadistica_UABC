
source("~/Documents/GitHub/Estadistica_UABC/anova_and_gaussianity.R")

# # # # # #
# Test w/ data-toy ---- 
# # # # # #

library(tidyverse)

c1 <- c(2, 2.8, 3.3, 3.2, 4.4, 3.6, 1.9, 3.3, 2.8, 1.1);c2 <- c(3.5, 2.8, 3.2, 3.5, 2.3, 2.4, 2, 1.6);c3 <- c(3.3, 3.6, 2.6, 3.1, 3.2, 3.3, 2.9, 3.4, 3.2, 3.2)

Rcalculate_coeff(c1)
Rcritical_coeff(length(c1))
Ztransform(c1)


# Tabla de anova 1-via ----

dt <- rbind(
  data.frame(x = c1, g = 'c1'),
  data.frame(x = c2, g = 'c2'),
  data.frame(x = c3, g = 'c3')
)
# 0) test if parametric


is_parametric(dt)
aov_homemade(dt)

# # # # # # # # # # # #
# desgloce de la formula ----
# # # # # # # # # # # #

# nota: para el caso de las formulas operacionales (usadas por todos los programas, pero con sus desviaciones), aplicaremos los datos del caldero de la bruja, ie. de los datos pooleados

k <- length(unique(dt$g))
Ntotal <- length(dt$x)

# 1) Suma de cuadrados SS

CM <- sum(dt$x)^2/length(dt$x) # ver nota

# 1.1) SS entre tratamiento (SST)
dt %>%
  group_by(g) %>%
  summarise(A = sum(x),
            B = A^2,
            C = B/length(x)) %>%
  pull(C) %>% sum - CM -> SST


# 1.2 SS Total (SSTo)
dt %>%
  mutate(s = x^2) %>%
  summarise(sum(s)) %>%
  pull() - CM -> SSTo

# 1.3 SS Error 
SSE <- SSTo-SST


# 2. Medias cuadraticas (MS)
MST <- SST / (k-1)
MSE <- SSE / (Ntotal-k)
MSTo <- SSTo / (Ntotal-1)

gl <- c(k-1,  Ntotal-k, Ntotal-1)
SS <- c(SST, SSE, SSTo)
MS <- c(MST, MSE, MSTo)

AnovaDF <-data.frame(gl = gl, SS = SS, MS = MS)

# 3. valores criticos de una distribucion F a 5%

Fcal <- MST / MSE
Fcri <- qf(0.95, df1 = k-1, df2 = Ntotal-k)

AnovaDF[,4] <- c(round(Fcal, digits = 3), '', '')
AnovaDF[,5] <- c(round(Fcri, digits = 3), '', '')

rownames(AnovaDF) <- c("Treatment", "Error", "Total")
colnames(AnovaDF)[c(4,5)] <- c('Fcal', 'Fcri')

# Resultados 
# en vez de usar valores p decimos, 
# si la Fcal > Fcri, decimos que se rechaza Ho, 
# y que no hay igualdades entre los tratamientos

# La funcion anterior, es equivalente a : 
dt %>%
  aov(x ~ g, data = .) %>%
  summary.aov() # F value es Fcal

qf(1-0.05, df1 = k-1, df2 = Ntotal-k)
qf(0.95, 2, 25) # los grados de libertad de la columna Df del resumen de la prueba de anova 1-via
# la varianza entre tratamientos es igual a la varianza a la critica, pro tanto se dice que los tratamientos son iguales

