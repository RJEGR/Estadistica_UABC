# # # # # #
# Functions ----
# # # # # #

# Probar gaussianidad

Rcalculate_coeff <- function(x, method = 'pearson') {
  x_sorted <- sort(x)
  n <- length(x_sorted)
  j <- 1:n
  pj <- (j-0.5) / n 
  
  ztable <- qnorm(pj, lower.tail = T) # nuestro referente de gaussianidad
  # plot(ztable, x_sorted)
  # qqline(x_sorted)
  
  Cpearson <- cor(ztable, x_sorted, method = method)
  return(Cpearson)
}
Rcritical_coeff <- function(n) {
  # con un alfa de 5%
  
  a <- (0.6118/n)
  b <- (1.3505/n**2)
  c <- (0.1288/sqrt(n))
  
  rcri <- 1.0063 - a + b - c
  
  return(rcri)
  
  # cat("\n", "The critical r is: ", rcri,
  #     "\nIf the correlation coefficient (r) is less than this value, the population is non-normal. If r is greater than this value, the population is normal")
} 

# probar valores extremos usando la media como medida central
Ztransform <- function(x) {
  
  z <- (x - mean(x)) / sd(x)
  # plot(z, x)
  # abline(v =  c(-3, 3), col = "red")
  
  return(z)
}

# 
is_parametric <- function(dt, outliers = T, gaussianity = T) {
  
  dt %>%
    group_by(g) %>%
    summarise(n= sum(x > 0),
              Zmax = max(Ztransform(x)),
              Zmin = min(Ztransform(x)),
              rCal = Rcalculate_coeff(x),
              rCri = Rcritical_coeff(n),
              gaussian = ifelse(rCal > rCri, TRUE, FALSE),
              outliers = sum(abs(Ztransform(x) > 3))) -> dt
  
  if(!outliers) {
    dt %>% select(-Zmin, -Zmax, -outliers) -> dt
  }
  
  if(!gaussianity) {
    dt %>% select(-rCri, -rCal, -gaussian) -> dt
  }
  
  return(dt)
}

# prueba anova 1 via - parametrica
aov_homemade <- function(df, p = 0.5) {
  
  library(tidyverse)
  
  k <- length(unique(dt$g))
  Ntotal <- length(dt$x)
  
  CM <- sum(dt$x)^2/Ntotal
  
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
  
  AnovaDF <-data.frame(gl = gl, 
                       SS = round(SS, digits = 3), 
                       MS = round(MS, digits = 3))
  
  # 3. valores criticos de una distribucion F a 5%
  
  Fcal <- MST / MSE
  Fcri <- qf(1-p, df1 = k-1, df2 = Ntotal-k)
  
  AnovaDF[,4] <- c(round(Fcal, digits = 3), '', '')
  AnovaDF[,5] <- c(round(Fcri, digits = 3), '', '')
  
  rownames(AnovaDF) <- c("Treatment", "Error", "Total")
  colnames(AnovaDF)[c(4,5)] <- c('Fcal', 'Fcri')
  
  return(AnovaDF)
  
}


           