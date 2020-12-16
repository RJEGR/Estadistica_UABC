library(tidyverse)

# Anova no parametrico > prueba de friendman

a <- c(6,9,6,5,7,5,6,6)
b <- c(5,8,9,8,8,7,7,5)
c <- c(3,4,3,6,9,6,5,7)

# dt <- rbind(
#   data.frame(x = a, g = 'A'),
#   data.frame(x = b, g = 'B'),
#   data.frame(x = c, g = 'C')
# )

df <- data.frame(a,b,c)

apply(df, 1, rank) %>% rowSums() -> Tranks

r <- nrow(df)
k <- ncol(df)

A <- 12 / ((r*k)*(k+1))
B <- sum(Tranks^2)
C <- 3*r*(k+1)

# FrCal <- 
(A*sum(B))-C
#Frcri
qchisq(p=.05, df= k-1, lower.tail=F)

# make a function
Friedman <- function(df) {
  
  apply(df, 1, rank) %>% rowSums() -> Tranks
  
  r <- nrow(df)
  k <- ncol(df)
  
  A <- 12 / ((r*k)*(k+1))
  B <- sum(Tranks^2)
  C <- 3*r*(k+1)
  
  # FrCal <- 
  FrCal <- (A*sum(B))-C
  #Frcri
  Frcri <- qchisq(p=.05, df= k-1, lower.tail=F)
  
  data.frame(FrCal, Frcri)
}

Friedman(df)

# ejercicio

a <- c(8,7,6,8,5,9,7,8,8,7,7,9)
b <- c(8,6,8,9,8,7,7,7,6,6,8,9)
c <- c(7,6,6,7,5,7,7,7,8,6,6,6)

df <- data.frame(a,b,c)

df <- df[apply(df, 1, var) > 0,]

Friedman(df)

# se rechaza Ho, por tanto, evaluamos con prueba de signos para contraste multiple:
# (por continuar)
df %>%
  mutate(difac = a-c,
         difab = a-b,
         difbc = b-c) %>%
  select(-a,-b,-c) %>%
  colSums() 

