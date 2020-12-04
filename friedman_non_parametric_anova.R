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
  Frcri <- qf(0.95, df1 = k-1, df2 = Ntotal-k)
  
  data.frame(FrCal, Frcri)
}
# ejercicio

a <- c(6,9,6,5,7,5,6,6)
b <- c(5,8,9,8,8,7,7,5)
c <- c(3,4,3,6,9,6,5,7)

dt <- rbind(
  data.frame(x = a, g = 'None'),
  data.frame(x = b, g = 'Clasic'),
  data.frame(x = c, g = 'Dance')
)

