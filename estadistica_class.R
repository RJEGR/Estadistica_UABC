mean <- 3
sd <- 1
#

x <- seq(0, 10, by = 0.1)
# The Normal Distribution

plot(dnorm(x, mean, sd), type = "l")
# or 
plot(pnorm(x, mean, sd), type = "l")

ecdf(dnorm(x, mean, sd))

mkNorm <- function(mean, sd) {
  
  x <- seq(0, 200, by = 0.1)
  # The Normal Distribution
  norm <- rnorm(x, mean = mean, sd = sd)
  norm <- round(norm)
  return(norm)
}

set.seed(10102020)

a <- mkNorm(3.63, 1.03)
b <- mkNorm(100, 16)
c <- mkNorm(27, 3)
d <- mkNorm(325, 15)
e <- mkNorm(151, 15)

rb <- rbind(
  data.frame(value = a, group = "a"),
  data.frame(value = b, group = "b"),
  data.frame(value = c, group = "c"),
  data.frame(value = d, group = "d"),
  data.frame(value = e, group = "e")
)

library(ggplot2)
library(tidyverse)

library(Hmisc)
ecdfa <- Ecdf(a)

Ecdf(d) %>%
  as_tibble() %>%
  view()

Ecdf(a) %>%
  as_tibble() %>%
  filter(x < 0.72)

Ecdf(a) %>%
  as_tibble() %>%
  filter(x >= 2 & x <= 3)

Ecdf(a) %>%
  as_tibble() %>%
  filter(x > 5)

rb %>%
  filter(group %in% "a") %>%
  ggplot(., aes(value, color = group, group = group)) +
  stat_ecdf(geom = "step") +
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(x)", 
       x = " ")+
  theme_classic()
  # facet_grid(~ group, scales = "free_x")

ggsave(., filename = "~/Documents/ecdf_")

a <- mkNorm(3.63, 1.03)
b <- mkNorm(100, 16)
c <- mkNorm(27, 3)
d <- mkNorm(325, 15)
e <- mkNorm(151, 15)

vl_mean <- c(3.63, 100, 27, 325, 151)
vl_sd <- c(1.03, 16, 3, 15, 15)

vl.dat <- data.frame(group = levels(rb$group), 
                        vl_mean = vl_mean,
                        vl_sd = vl_sd)

library(ggplot2)

ggplot(rb, aes(value, fill = group, group = group)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ group, scales = "free") +
  labs(y = "PMF", 
       x = " ") +
  theme_classic() -> p1

p1 + geom_vline(aes(xintercept = vl_mean),
                   color = "blue", data = vl.dat) -> p1

p1 + geom_vline(aes(xintercept = vl_sd),
             color = "red", data = vl.dat) -> p1

p1

#
#también podemos calcular la proporción de valores entre dos valores (o fuera de esos dos valores)

props <- pnorm(c(5.4,6.6), 
               mean = 6.0, sd = 1.1, 
               lower.tail = TRUE)
props

vl_mean <- c(3.63, 100, 27, 325, 151)
vl_sd <- c(1.03, 16, 3, 15, 15)

pnorm(6.6, mean = 6.0, sd = 1.1, lower.tail = TRUE)

# a)
pnorm( 0.72,# c(2,3), 
      mean = 3.63, sd = 1.03, 
      lower.tail = TRUE)

# ejemplo
esp <- 21
obs <- c(24, 20, 22) 

sum((esp-obs)^2 / esp)


# 1.
esp <- c(7.8, 19.8, 67.2, 5.2)
obs <- c(35,90,325,50) 
# obs<- obs / 500 *100

esp <- esp * 500 / 100

xcal <- (obs-esp)**2 / esp
sum(xcal)

alpha <- 0.5
k <- length(obs)
qchisq(alpha, k)

# 2 

esp <- c(50, 50, 100)
obs <- c(48, 69, 83)
sum((esp-obs)**2 / esp)

# 3

obs <- c(17, 27, 22, 15, 19)
esp <- sum(obs) / length(obs)
sum((esp-obs)**2 / esp)

qchisq(p=.95, df= 3, lower.tail=T)
# or
qchisq(p=.05, df= 3, lower.tail=F)

#https://www.statology.org/chi-square-critical-value-r/#:~:text=If%20the%20test%20statistic%20is%20greater%20than%2019.67514%2C%20then%20the,and%20degrees%20of%20freedom%20%3D%2011.

# transformation

x <- seq(0, 200, by = 0.1)
# A Normal Distribution
norm <- rnorm(x, mean = 10, sd = 2)

plot(norm, log(norm))
plot(norm, log(norm)*10)
plot(norm, (log(norm)*100)/3)

# z transform

Za <- (norm - mean(norm)) / sd(norm)

plot(Za, norm)
plot(Za, log(Za))

#

m <- matrix(nrow = 3, ncol = 4)
m[1,] <- c(157, 65, 181, 10)
m[2,] <- c(126, 82, 142, 46)
m[3,] <- c(58, 45, 60, 28)

colnames(m) <- c('A', 'B', 'C', 'D')
rownames(m) <- c('i', 'ii', 'iii')

Ktotal <- colSums(m)
Rtotal <- rowSums(m)
total <- sum(m)

# modelo de homogenidad ponderado
esp <- matrix(nrow = 2, ncol = 4)
colnames(esp) <- c('A', 'B', 'C', 'D')
rownames(esp) <- c('i', 'ii', 'iii')

esp[,1] <- (Rtotal * Ktotal[1]) / total
esp[,2] <- (Rtotal * Ktotal[2]) / total
esp[,3] <- (Rtotal * Ktotal[3]) / total
esp[,4] <- (Rtotal * Ktotal[4]) / total

View((esp-m)**2 / esp)
sum((esp-m)^2 / esp)

# si no ponderado
obs <- m[,4]
esp <- sum(obs) / length(obs)
sum((esp-obs)**2 / esp)

# que pasaria si quisiera probar de manera independiente cada muestrai, ie. usar el modelo de homogeniedad no ponderado.? valdria?
# ejercicio 2
m <- matrix(nrow = 2, ncol = 5)

m[1,] <- c(172, 71, 44, 70, 37)
m[2,] <- c(39, 19, 12, 28, 18)

colnames(m) <- c('A', 'B', 'C', 'D', 'E')

rownames(m) <- c('M', 'H')

Ktotal <- colSums(m)
Rtotal <- rowSums(m)
total <- sum(m)

# modelo de homogenidad ponderado
esp <- matrix(nrow = 2, ncol = 5)
colnames(esp) <- c('A', 'B', 'C', 'D', 'E')
rownames(esp) <- c('M', 'H')

esp[,1] <- (Rtotal * Ktotal[1]) / total
esp[,2] <- (Rtotal * Ktotal[2]) / total
esp[,3] <- (Rtotal * Ktotal[3]) / total
esp[,4] <- (Rtotal * Ktotal[4]) / total

esp[,5] <- (Rtotal * Ktotal[5]) / total

View((esp-m)**2 / esp)
sum((esp-m)^2 / esp)

# si individual

obs <- m[2,]
esp <- sum(obs) / length(obs)
sum((esp-obs)**2 / esp)

qchisq(p=.05, df= 1, lower.tail=F)


# ejercicio 3
m <- matrix(nrow = 2, ncol = 2)

m[1,] <- c(317, 291)
m[2,] <- c(17, 31)

colnames(m) <- c('bueno', 'malo')

rownames(m) <- c('blanco', 'contaminado')

Ktotal <- colSums(m)
Rtotal <- rowSums(m)
total <- sum(m)

# modelo de homogenidad ponderado
esp <- matrix(nrow = 2, ncol = 2)
colnames(esp) <- c('bueno', 'malo')
rownames(esp) <- c('blanco', 'contaminado')


esp[,1] <- (Rtotal * Ktotal[1]) / total
esp[,2] <- (Rtotal * Ktotal[2]) / total


View((esp-m)**2 / esp)
sum((esp-m)^2 / esp)

# Tarea:
# probando gausanidad
# Sacar histograma de frecuencia
# sacar marca de clase y pologino de frecuencia
# comparar poligono con histograma
# probar gausanidad (usando cdf) c/  bondad de ajuste
# obs (histograma) vs esp (cdf)
# por tanto,
# 0 hacer marca de clase
# requisitos: 
# equidistantes
# al menos una observacion por clase
# 1) hacer histograma
# pasos para hacer un histograma
# mayor numero de clases

x <- c(77.7, 76, 76.9, 74.6, 74.7, 76.5, 79.2, 75.4, 76, 76, 73.9, 77.4, 76.9, 77.3)

# hacer tabla de frecuencias

x <- sort(x)

hist(x)
# Una primera vision de los histogramas permite detectar una bimodalidad en la variable medida

# 1. Recorrido (r)

r <- max(x) - min(x)

# 2. N´umero de intervalos:
k <- sqrt(length(x)) # round(k)
1+log(length(x)) /log(2) # stockes rule
k <- 1 + (3.322*log10(length(x))) # sturges rule
# 3. Amplitud del intervalo (h)

h <- r / k # round()

# 4. Extremos de los intervalos
# Extremos de los intervalos. Para evitar coincidencias se toma un decimal m´as. El primer extremo se toma algo menor que el valor m´ınimo, pero calcul´andolo de forma que el ´ultimo extremo sea algo mayor que el valor m´aximo

# 5. Marcas de clase (numeros de clase)


m <- matrix(nrow = round(k),ncol = 2)
m
m[1,1] <- min(x)
m[1,2] <- min(x) + h
m[2,1] <-  m[1,2]
m[2,2] <-  m[1,2] + h
m[3,1] <-  m[2,2]
m[3,2] <-  m[2,2] + h
m[4,1] <-  m[3,2]
m[4,2] <-  m[3,2] + h
m[5,1] <-  m[4,2]
m[5,2] <-  m[4,2] + h
# using a package
# install.packages('fdth')



library(fdth)


dist <- fdt(x, 
            h = h,
            # k = floor(k),
            start = min(x),
            end = floor(max(x) + 1),
            breaks="Sturges")

hist(x, breaks = "Sturges")

dist <- fdt(x, k = 7)

# plot(dist, type="cfh")
# plot(dist, type="cfp")
# plot(dist, type="rfh")
# plot(dist, type="rfp")

library(tidyverse)
library(ggplot2)

names(dist$table)


#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

library(grid)

# https://stackoverrun.com/es/q/7056907
grid.newpage()

dist$table %>%
  pivot_longer(cols = -(starts_with(c("Class limits")))) %>%
  filter(name %in% c('f', 'cf')) %>%
  mutate(name = factor(name, levels = c("f","cf"))) %>%
  filter(value > 0) -> tbl

tbl %>%
  ggplot(aes(x = `Class limits`, y = value)) +
  geom_col(data= subset(tbl, name %in% 'f'), fill = 'blue') +
  geom_point(data= subset(tbl, name %in% 'cf'), color = 'red') +
  geom_line(data= subset(tbl, name %in% 'cf'),
            aes(group = name, color = 'red') , linetype = 2) +
  theme_bw(base_size = 12) +
  labs(y = "Frequency", color = "", x = "Class limits") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1,
                                   size = 12, hjust = 1),
        legend.position="top") -> p1

p1 + scale_color_discrete(labels = c("Cumulative")) -> p1
# facet_wrap( ~name, scales = "free_y", ncol = 1) -> p1

dist$table %>%
  pivot_longer(cols = -(starts_with(c("Class limits")))) %>%
  filter(!name %in% c('f', 'cf', 'rf')) %>%
  filter(value > 0) -> tbl2

tbl2 %>%
  mutate(name = factor(name, levels = c("rf(%)","cf(%)"))) %>%
  ggplot(aes(x = `Class limits`, y = value)) +
  geom_col(data= subset(tbl2, name %in% 'rf(%)'), fill = 'blue') +
  geom_point(data= subset(tbl2, name %in% 'cf(%)'), color = 'red') +
  geom_line(data= subset(tbl2, name %in% 'cf(%)'),
            aes(color = name, group = name), linetype = 2) +
  theme_bw(base_size = 12) +
  labs(y = "Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1,
                                   size = 12, hjust = 1),
        legend.position="top") -> p2
  # facet_wrap( ~name, scales = "free_y", ncol = 1) -> p2

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))

library(gtable)

g <- gtable::gtable_add_grob(g1, 
                     g2$grobs[[which(g2$layout$name == "panel")]], 
                     pp$t, pp$l, pp$b, pp$l)

# axis tweaks
alab <- g2$grobs[[which(g2$layout$name=="ylab-l")]]
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + 
  unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], 
                     length(g$widths) - 1 )
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], 
                     length(g$widths) - 1 )
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 2, pp$b)
g <- gtable_add_grob(g, alab, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)


obs <- c(74.095, 74.895, 75.695, 
         76.495, 77.295, 78.095, 
         78.895)

z <- (obs - mean(x)) / sd(x)


# obs <- c(7.14, 21.42, 50, 57.14,85.71, 92.86, 100)
# esp <- c(5.59, 15.39, 32.64, 54.78, 75.49, 89.62, 96.64) # cdf of z-classes

obs <- dist$table[,6] # cumulative f % 
es[]
# orx

alpha <- 0.05
qchisq(p=1-alpha, df= length(obs)-1, lower.tail=T)


sum((esp-obs)**2 / esp)

#
f <- v9_fam[1,][-1]
f <- c(t(f))

#

x <- sort(f)

hist(x)
# Una primera vision de los histogramas permite detectar una bimodalidad en la variable medida

# 1. Recorrido (r)

r <- max(x) - min(x)

# 2. N´umero de intervalos:
k <- sqrt(length(x)) # round(k)

# 3. Amplitud del intervalo (h)

h <- r / k # round()


# k-s test

obs <- c(7.14, 21.42, 50, 57.14,85.71, 92.86, 100)
esp <- c(5.59, 15.39, 32.64, 54.78, 75.49, 89.62, 96.64) # cdf of z-classes

# cambiamos a valores < 1
obs <- obs / 100
esp <- esp / 100
obs - esp

# q-q plots

x <- c(77.7, 76, 76.9, 74.6, 74.7, 76.5, 79.2, 75.4, 76, 76, 73.9, 77.4, 76.9, 77.3)
sorted_x <- sort(x)

j <- 1:length(x)

# probabilities by 0.5 (or 0.25, etc)

Pj <- (j - 0.5) / length(x)



z <- qnorm(Pj, lower.tail = T)

# https://www.stat.umn.edu/geyer/old/5101/rlook.html

# to esp model 
# using correlation coefficient of pearson to
# enssure the gaussianity

# rpearson cal
plot(z, sorted_x)
qqnorm(x) # or , by a function
with(x, qqplot(x))
qqline(x)

cor(sorted_x, z)


#

x <- c(77.7, 76, 76.9, 74.6, 74.7, 76.5, 79.2, 75.4, 76, 76, 73.9, 77.4, 76.9, 77.3)



qf(0.95, df1 = 11, df2 = 7)

# 
means <- c(0.79, 0.727)
sd <- c(0.485, 0.635)
n <- c(8, 12)
varPonderada <- ((n[1]-1 * sd[1])^2 + (n[2]-1 * sd[2])^2) / (sum(n)-2)

# ejercicio de lobos 
var <- c(0.019, 0.049)
mean <- c(4.25, 4.19)
n <- c(18, 13)

tcal <- (mean[1]- mean[2]) / sqrt((var[1] / n[1]) + (var[2] / n[2]))

Aglp <- var[1] / n[1]
Bglp <- var[2] / n[2]
Cglp <- Aglp**2 / (n[1]-1)
Dglp <- Bglp**2 / (n[2]-1)

glp <- (Aglp + Bglp)^2 / Cglp + Dglp

glp

# tcri <- 
qt(0.95, glp)

# 2 muestras dependienes ----

wi <- c(13, 15, 30, 10, 15, 37, 25, 28, 39, 10)
wf <- c(15, 22, 35, 9, 16, 42, 21, 32, 42, 11)
Dif <- wf - wi

(mean(Dif))/(sd(Dif)/sqrt(length(Dif)))
qt(p= 0.975, df= 9, lower.tail=F)

# ejercicio de condicion, calificacion y pintura mediante una prueba t para dos muestras dependientes, y b) probar en cada ejercicio valores extremos y gaussianidad de las diferencias.(ie. no la var1 o var2, sino la columna de diferencias)

# 1) consumo de oxigeno en dos condiciones de cultivo distinto.

cond1 <- c(7.8, 9.6, 6.4, 7.5, 8.2)
cond2 <- c(5.6, 7.4, 6.9, 5.3, 8.7)

Dif <- cond2-cond1

n <- length(Dif)

# probamos gausianidad

is_parametric(Dif)
calculate_r_critical_coeff(n)

# continuamos con la desicion de la prueba
(mean(Dif))/(sd(Dif)/sqrt(length(Dif)))
qt(p= 0.975, df= n-1, lower.tail=F)

# No se rechaza Ho
# 2) se entrenaron con un curso a empleados de call-center, verificar si el curso sirvio, servicio (s) antes (1 ) y dspues(2) del curso

s1 <- c(63, 93, 83, 72, 65, 72, 91, 84, 71, 80)
s2 <-  c(78, 92, 91, 80, 69, 85, 99, 82, 81, 87)
Dif <- s2-s1

# probar gaus con q-q ####
# sorteo de datos 
test_normality <- function(x) {
  x_sorted <- sort(x)
  n <- length(x_sorted)
  j <- 1:n
  pj <- (j-0.5) / n 
  ztable <- qnorm(pj, lower.tail = T) # nuestro referente de gaussianidad
  # plot(ztable, x_sorted)
  # qqline(x_sorted)
  
  Cpearson <- cor(ztable, x_sorted, method = 'pearson')
  return(Cpearson)
}

test_normality(Dif)



# 
# sacamos un grado grado equidistante 
# para que los datos no sean distintos
# usando un coefficiente de correlacion
# para medir grado de asociacion lineal

# Ho r_pearson = 0 # hay asosciacion por tanto la distribucion es normal
# Ha r_pearson != 0 # no hay grado de asociacion lineal

cor(ztable, Dif_sorted, method = 'pearson') # r_pearson_cal

# r_cri
calculate_r_critical_coeff <- function(n) {
  # con un alfa de 5%
  
  a <- (0.6118/n)
  b <- (1.3505/n**2)
  c <- (0.1288/sqrt(n))
  
  rcri <- 1.0063 - a + b - c
  
  return(rcri)
  
  # cat("\n", "The critical r is: ", rcri,
  #     "\nIf the correlation coefficient (r) is less than this value, the population is non-normal. If r is greater than this value, the population is normal")
}

calculate_r_critical_coeff(n)

ztrans <- function(x) {
  
  z <- (x - mean(x)) / sd(x)
  # plot(z, x)
  # abline(v =  c(-3, 3), col = "red")
  
  return(z)
}

# Calculate the critical value for a QQ Plot. If the correlation coefficient (r) is less than this value, the population is non-normal. If r is greater than this value, the population is normal.

# https://www.wolframalpha.com/widgets/view.jsp?id=55638a49308f3ff37aca461cabd108da

# hasta aqui probamos gaus ###

(mean(Dif))/(sd(Dif)/sqrt(length(Dif)))
qt(p= 0.975, df= n-1, lower.tail=F)

# Se rechaza Ho

# 3) se prueban dos tipos de pinturas
p1 <- c(892, 904, 775, 435, 946, 853, 780, 695, 825, 750)
p2 <- c(985, 953, 775, 510, 875, 875, 895, 725, 858, 812)
Dif <- p1-p2

n <- length(Dif) - 1 # eliminamos valores ceros

test_normality(Dif)
calculate_r_critical_coeff(n)

(mean(Dif))/(sd(Dif)/sqrt(length(Dif)))
qt(p= 0.975, df= n-1, lower.tail=F)

# Se rechaza Ho

# wilcoxon p/ dependiente ----
# prueba de signos para dos muetras dependientes

cond1 <- c(7.8, 9.6, 6.4, 7.5, 8.2)
cond2 <- c(5.6, 7.4, 6.9, 5.3, 8.7)

Dif <- cond2-cond1
Dif <- abs(Dif)
sort(Dif)
1:length(Dif)
match(Dif, sort(Dif))
rank(Dif, ties.method = 'average')

# tMeth <- eval(formals(rank)$ties.method)
# rx2 <- sapply(tMeth, function(M) rank(Dif, ties.method=M))
# cbind(Dif, rx2)


# Contrastes multiples ----
# 1) test parmetrico t par 2 muestras independientes
# 2) Valorex extremos y  q-q normalidad
# 3) Si hay muestras con valores extremos o no gausianos resolver todos los contrastes con wilcoxon-wilcoxon para 2 muestras independientes.
# hacemos contrastes multiples para saber si hay relaciones

c1 <- c(2, 2.8, 3.3, 3.2, 4.4, 3.6, 1.9, 3.3, 2.8, 1.1)
c2 <- c(3.5, 2.8, 3.2, 3.5, 2.3, 2.4, 2, 1.6)
c3 <- c(3.3, 3.6, 2.6, 3.1, 3.2, 3.3, 2.9, 3.4, 3.2, 3.2)
c4 <- c(3.2, 3.3, 3.2, 2.9, 3.9, 2.5, 2.6, 2.8)
c5 <- c(2.6, 2.6, 2.9, 2, 2, 2.1)
c6 <- c(3.1, 2.9, 3.1, 2.5, 3.4)
c7 <- c(2.6, 2.2, 2.2, 2.5, 1.2, 1.2)
c8 <- c(2.5, 2.4, 3, 1.5, 2.7)

library(tidyverse)

dt <- rbind(
  data.frame(x = c1, g = 'c1'),
  data.frame(x = c2, g = 'c2'),
  data.frame(x = c3, g = 'c3'),
  data.frame(x = c4, g = 'c4'),
  data.frame(x = c5, g = 'c5'),
  data.frame(x = c6, g = 'c6'),
  data.frame(x = c7, g = 'c7'),
  data.frame(x = c8, g = 'c8')
)

a <- c(1.96, 2.24, 1.71, 2.41, 1.62, 1.93)
b <- c(2.11, 2.43, 2.07, 2.71, 2.50, 2.85, 2.88)

dt <- rbind(
  data.frame(x = a, g = 'A'),
  data.frame(x = b, g = 'B'))

dt %>%
  mutate(ranks = rank(x)) %>%
  group_by(g) %>%
  summarise(Rmax = sum(ranks))

# supuest, Ho sumatoria Ra = sumatoria Rb, Ho -> (TA = TB)

# valores criticos revisados en tabla 7, Critical Values of TL and TU for the Wilcoxon Rank Sum Test: Independent Samples.
length(a)
length(b)

# 28 56


# anova ----

dt %>%
  filter(g %in% c("c1", "c2", "c3")) %>%
  group_by(g) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

res.aov <- dt %>%
  filter(g %in% c("c1", "c2", "c3")) %>%
  aov(x ~ g, data = .)

summary.aov(res.aov) # F value es Fcri

qf(0.95, 2, 25) # los grados de libertad de la columna Df del resumen de la prueba de anova 1-via
# la varianza entre tratamientos es igual a la varianza a la critica, pro tanto se dice que los tratamientos son iguales



data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
dt %>%
  group_by(g) %>%
  mutate(sigma = ztrans(x)) %>%
  ggplot(aes(x = g, y = x, color = sigma)) +
  geom_point(size = 7) +
  stat_summary(fun.data = data_summary, color = "grey67") +
  theme_minimal(base_family = "GillSans", base_size = 18) +
  ggsci::scale_color_gsea() +
  labs(x = "Treatment", y = "") +
  geom_hline(yintercept = mean(dt$x), color = "grey67", linetype = "dashed")


# kruskal-wallis p/ mas de dos muestras independientes
dt %>% 
  filter(g %in% c("c1", "c2", "c3")) %>%
  pull(x) %>% rank()

# tarea:
# Analisis anova kruskal -wallis 
# hacer contraste multiple con wilxocon - multiple

d1 <- c(6, 38, 3, 17, 11, 30, 15, 16, 25, 5)
d2 <- c(34, 28, 42, 13, 40, 31, 9, 32, 39, 27)
d3 <- c(13, 35, 19, 4, 29, 0, 7, 33, 18, 24)


dt <- rbind(
  data.frame(x = d1, g = 'd1'),
  data.frame(x = d2, g = 'd2'),
  data.frame(x = d3, g = 'd3'))

dt %>% 
  mutate(rank = rank(x)) %>%
  group_by(g) %>%
  summarise(Ntotal = nrow(dt),
            nk =length(rank),
            a = 12 / (Ntotal * Ntotal+1),
            b = sum(rank)^2/nk,
            c = 3*(Ntotal+1)) %>%
  mutate(Hcal = (a*b)-c)

# ejercicio 2

e1 <- c(96, 128, 83, 61)
e2 <- c(82, 124, 132, 109)
e3 <- c(115, 149, 196, 143)


dt <- rbind(
  data.frame(x = e1, g = 'e1'),
  data.frame(x = e2, g = 'e2'),
  data.frame(x = e3, g = 'e3'))

dt %>% 
  mutate(rank = rank(x)) %>% 
  group_by(g) %>%
  summarise(Ntotal = nrow(dt),
            nk = length(rank),
            a = 12 / (Ntotal * (Ntotal+1)),
            b = (sum(rank)^2)/nk,
            c = 3*(Ntotal+1)) %>%
  mutate(Hcal = (a*sum(b))-c)

# ejercicio 3

g1 <- c(1, 5, 6, 7, 10)
g2 <- c(12, 2, 17, 19, 20)
g3 <- c(8, 9, 3, 11, 13)
g4 <- c(14, 15, 16, 4, 8)

dt <- rbind(
  data.frame(x = g1, g = 'gas1'),
  data.frame(x = g2, g = 'gas2'),
  data.frame(x = g3, g = 'gas3'),
  data.frame(x = g4, g = 'gas4'))

dt %>% 
  mutate(rank = rank(x)) %>% 
  group_by(g) %>%
  summarise(Ntotal = nrow(dt),
            nk = length(rank),
            a = 12 / (Ntotal * (Ntotal+1)),
            b = (sum(rank)^2)/nk,
            c = 3*(Ntotal+1)) %>%
  mutate(Hcal = (a*sum(b))-c) 


qchisq(p=.05, df= k-1, lower.tail=F)

# anova de dos vias
# probemos que no sirve, jeje

a <- c(2.43, 2.48, 2.38, 2.40, 2.35, 2.43, 2.55, 2.41, 2.53, 2.35)
b <- c(2.47, 2.52, 2.44, 2.47, 2.42, 2.49, 2.62, 2.49, 2.60, 2.43)
c <- c(2.47, 2.53, 2.42, 2.46, 2.44, 2.47, 2.64, 2.47, 2.59, 2.44)
d <- c(2.41, 2.48, 2.35, 2.39, 2.32, 2.42, 2.56, 2.39, 2.49, 2.36)
e <- c(2.35, 2.60, 2.47, 2.56, 2.42, 2.44, 2.47, 2.38, 2.52, 2.47)

dt <- rbind(
  data.frame(x = a, g = 'A'),
  data.frame(x = b, g = 'B'),
  data.frame(x = c, g = 'C'),
  data.frame(x = d, g = 'D'),
  data.frame(x = e, g = 'E'))

dt %>%
  mutate(g = as.character(g)) %>%
  # pivot_wider(names_from = g, values_from = x)
  group_by(g) %>%
  summarise(n= length((x)),
            Zmax = max(ztrans(x)),
            Zmin = min(ztrans(x)),
            sigma = sum(abs(ztrans(x) > 3)),
            rpearsonCal = test_normality(x),
            rpearsoncri = calculate_r_critical_coeff(n))
  # summarise(
  #   nk = length(g),
  #           sigma = sum(abs(ztrans(x) > 3)),
  #           rpearsonCal = test_normality(x),
  #           rpearsoncri = calculate_r_critical_coeff(nk),
  #           Ntotal = nrow(dt),
  #           cm = sum(x) / Ntotal,
  #           a = (sum(x)^2)/nk)
  # mutate(Hcal = (a*sum(b))-c) )


# dt %>% pivot_wider(names_from = g, values_from = x)
