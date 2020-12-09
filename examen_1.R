library(tidyverse)

dataset <- read.csv('~/Documents/DOCTORADO/scanlon_et_al.csv') %>%   as_tibble() 

dataset %>% 
  mutate(x = X6th-X13th, g = Dataset) %>%
  is_parametric()

# 1) entre viernes 6 vs viernes 13, para los datos de shopping, usamos no parametrica > wilcoxon-wilcoxon-wilcoxon p/ 2 muestras dependientes (prueba de signos)
# 2) para el resto de los dataset, aplicamos prueba t parametrica 

dataset %>%
  filter(Dataset %in% 'shopping') %>%
  select(Dataset, X6th, X13th) %>%
  mutate(Dif = X6th-X13th) %>%
  # mutate(Dabs = abs(Dif)) %>% pull(Dabs) -> ff
         # RR = sort())
  mutate(ranks = rank(abs(Dif)),
         group = ifelse(Dif < 0, 'Tneg', 'Tpos')) %>%
  group_by(group) %>%
  summarise(Tcal = sum(ranks), 
            n = length(ranks))

# 2
dataset %>%
  filter(!Dataset %in% 'shopping') %>%
  # filter(Dataset %in% 'accident') %>%
  mutate(x = X6th-X13th, g = Dataset) %>%
  group_by(Dataset) %>%
  summarise(n = length(Dataset),
            Tcal = (mean(x))/(sd(x)/sqrt(length(x))),
            Tcri = qt(p = 0.975, df= n-1, lower.tail=T),
            mean_X6th = mean(X6th),
            mean_X13th = mean(X13th))


# 2 ----
# Existen diferencias entre las dos estaciones (A y B) para: -la profundidad de secchi y para las diatomeas pennadas?

# kd = 	m-1
# zecci = m
# diatomeas  =	células/litro

# A menos turbia y B más turbia

dataset2 <- read.csv('~/Documents/DOCTORADO/secchi_diatoms.csv') %>% as_tibble()


# 1) Existen diferencias en las profundidad de secchi, entre A y B?
# lo que se interesa es la diferencia entre a y b, ignoramos la variable de dia

# gaussianity in difference
dataset2 %>% 
  mutate(x = zsecA-zsecB, g = 'Diff') %>%
  select(x, g) %>%
  is_parametric()



dataset2 %>%
  mutate(x = zsecA-zsecB, g = 'Diff') %>%
  summarise(Tcal = (mean(x))/(sd(x)/sqrt(length(x))))
# Tcal = (mean(x))/(sd(x)/sqrt(length(x)))
n <- nrow(dataset2)

alfa <- 0.5
qt(p = 0.975, df= n-1, lower.tail=T)


# 
# 2) Existen diferencias en la [diatomeas] entre A y B?
# tienes datos de diatomeas por los miles 

dataset2 %>% 
  mutate(x = DPennA-DPennB, g = 'Diff') %>%
  select(x, g) %>%
  is_parametric()

dataset2 %>%
  mutate(x = DPennA-DPennB, g = 'Diff') %>%
  summarise(mean(x))
# Tcal = (mean(x))/(sd(x)/sqrt(length(x)))
n <- nrow(dataset2)

alfa <- 0.5
qt(p = 0.975, df= n-1, lower.tail=T)
  
