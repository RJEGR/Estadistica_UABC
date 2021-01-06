# 
# Functions
#

wilcox_wilxon_independent <- function(dt, p = 0.05) {
  
  Ntotal <- nrow(dt)
  k <- length(unique(dt$g))
  
  dt %>% 
    mutate(ranks = rank(x)) %>%
    group_by(g) %>%
    summarise(Tk = sum(ranks),
              nk = length(g)) %>%
    mutate(b = (Tk)^2/nk) %>%
    pull(b) %>% sum() -> b
  
  a <- 12 / (Ntotal * (Ntotal+1))
  c <- 3*(Ntotal+1)
  
  Hcal <- (a*b)-c
  Hcri <- qchisq(p = 0.05, df= k-1, lower.tail=F)
  
  data.frame(Hcal, Hcri, k)
}


library(tidyverse)

# 1
dataset <- read.csv('~/Documents/DOCTORADO/vasijas_romanas.csv', 
                    stringsAsFactors = F) %>%   as_tibble() 

dataset %>%
  mutate(id = ifelse(Site %in% "Ashley Rails", "A", 
                     ifelse(Site %in% "Caldicot", "C",
                            ifelse(Site %in% "Island Thorns", "I", "L")))) -> dataset

table(dataset$Site) # El sitio C tiene un n = 2, por tanto, manejamos como independientes las muetras , ya que los muetreos son desiquilibrados.
table(dataset$id)
# son independientes

dataset %>%
  select(-id) %>%
  pivot_longer(-Site, names_to = 'g', values_to = 'x') %>%
  mutate(g = Site) %>%
  is_parametric()

# by location


dataset %>%
  select(-Site) %>%
  pivot_longer(-id, names_to = 'g', values_to = 'x') -> dt

dt %>%
  group_by(g, id) %>%
  summarise(nk = length(g)) %>%
  arrange(id)

dt %>%
  filter(id %in% c('A')) %>% 
  wilcox_wilxon_independent()

dt %>%
  filter(id %in% c('C')) %>%
  wilcox_wilxon_independent()

dt %>%
  filter(id %in% c('I')) %>%
  wilcox_wilxon_independent()

dt %>%
  filter(id %in% c('L')) %>%
  wilcox_wilxon_independent()

# by oxidate
names(dataset)

dt %>%
  filter(g %in% c('Al')) %>% 
  mutate(g = id) %>%
  wilcox_wilxon_independent()

dt %>%
  filter(g %in% c('Fe')) %>% 
  mutate(g = id) %>%
  wilcox_wilxon_independent()

dt %>%
  filter(g %in% c('Mg')) %>% 
  mutate(g = id) %>%
  wilcox_wilxon_independent()

# exceptuando Caldicot hacemos prueba W-W para 2 muestras independientes, contratando
table(dataset$id)

dataset %>%
  select(-Site) %>%
  pivot_longer(-id, names_to = 'g', values_to = 'x') -> dt

# A vs I 
dt %>%
  filter(id %in% c('A', 'I')) %>%
  mutate(ranks = rank(x)) %>% 
  group_by(id) %>%
  summarise(Tranks = sum(ranks), n = length(g)/5)

# A vs L
dt %>%
  filter(id %in% c('A', 'L')) %>%
  mutate(ranks = rank(x)) %>%
  group_by(id) %>%
  summarise(Tranks = sum(ranks), n = length(g)/5)
# L vs I

dt %>%
  filter(id %in% c('L', 'I')) %>%
  mutate(ranks = rank(x)) %>%
  group_by(id) %>%
  summarise(Tranks = sum(ranks), n = length(g)/5)

# supuest, Ho sumatoria Ra = sumatoria Rb, Ho -> (TA = TB)

# valores criticos revisados en tabla 7, Critical Values of TL and TU for the Wilcoxon Rank Sum Test: Independent Samples.

dt %>%
  ggplot(aes(y = x, x = g )) +
  geom_boxplot() +
  facet_grid(Site ~.)

library(RColorBrewer)

# Ashley Rails
# Caldicot
# Island Thorns
# Llanederyn

# https://rlbarter.github.io/superheat-examples/Word2Vec/

dataset %>%
  select_if(is.double) %>%
  t() %>%
  superheat::superheat(membership.cols = dataset$Site, 
                       
                       pretty.order.rows = T,
                       pretty.order.cols = T,
                      
                       heat.pal = brewer.pal(5, "BuPu"),
                       grid.vline.col = "white",
                       grid.hline.col = "white",
                       
                       # yr = colSums(dataset[-6]),
                       # yr.plot.type = "scatterline",
                       # yr.axis.name = "Composición\nde óxidos\n(cumulativa)",
                       # yt = rowSums(dataset[-6]),
                       # yt.plot.type = "bar",
                       # yt.axis.name = "Composición\nde óxidos\nPor región",
                       # yt.plot.size = 0.2,
                       # yt.point.size = 4,
                       # yt.line.size = 2,
                       
                       yr.plot.size = 0.5,
                       
                       # bottom labels
                       bottom.label.size = 0.05,
                       bottom.label.col = "white",
                       bottom.label.text.angle = 0,
                       bottom.label.text.size = 2.5,
                       bottom.label.text.alignment = "center",
                       
                       left.label.col = "white",
                       left.label.text.size = 3.5,
                       
                       print.plot = T)


# Homocelasticidad


# no existen valores extremos.
# solo para Al y Ca, los datos son Gaussianos, 
# por tanto procedemos a prueba no parametrica independiente!!!!
# 2 ----

dataset2 <- read.csv('~/Documents/DOCTORADO/beisbol_dataset.csv') %>%   as_tibble() 

dataset2 %>%
  select(year, amer.home, amer.away, natl.home, natl.away) %>%
  pivot_longer(-year, names_to = 'g', values_to = 'x') %>%
  is_parametric()


# para liga nacional, usamos prueba T parametrica

dataset2 %>%
  select(natl.home, natl.away) %>%
  # filter(Dataset %in% 'accident') %>%
  mutate(x = natl.home-natl.away, g = 'National') %>%
  group_by(g) %>%
  summarise(n = length(g),
            Tcal = (mean(x))/(sd(x)/sqrt(length(x))),
            Tcri = qt(p = 0.975, df= n-1, lower.tail=T),
            mean_home = mean(natl.home),
            mean_away = mean(natl.away))

# para la liga americana

dataset2 %>%
  select(AmerTeam, amer.home, amer.away) %>%
  pivot_longer(-AmerTeam, names_to = 'g', values_to = 'x') %>%
  # filter(g %in% c("amer.home", "amer.away")) %>%
  wilcox_wilxon_independent()


dataset2 %>%
  select(amer.home, amer.away) %>%
  mutate(Dif = amer.home-amer.away) %>%
  mutate(ranks = rank(abs(Dif)),
         group = ifelse(Dif < 0, 'Tneg', 'Tpos')) %>%
  group_by(group) %>%
  summarise(Tcal = sum(ranks), 
            n = length(ranks))

qsignrank(0.975, 71, lower.tail = F)

dataset2 %>%
  select(amer.home, amer.away) %>%
  apply(., 2, sum)

# comparamos anova para ambas ligas

dt %>%
  filter(g %in% c('amer.home', 'natl.home')) %>%
  wilcox_wilxon_independent() 

dt %>%
  filter(g %in% c('amer.home', 'natl.home')) %>%
  mutate(ranks = rank(x)) %>% 
  group_by(g) %>%
  summarise(Tranks = sum(ranks), n = length(g))

dt %>%
  ggplot(aes(x = g, y = x)) +
  geom_boxplot() +
  labs(x = 'Liga', y = '%') +
  theme_bw(base_size = 14, base_family = "GillSans")

dataset2 %>%
  select(amer.home, natl.home) %>%
  apply(., 2, sum)
