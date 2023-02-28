install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

berkeley <- read.csv2('Berkeley.csv', header=TRUE, sep=';')
View(berkeley)
berkeley %>% select(Genero, Admitidos)
genero = group_by(berkeley, Genero)
genero
summarise(genero, Proporcao=100*sum(Admitidos)/sum(Candidado))

berkeley2 <- berkeley %>% mutate(Rejeitados = Candidado - Admitidos)
View(berkeley2)
berkeley2 %>% group_by(Genero) %>% summarise(Adm = (100*sum(Admitidos)/sum(Candidado)))

berkeley2 %>% mutate(Departamento = str_c("Dep ", Departamento)) %>%
  pivot_longer(cols = Admitidos:Candidado, names_to = 'Aceitos', values_to = 'n') %>% 
  ggplot(aes(x = Genero, y = n, fill = Aceitos)) +
  geom_col(position = 'dodge') +
  #scale_fill_viridis_d(NULL, option = 'A', end = 0.6) +
  xlab(NULL) + 
  theme(panel.grid = element_blank()) +
  facet_wrap(~Departamento)
berkeley2
