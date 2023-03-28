#install.packages("tidyverse")
library(tidyverse)
veiculos <- read_csv(file = 'vehicles.csv')
glimpse(veiculos)
summary(veiculos)
select(veiculos, drive)
select(veiculos, year, drive)
table(select(veiculos, class))
prop.table(table(select(veiculos, class)))
prop.table(table(select(veiculos, class)))*100
veiculos %>% select(class) %>% table() %>% prop.table()
veiculos %>% filter(year > 2000) %>% select (citympg) %>% summary()

veiculos %>% ggplot() +
  geom_boxplot(mapping = aes(x = class, y = co2emissions), fill = "red") + 
  labs(title = "Class x C02", x = "class", y = "co2emissions")

veiculos %>% ggplot() +
  geom_point(mapping = aes(x = citympg, y = co2emissions), color = 'blue')

veiculos %>% ggplot() +
  geom_histogram(mapping = aes(x = co2emissions))

veiculos %>% ggplot() +
  geom_histogram(mapping = aes(x = co2emissions), bins= 50, fill = "yellow", color = "black")

veiculos %>%   ggplot() +
  geom_bar(mapping = aes(x =year, fill = drive), color = "black") +
  labs(title = "Drive por ano", x = "Ano", y = "Carros") 

veiculos %>% filter(co2emissions < 100) %>% select(make, model,year, class)

summary(veiculos)
veiculos %>% 
  select (citympg, displacement, highwaympg) %>% 
  summary()

veiculosAjustados <- veiculos %>% 
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm=TRUE),  citympg))
summary(veiculosAjustados)

veiculosAjustados <- veiculosAjustados %>% 
  mutate(displacement= ifelse(is.na(displacement), mean(displacement, na.rm=TRUE),  displacement))
summary(veiculosAjustados)

veiculosAjustados <- veiculosAjustados %>% 
  mutate(highwaympg= ifelse(is.na(highwaympg), mean(highwaympg, na.rm=TRUE),  highwaympg))
summary(veiculosAjustados)

veiculosImputados <- veiculos %>% drop_na()
summary(veiculosImputados)
glimpse(veiculosImputados)

veiculosImputados %>% filter(drive == '2-Wheel Drive') %>% select(make, model,year, drive, drive2) %>% print(n=500)

veiculoAjustados <- veiculosImputados %>% mutate(drive2 = recode(drive, "2-Wheel Drive" = "Frot-Wheel Drive")) %>% mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive"))

veiculoAjustados %>% filter(drive == '4-Wheel Drive') %>% select(make, model,year, drive, drive2) %>% print(n=500)
head(veiculoAjustados, n= 10)
summary(veiculoAjustados)

veiculoAjustados %>% ggplot() + 
  geom_bar(mapping = aes(x = year, fill = drive2), color = "pink", position = "dodge") + 
  labs(title = "Drive por ano", x = "Ano", y = "Carros")

veiculoAjustados %>% select(co2emissions) %>% summary()
veiculosNormalizados <- veiculosAjustados %>% select(co2emissions) %>% 
  mutate(co2emissionsZ-mean(co2emissions)) / sd(co2emissions))

veiculoAjustados %>% select(co2emissions) %>% summary

veiculosNormalizados <- veiculosNormalizados %>% 
  mutate(co2emissionsMinMax = ((co2emissions - min(co2emissions))
                               / (max(co2emissions)-min(co2emissions))) *
           (1-(1)) + (-1))

summary(veiculoAjustados)

veiculoAjustados <- veiculosNormalizados %>% mutate(co2emissionsLog = log10(co2emissions))


