install.packages("tidyverse")
library(tidyverse)

bikes <- read_csv("bikes.csv", col_types = "Dffffddddd")
view(bikes) 
summary(bikes)

install.packages("cowplot")
library(cowplot)
g1 <- ggplot(bikes, aes(x= temperature, y=rentals, alpha=rentals)) +
  geom_point(color="blue")
g2 <- ggplot(bikes, aes(x= humidity, y=rentals, alpha=rentals)) +
  geom_point(color="red")
g3 <- ggplot(bikes, aes(x= windspeed, y=rentals, alpha=rentals)) +
  geom_point(color="black")
plot_grid(g1, g2, g3, labels="AUTO")

meu_perso <- cov(bikes$temperature, bikes$rentals)/(sd(bikes$temperature)*sd(bikes$rentals))

meu_perso

pearson <- cor(bikes$temperature, bikes$rentals, method= "pearson")
pearson

g5 <- ggplot(bikes, aes(y = rentals, x = realfeel)) + geom_point() + stat_smooth(method = "lm")
g5


install.packages("corrplot")
library(corrplot)

psn <- bikes %>% select(-date,-season,-holiday,-weekday, -weather) %>% cor()
psn
corrplot(psn, type="upper", tl.cex = 0.5)

loca_rl <- lm(data=bikes, rentals~realfeel)
loca_rl
#locacoes = 743,75 + (63,15 * realfeel)

loca_rlm <- lm(data=bikes, rentals~realfeel + humidity + windspeed +season + holiday + weekday + weather)
loca_rlm
summary(loca_rlm)
