## ----results = 'hide', echo = TRUE, message = FALSE----------------------
library(Lahman)
library(dplyr)
library(ggplot2)


data(Batting)
data(Master)

Master <- Master %>% 
  select(playerID, nameFirst, nameLast) 

Batting <- Batting %>% 
  filter(playerID %in% c('sosasa01', 'rosepe01')) %>% 
  merge(Master, all.x=TRUE) %>% 
  group_by(nameFirst, nameLast) %>% 
  summarise(Hits = sum(H)
            , AtBats = sum(AB)) %>% 
  ungroup() %>% 
  mutate(Name = paste(nameFirst, nameLast)
         , Misses = AtBats - Hits
         , BattingAverage = Hits / AtBats) %>% 
  select(-nameFirst, -nameLast)

## ----echo = TRUE---------------------------------------------------------

dfBeta <- data.frame(x = seq(0, 1, length.out = 1000))

for (i in seq_len(nrow(Batting))){
  dfBeta[, Batting$Name[i]] <- dbeta(dfBeta$x, Batting$Hits[i] + 1, Batting$Misses[i] + 1)
}

dfBeta <- dfBeta %>% 
  tidyr::gather("Batter", "Density", -x)

plt <- ggplot(dfBeta, aes(x = x, y = Density, color = Batter)) + geom_line()
plt

