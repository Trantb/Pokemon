setwd('C:/Users/HP/Desktop/Study/Pokemon')
df <- read.csv('pokemon.csv', header = T)
t(t(names(df)))
View(df)

#remove Janpanese name
df <- df[,-30]

#against-type
t(t(names(df)))
againstType <- df[,c(2:19)]
df <- df[,-c(2:19)] #remove

#base-stat
t(t(names(df)))
#include total base stat, atk, def, sp.atk, sp.def and speed
baseStat <- df[,c(5, 11, 2, 8, 15:17)]
df <- df[,-c(2, 5, 8, 11, 15:17)]

#Pokemon information
t(t(names(df)))
pokInfo <- df[,c(10, 8, 11, 12, 1)]
df <- df[,-c(1, 8, 10:12)]

#additional stat
t(t(names(df)))
addStat <- df[,c(9, 10, 4, 5, 2, 6, 8, 1, 3, 7)]

#regroup data
data <- cbind(pokInfo, baseStat, addStat, againstType)
rm(addStat, againstType, baseStat, pokInfo, df)
#plot
library(ggplot2)
library(tidyverse)
library(ggrepel)
t <- ggplot(data, aes(x = attack, y = sp_attack)) + 
  geom_point(aes(color = factor(is_legendary)))
t

#psychic
psychic <- data %>%
  filter(type1 == "psychic")
p <- data%>%
  filter(type2 == "psychic")
psychic <- rbind(psychic, p)
rm(p)
ggplot(psychic, aes(x = sp_attack, y = speed)) +
  geom_point(aes(color = factor(generation), 
                 shape = factor(is_legendary)), 
             size = 5) +
  geom_text_repel(aes(label = name), size = 3, nudge_x = 0, nudge_y = -2.5,
                  max.overlaps = 20) + 
  geom_hline(yintercept = 100, linetype = 'dashed') +  
  geom_vline(xintercept = 100, linetype = 'dashed') +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
  theme(panel.background = element_rect(fill = 'grey90'))

#Alakazam
alakazam <- psychic %>%
  filter(name == "Alakazam") %>%
  select(hp, attack, defense, sp_attack, sp_defense, speed)
alakazam <- t(alakazam)
alakazam <- data.frame(alakazam)
alakazam <- rownames_to_column(alakazam, var = 'base_stat') %>%
  rename(alakazam, value = alakazam) %>%
  arrange(value) %>%
  mutate(base_stat = factor(base_stat, levels = c('hp', 'attack', 'defense', 
                    'sp_attack', 'sp_defense', 'speed')))


ggplot(alakazam, aes(x = fct_rev(as.factor(base_stat)), 
                     fill = as.factor(base_stat),
                     y = value)) +
  geom_bar(stat = 'identity') +
  scale_fill_hue(c = 40) + 
  theme(legend.position = 'none') + coord_flip() + 
  geom_label(aes(label = value), size = 5.5) + 
  labs(x = NULL, y = NULL) +
  theme_classic() + guides(fill = 'none')

#against
weakness <- data %>%
  filter(name == 'Alakazam') %>%
  select(against_bug, against_dark, against_dragon, against_dragon,
         against_electric, against_fairy, against_fight, against_fire,
         against_flying, against_ghost, against_grass, against_ground,
         against_ice, against_normal, against_poison, against_psychic,
         against_rock, against_steel, against_water)
weakness <- t(weakness)
weakness <- data.frame(weakness)
weakness <- rename(weakness, effectiveness = weakness)
weakness <- rownames_to_column(weakness, var = 'against')

plot_table <- function(d, colors, marginColor,main="", text.cex=1.0)
{
  plot(c(-1,ncol(d)),c(0,nrow(d)+1), type="n", xaxt="n", yaxt="n", xlab="",ylab="",main=main, bty="n")
  
  for (c in 1:ncol(d)) {
    rect(c-1, nrow(d), c, nrow(d) + 1, col=marginColor)
    text(c-.5,nrow(d) +.5,colnames(d)[c], cex=text.cex)
  }
  
  for (r in 1:nrow(d)) {
    rect(-1, r-1, 0, r, col=marginColor)
    text(-.5, r-.5,rownames(d)[nrow(d) - r + 1], cex=text.cex)
  }
  
  for (r in 1:nrow(d))
    for (c in 1:ncol(d)) {
      rect(c-1, r-1, c, r, col=colors[nrow(d) - r + 1,c])
      text(c-.5,r-.5,d[nrow(d) - r + 1,c], cex=text.cex)
    }
}

d <- matrix(nrow = 18, ncol = 1)

colnames(d) <- c('Effectiveness')
rownames(d) <- c('Bug', 'Dark', 'Dragon', 'Electrice', 'Fairy',
                 'Fight', 'Fire', 'Flying', 'Ghost', 'Grass',
                 'Ground', 'Ice', 'Normal', 'Poison', 'Psychic',
                 'Rock', 'Steel', 'Water')
d[,1] <- weakness[,2]


colors <- matrix(sapply(d, function(x) ifelse(x < 1, 'Green', 
                                              ifelse(x == 1, "White", "Red"))),
                 ncol = ncol(d))
par(mar=c(0,0,1,0))
plot_table(d, colors, 'gray90', main = 'Against', text.cex = 0.8)



##Fairy
fairy <- data %>%
  filter(type1 == "fairy")
p <- data%>%
  filter(type2 == "fairy")
fairy <- rbind(fairy, p)
rm(p)
ggplot(psychic, aes(x = sp_attack, y = speed)) +
  geom_point(aes(color = factor(generation), 
                 shape = factor(is_legendary)), 
             size = 5) +
  geom_text_repel(aes(label = name), size = 3, nudge_x = 0, nudge_y = -2.5,
                  max.overlaps = 20) + 
  geom_hline(yintercept = 100, linetype = 'dashed') +  
  geom_vline(xintercept = 100, linetype = 'dashed') +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
  theme(panel.background = element_rect(fill = 'grey90'))



g <- ggplot(fairy, aes(defense, sp_defense)) + 
        geom_hline(yintercept = 100, linetype = 'dashed') +
        geom_vline(xintercept = 100, linetype = 'dashed')
g
g + geom_point(aes(size = hp, color = factor(generation))) +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_continuous(breaks = seq(0, 160, 20)) +
  scale_size(range = c(1, 10)) +
  geom_text_repel(aes(label = name), size = 3, nudge_x = 0, nudge_y = -2.5) +
  theme(legend.position = c(.95, .4),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5))
  
