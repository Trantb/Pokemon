##Calculating battle damage


battleDamage <- function(level, attack, power, defense, STAB, type){
  STAB <- ifelse(STAB == T, 1.5, 1)
  type <- ifelse(type == 'super effective', 2, ifelse(type == 'normal', 1, ifelse(type == 'not very effective', 0.5, 0)))
  crit <- sample(1:2, 1, replace = T, prob = c(0.9, 0.1))
  random <- sample(217:255, 1)
  damage <- ((((2*level*crit/5+2)*attack*power)/defense/50) +2)*STAB*type*random/255
  print(damage)
}

battleDamage(50, 200, 90, 150, F, 'super effective')


iv_generator <- function(){
  hp <- sample(0:31, 1)
  attack <- sample(0:31, 1)
  defense <- sample(0:31, 1)
  sp_attack <- sample(0:31, 1)
  sp_defense <- sample(0:31, 1)
  speed <- sample(0:31, 1)
}

iv_generator()

setwd("C:/Users/HP/Desktop/Study/Pokemon")


ivStat <- matrix(iv_generator())

pokemon <- function(pokemonName, level, evStat){
  library(tidyverse)
  for (i in 1:length(pokemonName)){ 
  myPokemon <- levels(factor(pokemonName))[i]
  baseStat <- data %>% filter(name == myPokemon) %>% select(hp, attack, defense, sp_attack, sp_defense, speed)
  rownames(baseStat)[1] <- myPokemon
  }
  ivStat <- matrix(iv_generator(), ncol = 6, nrow = 1)
#Calculation  
  baseStat[1] <- round(((2*baseStat[1]+ivStat[1]+round(evStat[1]/4,0))*level)/100,0)
  baseStat[2] <- round(((2*baseStat[2]+ivStat[2]+round(evStat[2]/4,0))*level)/100 + 5,0)
  baseStat[3] <- round(((2*baseStat[3]+ivStat[3]+round(evStat[3]/4,0))*level)/100 + 5,0)
  baseStat[4] <- round(((2*baseStat[4]+ivStat[4]+round(evStat[4]/4,0))*level)/100 + 5,0)
  baseStat[5] <- round(((2*baseStat[5]+ivStat[5]+round(evStat[5]/4,0))*level)/100 + 5,0)
  baseStat[6] <- round(((2*baseStat[6]+ivStat[6]+round(evStat[6]/4,0))*level)/100 + 5,0)
  print(baseStat)
}


pokemon('Alakazam', 60, c(4,0,0,252,0,252))




typeAgainst <- function(typeAttack, typeDefense){
  damage <- matrix(c(1,1), nrow = 2, ncol = 1)
  for (i in 1:length(typeDefense)){
  type_i <- levels(factor(typeDefense))[i]
  damage[i,1] <- d[d = typeAttack, d = type_i]
  }
  damage <- damage[1,1]*damage[2,1]
  if (damage == 0){
    print("Move has no effect")
  } else if (damage < 1){
    print("It's not very effective!")
  } else if (damage >= 2){
    print("It's super effective!")
  }
}
typeAgainst('Psychic', c('Poison', 'Grass'))


pokemonAgainst <- function(pokemonAttack, pokemonDefense){
  library(tidyverse)
  library(stringr)
  typeAttack <- data%>%filter(name == pokemonAttack)%>%select(type1)
  typeAttack <- str_to_title(typeAttack)
  typeDefense <- data%>%filter(name == pokemonDefense)%>%select(type1, type2)
  damage <- matrix(c(1,1), nrow = 2, ncol = 1)
  for (i in 1:length(typeDefense)){
  type_i <- str_to_title(typeDefense[i])
  damage[i,1] <- d[d = typeAttack, d = type_i]
  }
  damage <- damage[1,1]*damage[2,1]
  if (damage == 0){
    print("Move has no effect")
  } else if (damage < 1){
    print("It's not very effective!")
  } else if (damage >= 2){
    print("It's super effective!")
  }
}

pokemonAgainst('Venusaur','Swampert')
rm(typeDefense, typeAttack)


library(ggrepel)
allMove <- read.csv('All_Moves.csv', header = T)
##???? :?
grassMove <- allMove %>% filter(Type == "Dragon" & Category %in% c('Physical','Special')) %>% select(Name, Category, Power, Acc)
grassMove$Power <- as.integer(grassMove$Power)
grassMove$Acc <- as.integer(grassMove$Acc)
g <- ggplot(grassMove, aes(Power, Acc))
g + geom_point(aes(color = factor(Category)), size = 4) +
  geom_text_repel(aes(label = Name), size = 3, nudge_x = 0, nudge_y = -.3, max.overlaps = 7)


