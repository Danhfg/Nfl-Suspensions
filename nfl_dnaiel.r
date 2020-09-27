install.packages("fivethirtyeight")

install.packages("ggplot2")

library(fivethirtyeight)

library(ggplot2)

head(nfl_suspensions)

help(nfl_suspensions)

ns = nfl_suspensions

ns$games = as.integer(ns$games)

#removendo NAN de games
#ns = ns[complete.cases(ns[ , 3]),]

#Comparando todas as categorias de suspens�o
ggplot(ns, aes(games, group = category)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentual") +
  xlab("Number of games suspened") +
  xlim(0,20)+
  facet_wrap(~category, ncol=2, nrow=3)+
  labs(fill="Games") +
  title("Comparando")

# Comparando todas as descri��es
#ggplot(ns, aes(games, group = description)) + 
#  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#  scale_y_continuous(labels=scales::percent) +
#  ylab("Share of violation") +
#  xlim(0,20)+
#  facet_wrap(~description, ncol=4, nrow=11)
#  #facet_grid(~description)

# Contabilizando Descri��es da suspens�o 
as.data.frame(table(nfl_suspensions["description"]))


# Selecionando os dados das duas descri��es com mais suspens�es 
nsgd <- ns[which(ns$description =='Marijuana-related' | ns$description == 'Domestic violence'),c('games', 'description')]

# Comparando o tempo de suspens�o das duas descri��es com mais suspens�es 
ggplot(nsgd, aes(games, group = description)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentual") +
  xlab("Number of games suspened") +
  xlim(0,20)+
  facet_wrap(~description, ncol=2, nrow=1)+
  labs(fill="Games") 
  