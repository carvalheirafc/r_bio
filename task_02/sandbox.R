
install.packages('gtools')
library(dplyr)
library(gtools)
# conjunto de todas as proteínas de drosophila
# e suas anotações
data <- read.csv("drosophila.tab", sep="\t", na.strings="")
df <- tbl_df(data)
head(df)


################################################################################
                             # Roteiro 1 #
################################################################################
# 1
# A) Espaço amostra da proteinas 
a <-length(df$Protein.names)
a
# B) Protein Be Transmembrane
b <- length(dplyr::filter(df, is.na(Transmembrane) == F)$Protein.names) / length(df$Protein.names)
b
# C) Length > 1500
c <- length(dplyr::filter(df, Length > 1500)$Protein.names) / length(df$Protein.names)
c
# D) 
d <- length(dplyr::filter(df, is.na(Transmembrane) == F | Length > 1500)$Protein.names) / length(df$Protein.names)
d
# E) 
e <- length(dplyr::filter(df, is.na(Transmembrane) == F & Length > 1500)$Protein.names) / length(df$Protein.names)
e
# F)
f <- length(dplyr::filter(df, is.na(Transmembrane) == F & Length < 1500)$Protein.names) / length(df$Protein.names)
f
# G)
g <- length(dplyr::filter(df, is.na(Transmembrane) == T & Length > 1500)$Protein.names) / length(df$Protein.names)
g
# H)
h <- length(dplyr::filter(df, is.na(Transmembrane) == T | Length < 1500)$Protein.names) / length(df$Protein.names)
h

# 2
dplyr::filter(df, Length != 21)


gtools::combinations(n=4,r=3,v=c("A","U","C","G"), repeats.allowed=T)
gtools::permutations(n=4,r=3,v=c("A","U","C","G"), repeats.allowed=T)


# 3

################################################################################
# Roteiro 2 #
################################################################################
x <- 0:100
desease <- 1/20
# A)
dbinom(1, size=100, desease)

# B)
pbinom(10, size=100, desease)

#C)
sum(dbinom(5:10, size=100, desease))

#D)

#e)
sum(dbinom(0:95, 100, desease))
################################################################################
# Roteiro 3 #
################################################################################
library(tidyverse)

flu <- read.csv('data_flu.csv')
ggplot(flu, aes(sample=age)) + 
  geom_qq() + 
  geom_qq_line()

ggplot(flu, aes(x=age)) + 
  geom_histogram(aes(y=..density..))

