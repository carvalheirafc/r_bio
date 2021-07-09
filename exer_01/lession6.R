# Import Section 
#install.packages('janitor')
library(tidyverse)
library(janitor)


# 1 

hero_info <- read_csv(file="heroes_information.csv", na=c("", "-", "NA"))
hero_powers <- read_csv(file="super_hero_powers.csv", na=c("", "-", "NA"))

glimpse(hero_info)
glimpse(hero_powers)

# 2 

janitor::clean_names(dat = hero_info)
janitor::clean_names(dat = hero_powers)

# 3 
hero_info <- subset(hero_info, select= -X1)
#glimpse(hero_info)

#4 
hero_powers <- hero_powers %>% as.data.frame(sapply(hero_powers[, -1], as.logical))

#5
length(unique(hero_info$Publisher))
hero_info <- hero_info %>% mutate(Publisher = case_when(Publisher == 'Marvel Comics' ~ 'Marvel', Publisher == 'DC Comics' ~ 'DC'))

#6
as.data.frame(table(hero_info$Publisher, hero_info$Race))

#7
eyes <- as.data.frame(table(hero_info$`Eye color`, hero_info$Gender))
eyes[order(eyes$Freq, decreasing=TRUE), ]

#8
power_stats <- hero_powers %>% summarise_if(is.logical, mean, na.rm = TRUE) * 100


#9
tidy_hero_powers <- gather(hero_powers, 'poder', 'possui_poder', -hero_names)
tidy_hero_powers_stats <- aggregate(. ~ poder, tidy_hero_powers[-1], mean)

#10
colnames(hero_powers)[1] <- 'name'
hero <- hero_info %>% inner_join(hero_powers)

#11
length(filter(hero, Telepathy == TRUE)) * 100 / nrow(hero)

#12
filter(hero, Flight == TRUE) %>% select(c('name', 'Publisher', 'Flight', 'Weight')) %>% arrange(desc(Weight))

#13
write_csv(hero, 'hero.csv')

