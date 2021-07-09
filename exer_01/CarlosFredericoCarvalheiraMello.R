############# Lession 6 ################

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
table(hero_powers$Agility)

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




############# Lession 12 ################

library(tidyverse)
library(ggplot2)

# 1
hero <- read.csv('hero.csv')
hero_selected <- as.data.frame(dplyr::filter(hero, Height > 0.0 & Weight > 0.0))

# 2 
ggplot(hero_selected, aes(x=Height)) + 
  geom_histogram(binwidth = 42, color='darkblue', fill='lightblue')

#3
ggplot(hero_selected, aes(x=Weight)) + 
  geom_histogram() + 
  facet_grid(hero_selected$Publisher ~ ., scales = "free")

#4
require(forcats)
ggplot(hero_selected, aes(fct_infreq(Publisher))) + 
  geom_bar(stat='count') + 
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_minimal()

# 5
ggplot(hero_selected, aes(Alignment, fill=Publisher)) + 
  geom_bar(position=position_dodge())

#6
ggplot(hero_selected, aes(Alignment, fill=Publisher)) + 
  geom_bar(position=position_fill())

#7
columns_list <- c('Gender', 'Eye.color', 'Race', 'Hair.color', 'Height', 'Skin.color', 'Alignment', 'Weight')
hero <- hero %>% select(-columns_list)
hero_agg <- aggregate(sapply(hero[, -2], as.numeric), by = list(hero$name, hero$Publisher), FUN=sum)
hero_agg <- gather(select(hero_agg, -name), Power, Total, -Group.1, -Group.2)
hero_agg <- aggregate(hero_agg[, -1:-3], by=list(hero_agg$Group.1, hero_agg$Group.2), FUN=sum)
hero_agg <- rename(hero_agg, Total = x, Name = Group.1, Publisher = Group.2)
hero_agg <- hero_agg %>% arrange(desc(Total))


#8
head_hero <-rbind(head(filter(hero_agg, Publisher=='DC'), 10), head(filter(hero_agg, Publisher=='Marvel'), 10))
ggplot(head_hero, aes(Total, Name, fill=Publisher)) +
  geom_col(position=position_dodge())

#9
ggplot(hero_agg, aes(Total, color=Publisher, fill=Publisher)) +
  geom_density(alpha=0.3)

#10
data(economics)
head(economics)  

#11
p <- ggplot(economics, aes(x=date, y=unemploy)) +
  geom_line()

#12
p + annotate("rect", xmin =  as.Date("2005-00-00", "%d-%m-%Y"), xmax=as.Date("2010-00-00", "%d-%m-%Y"), ymin = -Inf, ymax = Inf, alpha = .5)

#13
p + labs(title='Texto Relevante')

#14
economics <- economics %>%
  gather(key = "variable", value = "value", -date)

ggplot(economics, aes(x=date, y=value, color=variable)) + 
  geom_line()  

#15
ggplot(economics, aes(x=date, y=value)) + 
  geom_line() +
  facet_grid(economics$variable ~ ., scales = "free")