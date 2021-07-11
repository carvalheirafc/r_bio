# IMPORTANT 
# Before Load libraries make sure they are all installed.
#install.packages('tidyverse')
#install.packages('ggpubr')

# Import Section
library(tidyverse)
library(ggpubr)

################################################################################
# Question 1
# Data Read into a DataFrame
df <- readr::read_tsv(file='data.tab')
glimpse(df)

# Na Search in DataFrame
df_na_columns_tidy <- df %>% 
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.)))) %>%
  dplyr::select_if(funs(sum(.)>0)) %>%
  tidyr::gather(., 'variable', 'na_count')


ggplot(df_na_columns_tidy, aes(x = reorder(variable, desc(na_count)), y=na_count, fill=variable)) +
  ggplot2::geom_col() + 
  theme_void() +
  coord_flip() +
  labs(x='') + labs(y='Na Values Count') +
  ggtitle('NA Values by Variable') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=na_count), hjust=-1) +
  geom_hline(yintercept = length(df$Domain), color='red', size=2) + 
  geom_text(aes(0,length(df$Domain),label = length(df$Domain), hjust = 1.3), vjust=-0.5, color='red') +
  annotate('text', 0.16, 2760, label='Total Objects in DataFrame:', color='red', fontface=2)

################################################################################
# Question 2
# Top 5 Species by Class 
top_5_class_by_species <- df %>% 
  dplyr::group_by(Class, Species) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::arrange(desc(n)) %>%
  head(., 5)

# Literal Answer ~~ Question 2 
head(top_5_class_by_species, 1)

# Top 5 Sorted Plot 
ggplot(top_5_class_by_species, aes(x=reorder(Species, desc(n)), y=n, fill=Class)) + 
  ggplot2::geom_col() + 
  coord_flip() +
  labs(x='', y='Species Count') +
  ggtitle('Species Count by Class') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=n), color='white', fontface=2, size=8, hjust=2)

################################################################################
# Question 3
family_by_class_and_phylum <- df %>%
  dplyr::group_by(Family, Phylum, Class) %>%
  dplyr::summarise(CDS.Count.Mean=mean(`CDS Count   * assembled`),
                   CDS.Count.Max=max(`CDS Count   * assembled`),
                   CDS.Count.Mean=min(`CDS Count   * assembled`), 
                   CDS.Count.By.Object=n(), 
                   .groups='drop') %>%
  dplyr::arrange(Family, Class, Phylum)

head(family_by_class_and_phylum, 5)

################################################################################
#Question 4
df_4 <- df %>%
  dplyr::group_by(Phylum, `Oxygen Requirement`) %>%
  dplyr::summarise(Freq=n(), .groups='drop') %>%
  tidyr::drop_na(.) %>%
  dplyr::arrange(desc(Freq))


# Dividing Plots in 4 different ones by Frequency Count
larger_freqs <- ggplot(df_4 %>% dplyr::filter(Freq > 50), 
                       aes(x=reorder(Phylum, desc(Freq)),
                           y=Freq, fill=`Oxygen Requirement`)) +
  
  ggplot2::geom_col(position = position_dodge()) +
  coord_flip() +
  labs(x='', y='Phylum') +
  ggtitle('Oxygen Requirement by Phylum Smaller') +
  theme(plot.title = element_text(hjust = 0.5))

smaller_freqs <- ggplot(df_4 %>% dplyr::filter(Freq == 1), 
                        aes(x=reorder(Phylum, desc(Freq)),
                            y=Freq, fill=`Oxygen Requirement`)) +
  
  ggplot2::geom_col(position = position_dodge(width = 1)) +
  coord_flip() +
  labs(x='', y='Phylum') +
  ggtitle('Oxygen Requirement by Phylum Small') +
  theme(plot.title = element_text(hjust = 0.5))

small_freqs <- ggplot(df_4 %>% dplyr::filter(Freq <= 10 & Freq > 1), 
                      aes(x=reorder(Phylum, desc(Freq)),
                          y=Freq, fill=`Oxygen Requirement`)) +
  
  ggplot2::geom_col(position = position_dodge(width = 1)) +
  coord_flip() +
  labs(x='', y='Phylum') +
  ggtitle('Oxygen Requirement by Phylum Medium') +
  theme(plot.title = element_text(hjust = 0.5))

medium_freqs <- ggplot(df_4 %>% dplyr::filter(Freq < 50 & Freq > 10), 
                       aes(x=reorder(Phylum, desc(Freq)),
                           y=Freq, fill=`Oxygen Requirement`)) +
  
  ggplot2::geom_col(position = position_dodge(width = 1)) +
  coord_flip() +
  labs(x='', y='Phylum') +
  ggtitle('Oxygen Requirement by Phylum Larger') +
  theme(plot.title = element_text(hjust = 0.5))


#Final Plot with all 4 together 
ggpubr::ggarrange(smaller_freqs, small_freqs, medium_freqs, larger_freqs)


################################################################################



  



