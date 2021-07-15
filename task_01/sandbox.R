# Trabalho 1 Bioestatística
# Aluno: Carlos Frederico Carvalheira Mello
# Author: Carlos Carvalheira
# github: https://github.com/carvalheirafc/r_bio

 
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


q1_na_count <- ggplot(df_na_columns_tidy, aes(x = reorder(variable, desc(na_count)), y=na_count, fill=variable)) +
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
df_2 <- df %>% 
  dplyr::group_by(Class, Species) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::arrange(desc(n)) %>%
  head(., 5)
# Literal Answer ~~ Question 2 
head(df_2, 1)
# Top 5 Sorted Plot 
q2_class_by_species <- ggplot(df_2, aes(x=reorder(Species, desc(n)), y=n, fill=Class)) + 
  ggplot2::geom_col() + 
  coord_flip() +
  labs(x='', y='Species Count') +
  ggtitle('Species Count by Class') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=n), color='white', fontface=2, size=8, hjust=2)


################################################################################
# Question 3
df_3 <- df %>%
  dplyr::group_by(Family, Phylum, Class) %>%
  dplyr::summarise(CDS.Count.Mean=mean(`CDS Count   * assembled`),
                   CDS.Count.Max=max(`CDS Count   * assembled`),
                   CDS.Count.Mean=min(`CDS Count   * assembled`), 
                   CDS.Count.By.Object=n(), 
                   .groups='drop') %>%
  dplyr::arrange(Family, Class, Phylum)

head(df_3, 5)


################################################################################
#Question 4
df_4 <- df %>%
  dplyr::group_by(Phylum, `Oxygen Requirement`) %>%
  dplyr::summarise(Freq=n(), .groups='drop') %>%
  tidyr::drop_na(.) %>%
  dplyr::arrange(desc(Freq))
# By Phylum Classification
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
q4_phylum_by_oxygen <- ggpubr::ggarrange(smaller_freqs, small_freqs, medium_freqs, larger_freqs)
# By Oxygen Requirement Classification
q4_oxygen_by_phylum <- ggplot(df_4, aes(x=reorder(x=`Oxygen Requirement`, desc(Freq)), y=Freq,  fill=Phylum)) +
  ggplot2::geom_col() +
  coord_flip() + 
  labs(x='', y='Oxygen Requirement') +
  ggtitle('Oxygen Requirement by Phylum Medium') +
  theme(plot.title = element_text(hjust = 0.5))

q4_oxygen_by_phylum


################################################################################
#Question 5
# Insight of what is the types of sequencing methods
# Run only if curious...
# unique_values_sequency <- unique(df$`Sequencing Method`)
# unique_values_sequency_splited <- table(unlist(strsplit(unique_values_sequency, ',')))
# unique_values_sequency_splited <- as.data.frame(unique_values_sequency_splited)
# unique_sequency_model <- unique_values_sequency_splited$Var1 
sequency_names <- c('454', 'Illumina', 'Ion Torrent', 
                    'Oxford Nanopore', 'PacBio', 'Sanger', 
                    'Solexa', 'SOLiD', 'ABI')

# Formatting Release Date from m/d/Y to Year only
df$`Release Date` <- format(as.Date(df$`Release Date`, format='%m/%d/%Y'), '%Y')
# Gathering data for creation of dataframe 5
# Important Steps:
# mutate: every ',' since two methods was used for
# unrest: duplicate mutated row into new ones, to preserve the count of use of all
# sequency methods mutateds.
df_5 <- df %>%
  dplyr::select(`Release Date`, `Sequencing Method`) %>%
  drop_na() %>%
  dplyr::group_by(`Release Date`, `Sequencing Method`) %>%
  dplyr::summarise(Freq=n(), .groups='drop') %>%
  dplyr::mutate(`Sequencing Method` = strsplit(as.character(`Sequencing Method`), ",")) %>%
  unnest(cols = c(`Sequencing Method`)) # Split Rows by 

# Function that creates a temp obj with 
# detected string in `Sequency Method` column, and group the data by date for 
# then count the frequency
rename_sequency <- function(df, name){
  df %>%
    dplyr::filter(str_detect(`Sequencing Method`,name)) %>%
    dplyr::mutate(`Sequencing Method`=name) %>%
    dplyr::group_by(`Release Date`) %>%
    dplyr::summarise(n=sum(Freq))
}
# Applying the rename_sequency function to all values in sequency_names and joing
# all in one dataframe
# head: Date, Frequency of 454, Frequency of Illumina, etc...
df_5_tidy <- dplyr::full_join(rename_sequency(df_5, '454'), 
                  rename_sequency(df_5, 'Illumina'), by='Release Date') %>%
            dplyr::full_join(., 
                          rename_sequency(df_5, 'Ion Torrent'), by='Release Date') %>%
            dplyr::full_join(., 
                          rename_sequency(df_5, 'Oxford Nanopore'), by='Release Date') %>%
            dplyr::full_join(., 
                             rename_sequency(df_5, 'PacBio'), by='Release Date') %>%
            dplyr::full_join(., 
                             rename_sequency(df_5, 'Sanger'), by='Release Date') %>%
            dplyr::full_join(., 
                             rename_sequency(df_5, 'Solexa'), by='Release Date') %>%
            dplyr::full_join(., 
                             rename_sequency(df_5, 'SOLiD'), by='Release Date') %>%
            dplyr::full_join(., 
                             rename_sequency(df_5, 'ABI'), by='Release Date')
# DataFrame rename columns and replacing na by 0
colnames(df_5_tidy)[-1] <- sequency_names
df_5_tidy[is.na(df_5_tidy)] <- 0
df_5_tidy <- tidyr::gather(df_5_tidy, key='SequencingMethod', value='Freq', -`Release Date`)
#Final Steps
# Plotting the df_5_tidy as a geom_col
ggplot(df_5_tidy, aes(x=Freq, y=`Release Date`, fill=SequencingMethod)) +
  ggplot2::geom_col() +
  labs(x='', y='') +
  ggtitle('Sequencing Method Used by Years') + 
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
# Question 6
#cor.test(df$`Genome Size   * assembled`, df$`Gene Count   * assembled`, method='spearman')


