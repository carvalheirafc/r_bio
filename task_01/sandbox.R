# Import and Install Packages
#install.packages('tidyverse')
library(tidyverse)


# DataFrame Read
df <- readr::read_tsv(file='data.tab')

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







