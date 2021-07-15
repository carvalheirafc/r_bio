library(dplyr)
# conjunto de todas as proteínas de drosophila
# e suas anotações
data <- read.csv("drosophila.tab", sep="\t", na.strings="")
df <- tbl_df(data)
head(df)


# 1
