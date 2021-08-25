# Teorema Central do Limite
# Exercicios

library(ggplot2)


x <- seq(0, 1, by=0.1)
sm <- c()
for (i in 1:1000){
  sm[i] <- mean(runif(10))
}
mean(sm)

babys <- rnorm(n=100, mean=3.4, sd=0.6)
# 1
samples <- c()
for (index in 1:1000){
  samples[index] <- mean(rnorm(100, 3.4, 0.6))
}
samples

mean(samples)
sd(samples)

# 2
samples_2 <- c()
a <- runif(n=10, min=0, max=10)
for (index in 1:10){
  samples_2[index] <- sum(runif(n=10, min=0, max=10))
}
data.frame(samples_2)
ggplot(data.frame(samples_2), aes(x=samples_2)) + ggplot2::geom_histogram(binwidth = 1)
# Maybe normal

# 3
samples_3 <- c()
for (index in 1:20){
  samples_3[index] <- var(x=(rexp(n=10, rate=1)))
}

data.frame(samples_3)
ggplot(data.frame(samples_3), aes(x=samples_3)) + ggplot2::geom_histogram(binwidth = 1)
# Maybe Left Skew



########################################################################################

# 1
dist <- rnorm(n=200, mean=140, var=25)
ggplot(data.frame(dist), aes(x=dist)) + ggplot2::geom_histogram()
#dist <- scale(dist)

sample_length <- length(dist)
mean <- mean(dist)
std_error <- sd(dist)/sqrt(sample_length)
lc <- qt(c(0.025, 0.975), sample_length - 1)

q1_a <- mean + lc * std_error

dist_b <- rnorm(n=100, mean=140, var=25**2)
ggplot(data.frame(dist), aes(x=dist)) + ggplot2::geom_histogram(binwidth = 5)
dist_b <- scale(dist_b)

sample_length <- length(dist_b)
mean <- mean(dist_b)
std_error <- sd(dist_b)/sqrt(sample_length)
lc <- qt(c(0.025, 0.975), sample_length - 1)

q1_b <- mean + lc * std_error



lc <- qt(c(0.050, 0.950), 200 - 1)
q1_a <- 140 + (lc * (25/sqrt(200)))
140 + lc * 25/sqrt(200)


lc_2 <- qt(c(0.05, 0.95), 100 - 1)
q1_b <- 140 + (lc * (25/sqrt(100)))
140 + lc_2 * 25/sqrt(100)

140 + qnorm(c(0.025, 0.975), mean = 0, sd = 1, lower.tail=TRUE) * 25


dist <- rnorm(n=100, mean=140, sd=25)
dist <- scale(dist)
tc_a <- t.test(dist)
tc_a$conf.int

dist <- rnorm(n=200, mean=140, sd=25)
tc_a <- t.test(dist)
tc_a$conf.int


##########################################################################################
# rnom sd 2 sample size = 12,  13, 15

confianca = mean + lc * sd/sqrt(sample)
se <- 2/sqrt(12)
mean <- 0
15 / se

########################################################################################
# Questionario 1
# Animais
animais <- c(635, 704, 662, 560, 603, 745, 698, 575, 633, 669)
mean(animais)
t.test(animais, mu=625, alternative='greater')


########################################################################################
npop <- 1000
pop <- runif(npop, 0, 100)
hist(pop)

# Gerando uma distribuição T a partir desta população
tscore  <- c() # vetor que armazena os valores T
nsample <- 40   # tamanho da amostra
rep <- 1000    # número de replicações


for (i in 1:rep){
  # sorteando índices da população
  sind <- sample(c(1:npop), nsample) 
  
  # gerando o vetor das amostras
  sample <- pop[sind] 
  
  # calculando o valor t para a amostra e armazenando em tscore
  tscore[i]<-(mean(sample) - mean(pop))/(sd(sample)/nsample^0.5)
}
# Gerando o qqplot para ver se a distribuição do valor T simulado
# segue a distribuição T teórico
y <- rt(rep, df = nsample - 1)
qqplot(y, tscore)
qqline(y, distribution = function(p) qt(p, df = nsample - 1))
