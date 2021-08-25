
data <- matrix(c(260, 299,15,41,7,14), ncol=2, byrow=T)
rownames(data) <- c("branco", "negro", "outros")
colnames(data) <- c("sim","não")
data


chisq <- chisq.test(data)
chisq

round(chisq$expected,2)


# Question 1
data_1 <- matrix(c(14, 94, 4, 97), ncol=2, byrow=T)
rownames(data_1) <- c('Aqueous', 'Insolube')
colnames(data_1) <- c('Infected', 'Not Infected')
data_1

chisq_1 <- chisq.test(data_1)
chisq_1

qchisq(.95, df=1) 

# Question 2
data_2 <- matrix(c(28, 35, 19, 38, 41, 44, 53, 60), ncol=2, byrow=T)
rownames(data_2) <- c('Negative', 'Bipolar disorder', 'Unipolar', 'Uni and Bi')
colnames(data_2) <- c('Early <= 18', 'Later >= 18')
data_2

chisq.test(data_2)
qchisq(.95, df=3)

chisq.test(data_2)$expected


#data_3 <- matrix(c(0, 1, c(seq(6, 6.99, by=0.1)), 45,seq(1, 1.99, by=0.1), 5, seq(7, 7.99, by=0.1), 30,
#                   seq(2, 2.99, by=0.1), 15, seq(8, 8.99, by=0.1), 22,seq(3, 3.99, by=0.1),  24, seq(9, 9.99, by=0.1), 10,
#                   seq(4, 4.99, by=0.1), 43, seq(10, 10.99, by=0.1), 5, seq(5, 5.99, by=0.1), 50), ncol=4, byrow=T)
data_3 <- matrix(c(1, 45, 5, 30, 15, 22, 24, 10, 43, 5, 50), ncol=2, byrow=T)
data_3
p <- rnorm(250, mean=5.74, sd=2.01)

chisq.test(data_3)



############# 
# 1
data <- matrix(c(128, 44, 173, 108, 25, 22, 7, 12), ncol=2, byrow=T)
rownames(data) <- c("T1", "T2", "T3", "T4")
colnames(data) <- c("Vivo", "Morto")
data
chisq <- chisq.test(data)
chisq
round(chisq$expected,2)
round(chisq$residuals, 3)


q1 <- matrix(c(131, 373), ncol=1, byrow=T)
rownames(q1) <- c('F', 'M')
colnames(q1) <- c('Observado')
q1

q1_q <-chisq.test(q1, p=c(0.3, 0.7))
q1_q$expected
q1_q$residuals
q1_q



fi <- matrix(c(5, 5, 10, 21, 201, 150), ncol=2, byrow=T)
chisq.test(fi)$expected
