x<-c("plyr", 'dplyr', 'tidyr', 'moments')
lapply(x,require,character.only = TRUE)
library(moments)
df<-Birthweight_reduced_kg_SPSS
summary(df$Birthweight)
shapiro.test(df$Birthweight)

Birthweight<-df$Birthweight

#Q1
mean(df$Birthweight[df$smoker==0])
#Q2
mean(df$Birthweight[df$smoker==1])
#Q3
mean(df$Headcirc[df$smoker==0])
  #Q4
mean(df$Gestation[df$smoker==1])
#Q5
max(df$Headcirc[df$smoker==0])
#Q6
min(df$Gestation[df$smoker==1])
#Q7-Q8
shapiro.test(df$Gestation)
summary(df$Gestation[df$smoker==0])
summary(df$Gestation[df$smoker==1])
#Q9
range(df$Birthweight[df$smoker==1])
#Q10
range(df$Birthweight[df$smoker==0])
#Q11-Q12
shapiro.test(df$Headcirc[df$smoker==1])
#Q13
(35.05-mean(df$Headcirc[df$smoker==0]))/sd(df$Headcirc[df$smoker==0])
#Q14
skewness(df$Birthweight[df$smoker==0])
#Q15-Q16
shapiro.test(df$Birthweight[df$smoker==1])
qqnorm(df$Birthweight[df$smoker==1])
qqline(df$Birthweight[df$smoker==1])
#Q18
z1<-(4.2-mean(df$Birthweight[df$smoker==1]))/sd(df$Birthweight[df$smoker==1])
pnorm(z1)
#peripou 95% sigouroi
#Q19
shapiro.test(df$Length[df$smoker==0])
qqnorm(df$Length[df$smoker==1])
qqline(df$Length[df$smoker==1])
plot(density(df$Length[df$smoker==1]))
hist(df$Length[df$smoker==1])
#Q21
(48.5-mean(df$Length[df$smoker==0]))/sd(df$Length[df$smoker==0])
#Q22
z2<-(55-mean(df$Length[df$smoker==0]))/sd(df$Length[df$smoker==0])
1-pnorm(z2)


