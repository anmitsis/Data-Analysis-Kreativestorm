df<-Birthweight_reduced_kg_SPSS
install.packages("dplyr")
library(dplyr)
#Q1
mean(df$fage)

#Q2

ranked_levels<-ntile(df$Birthweight,3)
ranked_levels<-factor(ranked_levels, labels = c("Low", "Medium", "High"))
summary(ranked_levels)
df$ranks<-ranked_levels
mean(df$fage[df$ranks=="Low"])
mean(df$fage[df$lowbwt==1])
#Q3
shapiro.test(df$fage)

#Q4-5-6
library(moments)
skewness(df$fage)
fage_log<-log(df$fage,base = 10)
shapiro.test(fage_log)
mean(fage_log)
log(mean(df$fage),base=10)
10^mean(fage_log)

#Q7-8
shapiro.test(df$fedyrs)


cor.test(df$Birthweight,df$fage,method = "spearman")



