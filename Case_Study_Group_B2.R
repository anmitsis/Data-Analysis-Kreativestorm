library(haven) #to read the .dta files
data<-read_dta("HSE_1999.dta") #load and view datasets
View(data)

newdata<-read_dta("HSE_2019.dta")
View(newdata)

#save variables to be easier for use
cignow<-data$cignow  #1 is yes 2 is no
cignow19<-newdata$cignow_19 #1 is yes 2 is no

#removing the non-positive values
cignow<-cignow[cignow>0]
cignow19<-cignow19[cignow19>0]

#values for the contingency table
sum(cignow==1) #1999 smokers
sum(cignow==2) #1999 non-smokers
sum(cignow19==1) #2019 smokers
sum(cignow19==2) #2019 non-smokers
tab<-matrix(c(1519,1344,1254,3063),nrow = 2) #contingency table
chisq.test(tab) #performing the chi-square test

sum(cignow==1)/length(cignow) #smoking percentage for 1999
sum(cignow19==1)/length(cignow19)  #smoking percentage for 2019
(sum(cignow==1)/length(cignow))/(sum(cignow19==1)/length(cignow19)) #ratio of ratios 
