
#reading a patient data and analysing it
#loading packages
install.packages("hrbrthemes")
install.packages("scales")
install.packages("tsoutliers");
install.packages("lmtest")
install.packages("grid")
install.packages("gridExtra")
library(gridExtra)
library(grid)
library(hrbrthemes)
library(scales)
library(lmtest)     #to check Homoskedasticity and heteroskedascity
library(tsoutliers)
library(datasets)
library(ggcorrplot)
library(dplyr)    #dplyr is a package for making data manipulation easier
library(tydyr)    #tidyr  package provides various important functions that can be used for Data Cleaning
library(ggplot2)  #ggplot2 is a powerful and a flexible for producing elegant graphics
library(corrplot) #package for correlation plot

#loading our dataset
PatientData = read.csv('dataKidneyReg_Mac.csv', header = TRUE, sep = ";")

str(PatientData)
#structure of a dataset
# Given Patient dataset include  different patient dermographic information  such as age and medical condition i.e Blood Pressure
# Gravity, Sugar,Red blood, Bacteria,Sodium, Potassium etc ..
#The quality of data that is given in medical report of patient such as age ,blood count ,Bp can be used to preidict
#health condition level and can help in diagnosis for various disease
#Dataset include sugar level of a patient in 4th coloumn.Even diet can be suggested as per given data
sum(is.na(PatientData))
#there is no missing data in Patient data




#Conducting exploratory analysis:
#dplyr pipe function
summary(PatientData)

barplot(PatientData$Glucose, ylim = c(0,500) , xlab = 'Individual Patients Sample' ,ylab = 'Glucose level')

#plotting count for different sugar level
nn<-ggplot(PatientData, aes(x=(Sugar) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7))
nn+ylim(0,200)


table(PatientData$Sugar)



             
             


#conducting exploratory analysis of age ,blood pressure,sugar,diabetes,glucose
data1 = PatientData %>%
  select(Age, Blood_Pressure, Sugar, Diabetes, Glucose)
summary(data1)
ggplot(data1, aes(x=Glucose, y=Diabetes)) + 
  geom_point(size=6, color="#69b3a2") +
  theme_ipsum()
M = cor(data1)
corrplot(M, method = 'number')








#patient having diabetes are stored in bh variable
bh <- data1 %>%
  filter(Diabetes == '1')
mean(bh$Age)


hist(
  data1$Age,
  main = "Histogram for Age",
  xlab = "Age",
  border = "black",
  col = "red"
)

hist(
  bh$Age,
  main = "Histogram for age having diabetes",
  col = "blue",
  border = "black",
  xlab = "Age  "
)


plot(data1$Sugar, data1$Glucose, data = data1)


#correlation of all patient data
N = cor(PatientData)

corrplot(N, method = 'number')


#strong association between cell volume and hemoglobin
qplot(x = Hemoglobin, y = Cell_Volume, data =PatientData) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(" Strong Positive Association")

#correlation plots of different vartiables
q1 <- qplot(x = Gravity, y = Pedal_Edema, data = PatientData)
q2 <- qplot(x = Glucose, y = Diabetes, data = PatientData)
q3 <- qplot(x = Cell_Volume, y = Anemia, data = PatientData)
q4 <- qplot(x = Sugar, y = Glucose, data = PatientData)

grid.arrange(q1, q2, q3, q4, ncol = 2, 
             top = textGrob("correlation plots"))

depvars = PatientData[, 7]

#removing dependent vartiable
exvars = select(PatientData,-7)

corrplot(cor(exvars), method = 'number')

cormat = abs(cor(exvars))
#Making diagnol elements of correlation zeroi to filter model with high correl
diag(cormat) = 0







#Removing explonatory variables that have high absolute correlation  more than 0.8
while (max(cormat) >= 0.8) {
  maxvar = which(cormat == max(cormat), arr.ind = TRUE)
  
  maxavg = which.max(rowMeans(cormat[maxvar[, 1], ]))
  
  print(rownames(maxvar)[maxvar[, 1] == maxvar[maxavg, 1]])
  exvars = exvars[, -maxvar[maxavg, 1]]
  cormat = cormat[-maxvar[maxavg, 1],-maxvar[maxavg, 1]]
}
cor(cormat)
my_data= cbind('Glucose' = depvars,exvars)







#Implementing linear regression model
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Bacteria+Sodium+Potassium+Hemoglobin+White_BloodCount+Red_BloodCount+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)

#Removing Hemoglobin
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Bacteria+White_BloodCount+Sodium+Potassium+Red_BloodCount+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)
#Removing white blood count
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Bacteria+Sodium+Potassium+Red_BloodCount+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)
#Removing Sodium
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Bacteria+Potassium+Red_BloodCount+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)
#Removing Red blood count
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Bacteria+Potassium+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)
#Removing Bacteria 
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Potassium+Diabetes+Pedal_Edema+Anemia, data=my_data)
summary(lineRegModel)
#Removing Anemia
lineRegModel= lm(Glucose~Age+Blood_Pressure+Sugar+Gravity+Red_Blood+Potassium+Diabetes+Pedal_Edema, data=my_data)
summary(lineRegModel)
#Removing age
lineRegModel= lm(Glucose~Blood_Pressure+Sugar+Gravity+Red_Blood+Potassium+Diabetes+Pedal_Edema, data=my_data)
summary(lineRegModel)
#Removing blood pressure
lineRegModel= lm(Glucose~Sugar+Gravity+Red_Blood+Potassium+Diabetes+Pedal_Edema, data=my_data)
summary(lineRegModel)
#Coefficiant in the model
coef(lineRegModel)




# Properties of linear regression(checking properties for residuals )


#mean of residual around zero
mean(residuals(lineRegModel))



#Breusch-Pagan test to check heteroskedascity
# p value is less than 0.05 means it is heteroskedacity
bptest(lineRegModel)



#Durbin Watson test to check for auto correlation
#Value of dw is greater than 2 and p value is also greater than 0.05 it clearly indicates that there is negative correlation
dwtest(lineRegModel)


#relationship between residuals and explonatory variables
cor(residuals(lineRegModel) , my_data[,2:15])



#JarqueBera test
JarqueBera.test(residuals(lineRegModel))

hist(residuals(lineRegModel))
