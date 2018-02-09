Emp <- read.csv("/Users/rocket/Documents/Data_sets/Employee_Data.csv")
colnames(Emp)

#lable
sal <- ifelse (Emp$Emp_Sal == ">50K","High", "Low")
Emp <- data.frame(Emp,sal)
View(Emp)

library(caret)
names(Emp)
Emp <- Emp[,-15]

#Splitting the data into train and test
set.seed(123)
id<-sample(2,nrow(Emp),prob = c(0.7,0.3),replace = T)
Emptrain <- Emp[id ==1,] 
Emptest <- Emp[id ==2,]

#Naive Bayes
library(e1071)
library(caret)
emp_nb <- naiveBayes(sal ~ Age_Of_emp + Emp_Stat_type + Edu_of_Emp + Edu_Cat + Occ_Of_Emp + Work_hour_in_week + country_of_res , data = Emptrain)
summary(emp_nb)   

#prediction
pred <- predict(emp_nb,Emptest)

confusionMatrix(table(pred,Emptest$sal))

