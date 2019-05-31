# Reading the CSV file
rime2014 <- read.csv("C:/Users/Ajay/Desktop/CSV/2014_anti_social.csv", header = TRUE)
crime2014a <- read.csv("C:/Users/Ajay/Desktop/CSV/2014_Crimetypes.csv", header = TRUE)
crime2014b <- cbind(crime2014,crime2014a)
View(crime2014b)
df2 <- subset(crime2014b, select = -c(Month,Year,Location,Number_of_incidents,Antisocial_type,X))
View(df2)
df2 <- subset(df2, select = -c(X))
View(df2)

# Reading the CSV file
crime2015 <- read.csv("C:/Users/Ajay/Desktop/CSV/2015_antisocial.csv", header = TRUE)
crime2015a <- read.csv("C:/Users/Ajay/Desktop/CSV/2015_Crimetypes.csv", header = TRUE)
crime2015b <- cbind(crime2015,crime2015a)
View(crime2015b)
df3 <- subset(crime2015b, select = -c(Month,Year,Location,Number_of_.incidents,Antisocial_incident))
View(df3)
df3 <- subset(df3, select = -c(X))
View(df3)

# Reading the CSV file
crime2016 <- read.csv("C:/Users/Ajay/Desktop/CSV/2016_antisocial.csv", header = TRUE)
crime2016a <- read.csv("C:/Users/Ajay/Desktop/CSV/2016_Crimetypes.csv", header = TRUE)
crime2016b <- cbind(crime2016,crime2016a)
View(crime2016b)
df4 <- subset(crime2016b, select = -c(Months,Year,Locations,Number_of_Incidents, Antisocial_type,X))
View(df4)

# Reading the CSV file
crime2017 <- read.csv("C:/Users/Ajay/Desktop/CSV/2017 antisocial.csv", header = TRUE)
crime2017a <- read.csv("C:/Users/Ajay/Desktop/CSV/2017 Crimetypes.csv", header = TRUE)
crime2017b <- cbind(crime2017,crime2017a)
View(crime2017b)
df5 <- subset(crime2017b, select = -c(Months,Year,Locations,Number_of_Incident, Antisocial_,X))
View(df5)

# Reading the CSV file
crime2018 <- read.csv("C:/Users/Ajay/Desktop/CSV/2018 antisocial.csv", header = TRUE)
crime2018a <- read.csv("C:/Users/Ajay/Desktop/CSV/2018-Crimetypes.csv", header = TRUE)
crime2018b <- cbind(crime2018,crime2018a)
View(crime2018b)
df6 <- subset(crime2018b, select = -c(Months,Year,Locations,Number_of_Incident, Antisocial_type,X))
View(df6)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)

# Renaming the columnnames
colnames(df2)[colnames(df2)=="Month"] <- "Months"
str(df2)
colnames(df3)[colnames(df3)=="Month"] <- "Months"
colnames(df6)[colnames(df6)=="Years"] <- "Year"
#df_final <- rbind(df2, df3, df4, df5, df6)
#View(df_final)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)

# Combaining the all the years of the dataset
df_final <- rbind(df2, df3, df4, df5, df6)
View(df_final)
df_final1 <- subset(df_final, select = -c(Months))
View(df_final1)
df_final_2 <- subset(df_final1, select = c(Year,Crime_types, Crime_Percent))
View(df_final_2)
plot(df_final_2)
#install.packages('tseries')
df_final1[df_final1 == '/0'] <- NA
View(df_final1)

# Ommiting all the null values from the dataset
df_final_3 <- na.omit(df_final1)
View(df_final_3)

str(df_final_3)
# Converting the data types 
df_final_3$Crime_types <- as.character(gsub('-','',df_final_3$Crime_types))
df_final_3$Crime_Percent <- as.numeric(gsub(',','',df_final_3$Crime_Percent))
df_final_3$Locations <- as.character(gsub(',','',df_final_3$Locations))
str(df_final_3)
df_final_4 <- subset(df_final_3, select = c(Year,Crime_Percent))
View(df_final_4)
str(df_final_4)
#install.packages('FitARMA')
library(tseries)
library(forecast)
# Predicting the forecast offuture crimes in different places along years
plot(df_final_4)
summary(df_final_4)
df_diff <- diff(df_final_4$Crime_Percent,4)
fit_arm <- arima(df_diff, order = c(1,0,0))
fit_arm
accuracy(fit_arm)
help("qqnorm")

# Plotting the rescidual  axis 
qqnorm(fit_arm$residuals)
qqline(fit_arm$residuals)
Box.test(fit_arm$residuals, type = 'Ljung-Box')

#Forecasting
# Plotting the ARIMA model in the dataset 

fit_forecst <- forecast(fit_arm,h=12)
plot(forecast(fit_forecst,h=12),include = 20)
summary(fit_forecst)
plot(fit_forecst, include = 20)
