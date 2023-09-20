install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
library(ggplot2)
install.packages("vcd")
library(vcd)
install.packages("gmodels")
library(gmodels)
#Install packages and libraries
MD <- read_csv("C:/Users/merce/Downloads/MD.csv")
View(MD)
#Upload CSV file and View
variables <- MD[, c("HighBlood", "Stroke","Overweight", "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Income", "TotalCharge", "Additional_charges", "Initial_days")]
# Create data frame "variable"
variables
# view data frame "Variables"
summary(variables)
#summarize variables 
v <- variables
#Change Variables to v
hist(MD$Initial_days)
# Histogram Initial days
hist(MD$Income)
#hist(Income)
hist(MD$TotalCharge)
#Histogram for TC
hist(MD$Additional_charges)
#Histogram ADC
category_counts <- table(MD$Asthma)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Asthma")
#Pie chart for Asthma
category_counts <- table(MD$HighBlood)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of HighBlood")
#Pie chart for High Blood
category_counts <- table(MD$Stroke)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Stroke")
#pie chart for stroke
category_counts <- table(MD$Overweight)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Overweight")
#pie chart Overweight
category_counts <- table(MD$Arthritis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Arthritis")
#pie chart arthritis
category_counts <- table(MD$Diabetes)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Diabetes")
#pie chart Diabetes
category_counts <- table(MD$Hyperlipidemia)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Hyperlipidemia")
#Pie chart Hyperlipidemia
category_counts <- table(MD$BackPain)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of BackPain")
#Back pain pie chart
category_counts <- table(MD$Anxiety)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Anxiety")
#pie chart for anxiety
category_counts <- table(MD$Allergic_rhinitis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Allergic rhinitis")
#pie chart Allergic Rhinitis
category_counts <- table(MD$Reflux_esophagitis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Reflux_esophagitis")
#pie chart Reflux Esophagitis
num_columns <- ncol(v)
#number of columns
MD$Reflux_esophagitis <- factor(MD$Reflux_esophagitis)
#Reflux esophagitis make into a factor
ggplot(MD, aes(x = Reflux_esophagitis, y = Initial_days)) +
  geom_boxplot() +
  xlab("Reflux Esophagatis") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Reflux Esophagitis")
#bivariate graph for RE
v$Allergic_rhinitis <- factor(v$Allergic_rhinitis)
#create Allergic Rhinitis
ggplot(v, aes(x = Allergic_rhinitis, y = Initial_days)) +
  geom_boxplot() +
  xlab("Allergic Rhinitis") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Allergic Rhinitis")
#boxplot with AR
MD$Stroke <- factor(MD$Stroke)
#stoke factor
ggplot(MD, aes(x = Stroke, y = Initial_days)) +
  geom_boxplot() +
  xlab("Stroke") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Stroke")
#boxplot for stroke
MD$Overweight <- factor(MD$Overweight)
#Change Overweigh to factor
ggplot(MD, aes(x = Overweight, y = Initial_days)) +
  geom_boxplot() +
  xlab("Overweight") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Overweight")
#boxplot for Overweight
MD$Arthritis <- factor(MD$Arthritis)
#change arthritis to a factor
ggplot(MD, aes(x = Arthritis, y = Initial_days)) +
  geom_boxplot() +
  xlab("Arthritis") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Arthritis")
#Create boxplot
MD$HighBlood <- factor(MD$HighBlood)
MD$Diabetes <- factor(MD$Diabetes)
MD$Hyperlipidemia <- factor(MD$Hyperlipidemia)
MD$BackPain <- factor(MD$BackPain)
MD$Anxiety <- factor(MD$Anxiety)
MD$Asthma <- factor(MD$Asthma)
#Convert X variables to Categorical
ggplot(MD, aes(x = HighBlood, y = Initial_days)) +
  geom_boxplot() +
  xlab("High Blood") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by High Blood")
#Bivaraiate graph with HB
ggplot(MD, aes(x = Diabetes, y = Initial_days)) +
  geom_boxplot() +
  xlab("Diabetes") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Diabetes")
#Bivariate graph Diabetes
ggplot(MD, aes(x = Hyperlipidemia, y = Initial_days)) +
  geom_boxplot() +
  xlab("Hyperlipidemia") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Hyperlipidemia")
#bivariate graph  HLA
ggplot(MD, aes(x = BackPain, y = Initial_days)) +
  geom_boxplot() +
  xlab("Back Pain") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Back Pain")
#Bivariate graph BP
ggplot(MD, aes(x = Anxiety, y = Initial_days)) +
  geom_boxplot() +
  xlab("Anxiety") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Anxiety")
#Bivariate graph Anxiety
ggplot(MD, aes(x = Asthma, y = Initial_days)) +
  geom_boxplot() +
  xlab("Asthma") +
  ylab("Initial Days") +
  ggtitle("Box Plot of Initial Days by Asthma")
#Bivariate graph Asthma
ggplot(MD, aes(x = Income, y = Initial_days)) +
  geom_point() +
  xlab("Income") +
  ylab("Initial Days") +
  ggtitle("Scatter Plot of Income vs Initial Days")
#Scatter plot for Income
ggplot(MD, aes(x = TotalCharge, y = Initial_days)) +
  geom_point() +
  xlab("Total Charge") +
  ylab("Initial Days") +
  ggtitle("Scatter Plot of Total Charge vs Initial Days")
#Line plot for TC
ggplot(MD, aes(x = Additional_charges, y = Initial_days)) +
  geom_point() +
  xlab("Additional Charges") +
  ylab("Initial Days") +
  ggtitle("Scatter Plot of Additional Charges vs Initial Days")
#Scatterplot ADC
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Diabetes + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + Income + TotalCharge + Additional_charges, data = MD)
#Create linear models
summary(Lmodel)
#Print model
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + Income + TotalCharge + Additional_charges, data = MD)
#Create linear models
summary(Lmodel)
#print model
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear regression model
summary(Lmodel)
#print lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear regression
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#Linear model
summary(Lmodel)
#print lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + Anxiety + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Overweight + Arthritis + Hyperlipidemia + Anxiety + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Arthritis + Hyperlipidemia + Anxiety + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Hyperlipidemia + Anxiety + Reflux_esophagitis + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Stroke + Hyperlipidemia + Anxiety + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Hyperlipidemia + Anxiety + Asthma + TotalCharge + Additional_charges, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel <- lm(Initial_days ~ HighBlood + Hyperlipidemia + Anxiety + Asthma + TotalCharge, data = MD)
#linear model
summary(Lmodel)
#print Lmodel
Lmodel2 <- lm(Initial_days ~ HighBlood + Asthma + TotalCharge, data = MD)
#linear model
summary(Lmodel2)
#print Lmodel2
r_squared <- summary(Lmodel)$r.squared
#Extract Rsquared from Initial model
 print(r_squared)
 #print r_squared
 r_squared2 <- summary(Lmodel2)$r.squared
 #Extract Rsquared from reduced model
 print(r_squared2)
 #print rsquared for reduced model
 predicted_values <- predict(Lmodel2)
 #reduced model variables
 residuals <- residuals(Lmodel2)
 #Calculate residuals
 qqnorm(residuals, main = "QQ Plot of Residuals")
 qqline(residuals)
 #Created qqplot of residuals
 rse <- sqrt(sum(residuals^2) / (length(residuals) - length(Lmodel2$coefficients)))
 #Calculate the RSe
 print(rse)
 #Print RsE
 coefficients <- coef(Lmodel2)
 #coefficents Lmodel2
 print(coefficients)
 #print coefficent