#using read.csv function to read data 
bodytemp = read.csv(file="/Users/cherukurikavyachowdhary/Downloads/bodytemp-heartrate.csv", header = T )

#creating two seperate data for male and female
male = subset(bodytemp, bodytemp$gender == 1)
female = subset(bodytemp, bodytemp$gender == 2)

#drawing boxplot for the heart rate values
boxplot(male$heart_rate, female$heart_rate, main = "Boxplots of Heart Rates", names = c('male', 'female'), ylab = "Heart Rates")

#drawing Q-Q plot for the heart rate values
par(mfrow=c(1,2))
qqnorm(male$heart_rate, main = 'Q-Q Plot for Males')
qqline(male$heart_rate)
qqnorm(female$heart_rate, main = 'Q-Q Plot for Females')
qqline(female$heart_rate)

#getting the confidence interval using the t.test function
t.test(male$heart_rate, female$heart_rate, alternative = 'two.sided', var.equal = F)