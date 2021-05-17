
#using read.csv function to read data 
bodytemp = read.csv(file="/Users/cherukurikavyachowdhary/Downloads/bodytemp-heartrate.csv", header = T )

#creating two seperate data for male and female
male = subset(bodytemp, bodytemp$gender == 1)
female = subset(bodytemp, bodytemp$gender == 2)

#plotting box plots for body temperature
boxplot(male$body_temperature, female$body_temperature, main = "Boxplots of Body Temperatures", ylab = "Temperatures", names = c('male','female'))

#plotting Q-Q plots for the body temperature
par(mfrow=c(1,2))
qqnorm(male$body_temperature, main = 'Q-Q Plot for male')
qqline(male$body_temperature)
qqnorm(female$body_temperature, main = 'Q-Q Plot for female')
qqline(female$body_temperature)

#confidence interval using t,test function for the body temperature values
t.test(male$body_temperature, female$body_temperature, alternative = 'two.sided', var.equal = F)