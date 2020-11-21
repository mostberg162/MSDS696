### Richard M. Ostberg
### MSDS 696 Data Science Practicum
### Church Attendance Analysis
### 11/15/2020

install.packages("imputeTS")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forecast")
install.packages("zoo")
install.packages("lubridate")
install.packages("nortest")
library(forecast)
library(zoo)
library(imputeTS)
library(dplyr)
library(ggplot2)
library(lubridate)
library(nortest)

## Read in the data and take care of formatting and missing data
Churchdata <- read.csv("/Users/mostberg/Desktop/MSDS692/NewChurchDataForR.csv")
Churchdata$single.service <- factor(Churchdata$single.service, levels = c("Pre", "Single", "Post", "Video", "Covid"))
Churchdata$Date <- as.Date(Churchdata$Date, '%m/%d/%y')

## cut down to Sundays, then Interpolate
SundaysWithVideo <- 
  Churchdata %>%
  filter(Day.of.Week == 1 & Services > 0)

## distinguish between missing data and zeros, allow video weeks to interpolate with 2 services
SundaysWithVideo <-
  SundaysWithVideo %>% mutate(X1st.Service = ifelse(Services == 0 , 0, X1st.Service)) 
SundaysWithVideo <-
  SundaysWithVideo %>% mutate(X2nd.Service = ifelse(Services < 2 , 0, X2nd.Service)) 
SundaysWithVideo <-
  SundaysWithVideo %>% mutate(third.xmas = ifelse(Services < 3 , 0, third.xmas)) 

## interpolate missing data
SundaysWithVideo$X1st.Service <- na_interpolation(SundaysWithVideo$X1st.Service, option = "stine")
SundaysWithVideo$X2nd.Service <- na_interpolation(SundaysWithVideo$X2nd.Service, option = "stine")
SundaysWithVideo$third.xmas <- na_interpolation(SundaysWithVideo$third.xmas, option = "stine")

## make averages per service
SundaysWithVideo <-
  SundaysWithVideo %>%
  rowwise() %>%
  mutate(Totalattendance = sum(X1st.Service,X2nd.Service,third.xmas, na.rm=TRUE))
SundaysWithVideo <- mutate(SundaysWithVideo,averageservice = Totalattendance/Services)

## A useful subset is Sundays only, without the Video weeks
Sundays <- 
  SundaysWithVideo %>%
  filter(Day.of.Week == 1 & Services > 0 & single.service != "Video")

Sundays$single.service <- factor(Sundays$single.service, levels = c("Pre", "Single", "Post", "Covid"))
##Check for normality
ad.test(Sundays$Totalattendance)
## Looks like Easter/Christmas may be influencing normality

## Check to see if the single service period impacted attendance
SundayAnova <- Sundays[,c(5,17)]

fit <- lm(Totalattendance ~ ., data = SundayAnova)
anova(fit)

boxplot(Totalattendance ~ single.service, data = SundayAnova, main="Did COVID-19 Affect Attendance?", xlab = "Service", ylab='Total Attendance')

## ANOVA may skewed due to Easter, let's remove its influence to be sure
SundayNoHolidays <-
  Sundays %>%
  filter(Religious.Holiday == 0)

#Check for normality
ad.test(SundayNoHolidays$Totalattendance)
#Taking Easter/Christmas out does fix it.

fit2 <- lm(Totalattendance ~ single.service, data = SundayAnova)
anova(fit2)

boxplot(Totalattendance ~ single.service, data = SundayNoHolidays, main="Did COVID-19 Affect Attendance?", xlab = "Service No Easter/Christmas", ylab='Total Attendance')

## The difference looks like it may be significant, let's follow up with a t-test
PrePost <-
  SundayNoHolidays %>%
  filter(single.service == "Post" | single.service == "Covid")

t.test(PrePost$Totalattendance~PrePost$single.service)
PrePost$single.service <- factor(PrePost$single.service, levels = c("Post", "Covid"))
boxplot(Totalattendance ~ single.service, data = PrePost, main="Did Covid Affect Attendance?", xlab = "Service No Easter/Christmas", ylab='Total Attendance')

ggplot(PrePost, aes(Totalattendance, fill = single.service)) + geom_density(alpha = 0.2) + labs(title = "Did Covid Affect Attendance?", y='% of Sundays', x='Total Attendance')


## Growth Rate for total attendance
ggplot(Sundays, aes(Date,Totalattendance)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Attendance Growth", y='Total Attendance')


## Simple regression Time series
LinearModel <- lm(Totalattendance~Date, data = SundayNoHolidays)
summary(LinearModel)

## Growth Rate
growthrate <- function(x)(x/lag(x,52)-1)
Sundays$growthrate <- growthrate(Sundays$Totalattendance)

summary(Sundays$growthrate)
## Simple ARIMA models for second view
ChurchTS <- ts(as.numeric(SundaysWithVideo$Totalattendance), freq=365.25/7, 
               start=decimal_date(ymd("2016-05-29")))
ggtsdisplay(ChurchTS)
ggtsdisplay(diff(ChurchTS))
ggtsdisplay(diff(diff(ChurchTS)))

## I tried a number of different parameters, using guidelines from https://otexts.com/fpp2/arima.html
## This one fit best and had reasonable margins of error
fit2 <- Arima(ChurchTS, order = c(2,0,2), seasonal = list(order = c(0,0,3), period = 52))
print(fit2)
res <- residuals(fit2)
ggtsdisplay(res)
Box.test(res, lag=52, fitdf=2, type="Ljung")


# fit3 <- auto.arima(ChurchTS)
# print(fit3)
# res <- residuals(fit3)
# ggtsdisplay(res)
# Box.test(res, lag=52, fitdf=2, type="Ljung")

plot(ChurchTS)
plot(forecast(fit2,h=159, bootstrap = TRUE), main="Total Attendance Forecast (Seasonal Model)", xlab = "Weekly View", ylab='Total Attendance')
nextyear <- forecast(fit2, h = 53, bootstrap = TRUE)
volumes <- as.data.frame(nextyear$mean)
print(volumes)

# What if we look at just the current year post-Covid
# First, post and covid together

Church2TS <- ts(as.numeric(PrePost$Totalattendance), freq=365.25/7, 
               start=decimal_date(ymd("2019-10-06")))
ggtsdisplay(Church2TS)
ggtsdisplay(diff(Church2TS))
ggtsdisplay(diff(diff(Church2TS)))

fit3 <- auto.arima(Church2TS)
print(fit3)
res <- residuals(fit3)
ggtsdisplay(res)
Box.test(res, lag=52, fitdf=2, type="Ljung")

plot(Church2TS)
plot(forecast(fit3,h=53, bootstrap = TRUE), main="Pre-Post Covid With Forecast", xlab = "Weekly View", ylab='Total Attendance')
nextyear2 <- forecast(fit3, h = 53, bootstrap = TRUE)
volumes2 <- as.data.frame(nextyear2$mean)
print(volumes2)

## post-covid only
Covid <- PrePost %>%
  filter(single.service ==  "Covid")

Church3TS <- ts(as.numeric(Covid$Totalattendance), freq=365.25/7, 
                start=decimal_date(ymd("2020-06-07")))
ggtsdisplay(Church3TS)
ggtsdisplay(diff(Church3TS))
ggtsdisplay(diff(diff(Church3TS)))

fit4 <- auto.arima(Church3TS)
print(fit4)
res <- residuals(fit4)
ggtsdisplay(res)
Box.test(res, lag=52, fitdf=2, type="Ljung")

plot(Church3TS)
plot(forecast(fit4,h=53, bootstrap = TRUE), main="Post-Covid only \n With Forecast", xlab = "Weekly View", ylab='Total Attendance')
nextyear3 <- forecast(fit4, h = 53, bootstrap = TRUE)
volumes3 <- as.data.frame(nextyear2$mean)
print(volumes3)

## Average service modeling to answer when to get a new church
ggplot(Sundays, aes(Date,averageservice)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Attendance Growth", y='Average Attendance per Service')

## We need to take out the influence of "Single Service" 
## and interpolate the data for average service in those weeks

# Pull out the average for single service
SundaysNormalized <- Sundays %>%
  rowwise() %>%
  mutate(averageservice = ifelse(single.service == "Single", NA, averageservice))

SundaysNormalized$averageservice <- na_interpolation(SundaysNormalized$averageservice, option = "stine")

# Turn it into a time series for Kalman Smoothing
NormalizedAvgTS <- ts(as.numeric(SundaysNormalized$averageservice), freq=365.25/7, 
                  start=decimal_date(ymd("2016-05-29")))

# Replace original with Kalman smoothing
usermodel <- arima(NormalizedAvgTS, order = c(4,0,1))$model
SundaysNormalized$averageservice <- na_kalman(NormalizedAvgTS, model = usermodel)

## Average service with smoothed (ARIMA Estimated) data
ggplot(SundaysNormalized, aes(Date,averageservice)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Attendance Growth", y='Average Attendance per Service')
  
ChurchAvgTS <- ts(as.numeric(SundaysNormalized$averageservice), freq=365.25/7, 
                  start=decimal_date(ymd("2016-05-29")))
ggtsdisplay(ChurchAvgTS)
ggtsdisplay(diff(ChurchAvgTS))
ggtsdisplay(diff(diff(ChurchAvgTS)))

## I tried a number of different parameters, using guidelines from https://otexts.com/fpp2/arima.html
## This one fit best and had reasonable margins of error
fit2 <- Arima(ChurchAvgTS, order = c(4,1,0), seasonal = list(order = c(0,1,3), period = 52))
print(fit2)
res <- residuals(fit2)
ggtsdisplay(res)
Box.test(res, lag=53, fitdf=2, type="Ljung")

plot(ChurchAvgTS)
plot(forecast(fit2,h=53, bootstrap = TRUE), main="Average Attendance Forecast (Seasonal Model)", xlab = "Weekly View", ylab='Average Attendance')
next3year <- forecast(fit2, h = 53, bootstrap = TRUE)
volumes <- as.data.frame(nextyear$mean)
print(volumes)                        
print(nextyear$upper)

### Multiple regression to test extraneous variables for effect
RegressionSet <- Sundays[,8:17]
MultiLinearModelAvg <- lm(Totalattendance~., data = RegressionSet)
summary(MultiLinearModelAvg)

## Second model without religious holidays, since those effects are well known
SecondRegressionSet <- SundayNoHolidays[,8:17]
MultiLinearModelAvg <- lm(Totalattendance~., data = SecondRegressionSet)
summary(MultiLinearModelAvg)

## Read in the data and take care of formatting and missing data
Offerings <- read.csv("/Users/mostberg/Desktop/MSDS692/Offerings.csv")
Offerings$Date <- as.Date(Offerings$Month, '%m/%d/%y')   

## Growth Rate for offerings over time
ggplot(Offerings, aes(Date,Sum.of.Offering)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Offering Trend", y='Monthly Amount')

## Offerings compared to monthly attendance
ggplot(Offerings, aes(Monthly.Attendance,Sum.of.Offering)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Attendance vs. Offerings", y='Monthly Amount', x="Attendance")

## Simple regression 
LinearModel <- lm(Sum.of.Offering~Monthly.Attendance, data = Offerings)
summary(LinearModel)
