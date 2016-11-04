# Linear Regression Model
setwd("C:/Users/hp/Desktop/Ivy_R/Class 4")

train <- read.csv("input/train.csv", header = T)
test <- read.csv("input/test.csv", header = T)
store <- read.csv("input/store.csv")

train <- merge(train,store)
test <- merge(test,store)

# Separation in to two data sets

df <- train
fractiontraining <- 0.75
fractionvalidation <- 0.25

Samplesizetraining <- floor(fractiontraining*nrow(df))
samplesizevalidation <- floor(fractionvalidation*nrow(df))

indicesTraining    <- sort(sample(seq_len(nrow(df)),Samplesizetraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation  <- sort(sample(seq_len(nrow(df)), size=samplesizevalidation))


dfTraining   <- df[indicesTraining, ]
dfValidation <- df[indicesValidation, ]
write.csv(dfValidation, "cv.csv")

# Cleaning of Data


munge_data <- function(dt){
# replacing NA's by the mean value  
# na.rm = T, takes only True values
dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
dt$Open[is.na(dt$Open)] = round(mean(dt$Open, na.rm = T))

# converting to numeric
dt$StateHoliday = as.numeric(dt$StateHoliday)
dt$StoreType = as.numeric(dt$StoreType)
dt$Assortment = as.numeric(dt$Assortment)
dt$PromoInterval = as.numeric(dt$PromoInterval)

# seperating out the elements of the date column for the dt set
dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
dt$month <- as.integer(format(dt$Date, "%m"))
dt$year <- as.integer(format(dt$Date, "%y"))
dt$day <- as.integer(format(dt$Date, "%d"))

# removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
dt$Date = NULL
dt$Customers = NULL
dt$CompetitionOpenSinceYear = NULL

return(dt)
}

dfTraining = munge_data(dfTraining)
dfValidation = munge_data(dfValidation)
test = munge_data(test)

mod = lm(Sales ~ ., data = dfTraining)

summary(mod)


cv <- read.csv("cv.csv")

y = predict(mod, newdata = cv)

submission <- data.frame(Id = cv$Id, Salesnew = y)

write.csv(submission,"sample_submission1.csv")

# Steps to improve model 
# Stepwise Selection

library(MASS)
stepAIC(mod, direction="backward")

# All subset regression

library(leaps)
leaps <-regsubsets(Sales ~ .,data = dfTraining,nbest=2)
plot(leaps, scale="adjr2")

mod1 = lm(formula = Sales ~ DayOfWeek + Open + Promo + StateHoliday + 
      Assortment + CompetitionDistance + 
            PromoInterval + year, data = dfTraining)
summary(mod1)

# To detect Outliers & removal

summary(dfTraining)

hist(dfTraining$CompetitionDistance)
data_frame_hist <- subset(dfTraining, dfTraining$CompetitionDistance<40000)
mod2 <- lm(formula = Sales ~ DayOfWeek + Open + Promo + CompetitionDistance + 
             PromoInterval+ Assortment + StateHoliday , data = data_frame_hist)
summary(mod2)

y = predict(mod2, newdata = test)

submission <- data.frame(Id = test$Id, Sales = y)

write.csv(submission,"sample_submission.csv")



