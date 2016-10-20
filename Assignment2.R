# Question 1
df["X_Square"] <- NA
df$X_Square <- df$X * df$X
# Question 2
sum(df$FFMC)
sum(df$DMC)
sum(df$DC)
mean(df$FFMC)
mean(df$DMC)
mean(df$DC)
median(df$FFMC)
median(df$DMC)
median(df$DC)
sd(df$FFMC)
sd(df$DMC)
sd(df$DC)
# Question 3
df["Month"] <- NA
summary(df)
df$Month <- 
  sapply(df$month,function(x){
    if(x=="aug"){
      x <- as.factor("August")
    }
    else if (x=="jan"){
      x <- as.factor("January")
    }
    else if (x=="feb"){
      x <- as.factor("February")
    }
    else if (x=="mar"){
      x <- as.factor("March")
    }
    else if (x=="apr"){
      x <- as.factor("April")
    }
    else if (x=="may"){
      x <- as.factor("May")
    }
    else if (x=="jun"){
      x <- as.factor("June")
    }
    else if (x=="jul"){
      x <- as.factor("July")
    }
    else if (x=="sep"){
      x <- as.factor("September")
    }
    else if (x=="oct"){
      x <- as.factor("October")
    }
    else if (x=="nov"){
      x <- as.factor("November")
    }
    else if (x=="jan"){
      x <- as.factor("January")
    }
    else if (x=="dec"){
      x <- as.factor("December")
    }
    return(x)}
  )
# Question 4
df["Day_num"] <- NA
df$Day_num <- sapply(df$day, function(x) {
  if(x=="sun")
  {x <- as.numeric(1)
  }
  else if(x=="mon")
  {x <- as.numeric(2)
  }
  else if(x=="tue")
  {x <- as.numeric(3)
  }
  else if(x=="wed")
  {x <- as.numeric(4)
  }
  else if(x=="thu")
  {x <- as.numeric(5)
  }
  else if(x=="fri")
  {x <- as.numeric(6)
  }
  else if(x=="sat")
  {x <- as.numeric(7)
  }
  return(x)})
df$Day_num

# Question 5
?cor
cor(df$X, df$Y)
# Value of r indicates a relation between two variables. 
# Near to 1 perfect fit, near to -1 bad fit

# Question 6
# Total rain,wind group by Month
summarise(group_by(df,month), sum_wind= sum(wind), sum_rain = sum(rain),count = n())

# Question 7
summarise(group_by(df,month), mean_wind= mean(wind), mean_rain = mean(rain),count = n())

# Question 8
summarise(group_by(df,month),count = n())

# Question 9
unique(df[,c("month","day")])
count(df,"month")
summarise(group_by(df,month,day),count = n())
