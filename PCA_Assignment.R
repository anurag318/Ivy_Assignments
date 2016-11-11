getwd()
train<- read.csv("Big_Mart_Train.csv")
test<- read.csv("Big_Mart_Test.csv")
#train <- read.csv("C:/Users/hp/Desktop/Ivy_R/Class6/Big_Mart_Train.csv")
#test <- read.csv("C:/Users/hp/Desktop/Ivy_R/Class6/Big_Mart_Test.csv")

test$Item_Outlet_Sales <- 1

#combine the data set
combi <- rbind(train, test)

#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),
                                combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1] <- "Other"

#remove the dependent and identifier variables
my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier,
                                     Outlet_Identifier))

#check available variables
colnames(my_data)

#check variable class
str(my_data)

#load library
install.packages("dummies")
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type"))

#check the data set
str(new_my_data)

#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]

#PCA 

prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

prin_comp$sdev
prin_comp$rotation
prin_comp$center
prin_comp$scale
prin_comp$x
dim(prin_comp$x)

biplot(prin_comp)

#Compute Standard Deviation

std_dev <- prin_comp$sdev

#Compute variance

var <- std_dev^2
var[1:10]

#Compute proportion of Variance

prop_var <- var/sum(var)
prop_var[1:20]

#Screen Plot

plot(prop_var, xlab = "Principle Component", ylab = "Proportion of Variance")

#Cumulative Screen Plot

plot(cumsum(prop_var), xlab = "Principle Component", ylab = "Proportion of Variance")

#As it is visible from graph, taht 30 PCA components accounty for more than 98% variance in data.

