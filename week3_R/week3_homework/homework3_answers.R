# exercise 1.1
attenu[is.na(attenu$station),]
attenu_cleaned <- attenu[!(is.na(attenu$station)),]

# new attenu_cleaned summaries
View(attenu_cleaned)
head(attenu_cleaned)
dim(attenu_cleaned)

# exercise 1.2
Theoph_2 <- Theoph
View(Theoph_2)
str(Theoph_2)
med <- median(Theoph_2$Dose)
Theoph_2$Dose_Class <- ifelse(Theoph_2$Dose >= med, 
                              "high", 
                              "low")
head(Theoph_2)
dim(Theoph_2)

# exercise 1.3 (not done)
getwd()
setwd("/Users/joshlu/Documents/USC/Classes/QBIO/qbio_data_analysis_joshl/week3_R/week3_homework")
starbucks <- read.csv('starbucks.csv')
View(starbucks)

is.na(starbucks)
is_row_empty <- rowSums(is.na(starbucks[,2:7])) == 0

length(is_row_empty) 
nrow(starbucks) 
starbucks_cleaned <- starbucks[is_row_empty == TRUE,]

plot(starbucks_cleaned$Carb, starbucks_cleaned$Calories, xlab = "Carbohydrates (g)", ylab = "Calories", 
     main = "Carbs vs. Calories in Starbucks Drinks")

max(starbucks_cleaned$Calories)
starbucks_cleaned[starbucks_cleaned$Calories == 430,]

plot(starbucks_cleaned$Carb, starbucks_cleaned$Calories, xlab = "Carbohydrates (g)", ylab = "Calories", 
     main = "Carbs vs. Calories in Starbucks Drinks", col=ifelse(starbucks_cleaned$Calories == 430, "red", "black"))
starbucks_cleaned$is_highest_fat <- ifelse(starbucks_cleaned$Fat == max(starbucks_cleaned$Fat), "TRUE", "FALSE")
View(starbucks_cleaned)
plot(starbucks_cleaned$Carb, starbucks_cleaned$Calories, xlab = "Carbohydrates (g)", ylab = "Calories", 
     main = "Carbs vs. Calories in Starbucks Drinks", col=ifelse(starbucks_cleaned$is_highest_fat == "TRUE", "red", "black"))

# exercise 1.4
getwd()
baseball <- read.csv(file = "Batting.csv")
View(baseball)
nrow(baseball[baseball$HR > 3,])

plot(baseball$yearID, baseball$HR, ylab = "homeruns", xlab = "year", main = "homeruns vs. year")

LA_Angels <- baseball[baseball$teamID == "LAA",]
View(LA_Angels)
plot(LA_Angels$yearID, LA_Angels$HR, ylab = "homeruns", xlab = "year", main = "homeruns vs. year for LA Angels")

ATL_PIT <- baseball[baseball$teamID == "ATL" | baseball$teamID == "PIT",]
plot(ATL_PIT$yearID, ATL_PIT$HR, ylab = "homeruns", xlab = "year", main = "homeruns vs. year for ATL and PIT",
     col=ifelse(ATL_PIT$teamID == "ATL", "red", "blue"))

# exercise 1.5
easy_plot <- function(x, y, color_data) {
  color_med <- median(color_data)
  levels <- c(ifelse(color_data < color_med, "low", "high"))
  levels = factor(levels)
  plot(x, y, col = levels, psch = 20)
  print(color_med)
}

easy_plot(starbucks_cleaned$Calories, starbucks_cleaned$Fat, starbucks_cleaned$Sodium)

# exercise 2.1
# The data set describes three different species of flowers
# (setosa, versicolor, and virginica) and the respective
# sepal length & width, as well as petal length and width. 
# There are 150 different flower data points, each describing 
# the above 5 variables.

# exercise 2.2 
# Sepal and petal length & width (double) are continuous variables. 
# Flower species (integer) is a categorical variable.

# exercise 2.3
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)
# Sepal Length and Width have a generally bell-shaped distribution.
# On the other hand, Petal Length and Width show 

# exercise 2.4 
mean_sep_wid <- mean(iris$Sepal.Width)
iris_copy <- iris

sep_compare <- ifelse(iris$Sepal.Width > mean_sep_wid, "wide", "narrow")
iris$comparison <- sep_compare

boxplot(iris$Sepal.Width ~ iris$comparison, xlab = "Comparison with Mean", 
        ylab = "Sepal Width (cm)", 
        main = "Sepal Width in Comparison with the Mean Width")

# exercise 2.5
?pairs
args(pairs)
pairs(iris[,1:4],col=iris[,5])

# exercise 3.1 (done -- biolinks installed!)
install.packages("BiocManager")
library(BiocManager)
BiocManager::install("TCGAbiolinks")
