### 1. Covariance and Correlation
A <- c(18, 2, 20)
B <- c(22, 28, 10)
C <- c(20, 40, 40)

# Covariance between B and C
cov(B, C)

# Covariance matrix
prefs <- data.frame(A, B, C)
cov(prefs)

# Correlation between B and C
cor(B, C)

# Correlation matrix
cor(prefs)

### Equal-frequency partitioning and smoothing
data <- c(18,18,18,20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,30,30)
num_bins <- 3
bins <- cut(data, breaks=num_bins, labels=FALSE)

# Bin means
bin_mean <- tapply(data, bins, mean)
smoothed_mean <- bin_mean[bins]

# Bin boundaries
bin_boundaries <- tapply(data, bins, function(x) c(min(x), max(x)))
smoothed_boundaries <- unlist(bin_boundaries[bins])

# Histogram
hist(data, breaks=num_bins, col="lightblue", main="Equal-Frequency Partitioning", xlab="Values")

### 2. All Electronics Prices
prices <- c(1,1,5,5,5,5,5,8,8,10,10,10,10,12,14,14,14,15,15,15,15,15,15,18,18,18,18,18)
summary(prices)
hist(prices, col="orange", main="All Electronics Prices", xlab="Price")

### 3. Maths Teachers Comparison
classA <- c(76,35,47,64,95,66,89,36,84)
classB <- c(51,56,84,60,59,70,63,66,50)

mean(classA); median(classA); range(classA)
mean(classB); median(classB); range(classB)

boxplot(classA, classB, names=c("Class A","Class B"), col=c("lightblue","lightgreen"), main="Class Performance")

### 4. Normalization Example
# Min-max normalization for v=80 with min=50000, max=100000
v <- 80000
min_val <- 50000
max_val <- 100000
min_max_norm <- (v - min_val) / (max_val - min_val)
min_max_norm

# Group data normalization
x <- c(200,300,400,600,1000)
min_max <- (x - min(x)) / (max(x) - min(x))
z_score <- (x - mean(x)) / sd(x)
min_max
z_score

### 5. AirPassengers Histogram
hist(AirPassengers, breaks=seq(100,700,150), col="purple", main="AirPassengers Histogram", xlab="Passengers")

### 6. Multiple Lines in Line Chart (mtcars)
plot(mtcars$mpg, type="l", col="blue", ylab="Values", xlab="Index", main="Multiple Line Chart")
lines(mtcars$qsec, col="red")
legend("topright", legend=c("mpg","qsec"), col=c("blue","red"), lty=1)

### 7. Water Dataset Linear Regression
install.packages("HSAUR3")
library(HSAUR3)
data("water")
head(water)

data(water)
plot(water$hardness, water$mortality, main="Mortality vs Hardness", xlab="Hardness", ylab="Mortality")
model <- lm(mortality ~ hardness, data=water)
abline(model, col="red")
predict(model, data.frame(hardness=88))

### 8. Boxplot mpg vs cyl (mtcars)
boxplot(mpg ~ cyl, data=mtcars, col=c("lightblue","lightgreen","pink"), main="MPG vs Cylinders")

### 9. Tennis Coach Outliers
scores <- c(15,18,20,22,25,30,35,40,100) # example data with outlier
boxplot(scores, main="Tennis Players Scores", col="orange")

### 10. Diabetes Dataset Visualization
diabetes <- read.csv("diabeties.csv")
plot(diabetes$Age, diabetes$BloodPressure, main="Blood Pressure vs Age", xlab="Age", ylab="Blood Pressure", col="blue")
barplot(diabetes$BloodPressure, names.arg=diabetes$Age, col="green", main="Blood Pressure by Age")
