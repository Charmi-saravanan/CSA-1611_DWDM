### Q1
age <- c(5,15,20,50,80,110)
frequency <- c(200, 450, 300, 1500, 700, 44)
median(age)
median(frequency)

### Q2
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
mean(age)
median(age)
mode_age <- names(table(age))[table(age) == max(table(age))]
mode_age
range(age)
quantile(age, .25)
quantile(age, .75)

### Q3
x <- c(200, 300, 400, 600, 1000)
min_max_norm <- (x - min(x)) / (max(x) - min(x))
min_max_norm
z_score_norm <- (x - mean(x)) / sd(x)
z_score_norm

### Q4
data <- c(11,13,13,15,15,16,19,20,20,20,21,21,22,23,24,30,40,45,45,45,71,72,73,75)
bins <- 5
bin_indices <- cut(data, bins)
mean_smooth <- tapply(data, bin_indices, mean)
print(mean_smooth)
median_smooth <- tapply(data, bin_indices, median)
median_smooth
min_max_smooth <- tapply(data, bin_indices, function(x) c(min(x), max(x)))
print(min_max_smooth)

### Q5
age <- c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)
fat <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7)
mean(age); median(age); sd(age)
mean(fat); median(fat); sd(fat)
boxplot(age, fat)
scatter.smooth(age, fat)
qqplot(age, fat)

### Q6
v <- c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)
min_max <- (35 - min(v)) / (max(v) - min(v))
print(min_max)
m <- mean(v)
s <- 12.94
z_score <- (35 - m) / s
print(z_score)
m <- 35
j <- max(m) < 1
decimal_scaling <- m / 10^j
print(decimal_scaling)

### Q7
pencils <- c(9,25,23,12,11,6,7,8,9,10)
mean(pencils)
median(pencils)
mode <- names(table(pencils))[table(pencils) == max(table(pencils))]
mode

### Q8
x <- c(4,1,5,7,10,2,50,25,90,36)
y <- c(12,5,13,19,31,7,153,72,275,110)
scatter.smooth(x, y)

### Q9
marks <- c(55,60,71,63,55,65,50,55,58,59,61,63,65,67,71,72,75)
num_bins <- 3
bins_eq_frequency <- cut(marks, breaks = num_bins, labels = FALSE)
hist(marks, breaks = num_bins, col = "lightblue", xlab = "Marks", main = "Equal-Frequency (Equi-Depth) Partitioning")
bin_mean <- tapply(marks, cut(marks, num_bins), mean)
smoothed_data_by_mean <- unname(bin_mean[as.character(cut(marks, num_bins))])
bin_median <- tapply(marks, cut(marks, num_bins), median)
smoothed_data_by_median <- unname(bin_median[as.character(cut(marks, num_bins))])
bin_boundaries <- tapply(marks, cut(marks, num_bins), function(x) c(min(x), max(x)))
smoothed_data_by_boundaries <- unlist(bin_boundaries[as.character(cut(marks, num_bins))])
print("Original data: "); print(marks)
print("Smoothed data by bin mean: "); print(smoothed_data_by_mean)
print("Smoothed data by bin median:"); print(smoothed_data_by_median)
print("Smoothed data by bin boundaries: "); print(smoothed_data_by_boundaries)

### Q10
v <- c(78.3,81.8,82,74.2,83.4,84.5,82.9,77.5,80.9,70.6)
IQR(v)
sd(v)

### Q11
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
quantile(age, .25)
quantile(age, .75)
