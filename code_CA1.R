# Creating the data frame
student_scores <- data.frame(
  student = 1:17,
  no_visual_aids = c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  with_visual_aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)
str(student_scores)
View(student_scores)
# boxplot
boxplot(
  student_scores$no_visual_aids, student_scores$with_visual_aids,
  names = c("No Visual Aids", "With Visual Aids"),
  main = "Scores Comparison",
  ylab = "Scores",
  col = c("lightblue", "lightgreen")
)

differences <- student_scores$with_visual_aids - student_scores$no_visual_aids

# Q-Q plot for the differences
qqnorm(differences, main = "Q-Q Plot for Differences Between Visual Aids and No Visual Aids")
qqline(differences, col = "red", lwd = 2)

#Normality test
shapiro.test(no_visual_aids)
shapiro.test(with_visual_aids)
combined_scores <- c(no_visual_aids, with_visual_aids)
shapiro_result_combined <- shapiro.test(combined_scores)
shapiro_result_combined 
hist(no_visual_aids)
hist(with_visual_aids)

# Calculate the mean of the combined scores
mean_combined <- mean(combined_scores)
mean_combined #52.29412 is the mean_combined


# Calculate the median of the combined scores
median_combined <- median(combined_scores)
median_combined # 53 is the median_combined

# Load the e1071 package to calculate skewness
# If you haven't already installed it, you can install via: install.packages("e1071")
install.packages("e1071")
library(e1071)

# Calculate the skewness of the combined scores
skewness_combined <- skewness(combined_scores)
skewness_combined # -0.2033531 skewness_combined

# Descriptive statistics for both groups
mean_no_aids <- mean(student_scores$no_visual_aids)
mean_no_aids  # 46.94118
mean_with_aids <- mean(student_scores$with_visual_aids)
mean_with_aids # 57.64706
sd_no_aids <- sd(student_scores$no_visual_aids)
sd_no_aids # 13.11712
sd_with_aids <- sd(student_scores$with_visual_aids)
sd_with_aids # 13.52748

# Paired t-test
t_test_result <- t.test(student_scores$no_visual_aids, student_scores$with_visual_aids, paired = TRUE)
t_test_result


# Install and load the psych package
install.packages("psych")
library(psych)

# Filter relevant columns from the student_scores dataset
numeric_data <- student_scores[, c("no_visual_aids", "with_visual_aids")]

# Check the structure of the data
str(numeric_data)

# Correlation using the default pairs() function
pairs(numeric_data, labels = colnames(numeric_data), main = "Student Scores Correlation Plot")

# Enhanced correlation visualization using pairs.panels()
windows(16,20)
pairs.panels(
  numeric_data,
  smooth = TRUE,      # Draw less smooths
  scale = FALSE,      # Don't scale correlation text font
  density = TRUE,     # Adds density plots and histograms
  ellipses = TRUE,    # Draws confidence ellipses
  method = "pearson", # "pearson" for normal data (or "spearman" for non-normal)
  pch = 21,           # pch symbol
  lm = TRUE,          # Plots linear fit rather than smoothed fit
  cor = TRUE,         # Report correlations
  hist.col = 4,       # Histogram color
  stars = TRUE,       # Adds significance level with stars
  ci = TRUE           # Adds confidence intervals
)
