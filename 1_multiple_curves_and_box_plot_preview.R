#可以实现的功能：将工作目录下所有的curve数字.txt都导入，且将所有txt中的retract数据绘制折线图。并且计算所有折线图的最低点，并且绘制箱型图于左上角。
# Set working directory
setwd("D:/your_folder_path")  # Change this to your working directory

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.txt")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组


# First read all data to determine the axes range
for (file in files) {
  data <- read.table(file, header = TRUE)
  all_x <- c(all_x, data[, 2])  # Collect all x data
  all_y <- c(all_y, data[, 4])  # Collect all y data
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.table(files[i], header = TRUE)
  x_data <- data[, 2]
  y_data <- data[, 4]
  
  # Draw the line graph, specify color for each file
  lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  
  # Find the lowest point
  min_index <- which.min(y_data)
  min_value <- y_data[min_index]
  min_values[i] <- min_value  # Store minimum value
  min_x <- x_data[min_index]
  # Store min points for legend
  min_points[i, ] <- c(min_x, min_value)
  names[i] <- paste("Curve", sub("curve(\\d+)\\.txt", "\\1", files[i]))
  
 
}

# Add a legend to the plot
#legend("topright", legend = names, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)


# Open a new plot area for the boxplot in the top left corner
par(fig = c(0.1, 0.4, 0.5, 0.85), new = TRUE,mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")