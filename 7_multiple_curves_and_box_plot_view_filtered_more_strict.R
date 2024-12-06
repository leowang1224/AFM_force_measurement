#可以实现的功能：将工作目录下所有的curve数字.csv都导入，且将所有csv中的retract数据绘制折线图。并且开始filter。filter的依据是：1.去掉所有曲线的500-512位置的数，即左边最顶端。2.曲线最小值绝对值的百分位数。比如去掉曲线最小值绝对值最大和最小的百分之十的曲线，此时设置percentile_value为0.9就好。3.去掉最小值点的横坐标小于0的曲线。
# Set working directory
# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.csv")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组

# First read all data to determine the axes range
for (file in files) {
  data <- read.csv(file, header = TRUE)
  all_x <- c(all_x, data[, 5][1:500])  # Collect all x data except 500-512
  all_y <- c(all_y, data[, 6][1:500])  # Collect all y data except 500-512
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.csv(files[i], header = TRUE)
  x_data <- data[, 5][1:500]
  y_data <- data[, 6][1:500]
  
  # Draw the line graph, specify color for each file
  lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  
  # Find the lowest point
  min_index <- which.min(y_data)
  min_value <- y_data[min_index]
  min_values[i] <- min_value  # Store minimum value
  min_x <- x_data[min_index]
  # Store min points for legend
  min_points[i, ] <- c(min_x, min_value)
  names[i] <- paste("Curve", sub("curve(\\d+)\\.csv", "\\1", files[i]))
}

# Add a legend to the plot
#legend("topright", legend = names, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)

# Open a new plot area for the boxplot in the top left corner
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")

dev.off()

# Start to filter
# Calculate the percentile of the absolute minimum values
abs_min_values <- abs(min_values)
percentile_value <- 0.9 # 在这里改变想要 filter 的阈值

# 计算10%和90%百分位的值
lower_cutoff_value <- quantile(abs_min_values, probs = 1-percentile_value)
upper_cutoff_value <- quantile(abs_min_values, probs = percentile_value)

# 找出需要排除的数据点索引，如：最小的10%和最大的10%
excluded_indices <- which(abs_min_values <= lower_cutoff_value | abs_min_values >= upper_cutoff_value)

# Additional filtering: exclude curves with min point x < 0
additional_excluded_indices <- which(min_points[, 1] < 0)

# 使用正则表达式匹配包含三组小数的文件名
pattern1 <- "^-?(\\d*\\.\\d+|\\d+\\.?\\d*[Ee][-+]?\\d+)_-?(\\d*\\.\\d+|\\d+\\.?\\d*[Ee][-+]?\\d+)_-?(\\d*\\.\\d+|\\d+\\.?\\d*[Ee][-+]?\\d+)_reversed_curve\\d+\\.csv$"
pattern_excluded_indices <- which(!grepl(pattern1, basename(files)))

####
# 打开文件以写入报告
report_file <- "exclusion_details_report.txt"
report_connection <- file(report_file, "w")

# 写入报告的头部信息
writeLines("Exclusion Report", con = report_connection)
writeLines("=============================================\n", con = report_connection)

# 写入截断值信息
writeLines(paste("Lower cutoff value (10%):", lower_cutoff_value), con = report_connection)
writeLines(paste("Upper cutoff value (90%):", upper_cutoff_value), con = report_connection)
writeLines("", con = report_connection) # 添加空行

# 处理第一组排除原因
writeLines("Files with minimum value absolute greater than or equal to upper cutoff or less than or equal to lower cutoff:", con = report_connection)
if (length(excluded_indices) > 0) {
  excluded_info <- paste(files[excluded_indices], abs_min_values[excluded_indices], sep=": ", collapse=", ")
  writeLines(excluded_info, con = report_connection)
} else {
  writeLines("None", con = report_connection)
}

writeLines("", con = report_connection) # 添加空行

# 处理第二组排除原因
writeLines("Files excluded because min point x < 0:", con = report_connection)
if (length(additional_excluded_indices) > 0) {
  additional_excluded_info <- paste(files[additional_excluded_indices], min_points[, 1][additional_excluded_indices], sep=": ", collapse=", ")
  writeLines(additional_excluded_info, con = report_connection)
} else {
  writeLines("None", con = report_connection)
}

writeLines("", con = report_connection) # 添加空行

# 处理第三组排除原因
writeLines("Files excluded because lack of sensitivity or baseline:", con = report_connection)
if (length(pattern_excluded_indices) > 0) {
  pattern_excluded_info <- paste(files[pattern_excluded_indices], collapse=", ")
  writeLines(pattern_excluded_info, con = report_connection)
} else {
  writeLines("None", con = report_connection)
}

# 关闭文件连接
close(report_connection)

# 输出文件保存成功的消息
cat("Exclusion report has been saved to:", report_file, "\n")
###

# Combine all exclusion criteria
all_excluded_indices <- unique(c(excluded_indices, additional_excluded_indices, pattern_excluded_indices))


# Redraw the plot excluding these curves
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points Excluding Top 10% and x < 0")

# Loop to redraw only the included curves
for (i in seq_along(files)) {
  if (!(i %in% all_excluded_indices)) {
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:500]
    y_data <- data[, 6][1:500]
    lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  }
}

# Redraw the boxplot excluding the top 10% of minimum values and x < 0
min_values_excluded <- min_values[!(seq_along(min_values) %in% all_excluded_indices)]
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values_excluded, main = "Boxplot of Minimum Values Excluding Top 10% and x < 0", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values_excluded, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median of the excluded values
median_val_excluded <- median(min_values_excluded, na.rm = TRUE)

# Add a point for median in the new boxplot
points(1, median_val_excluded, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point in the new boxplot
text(1, median_val_excluded, labels = sprintf("Median: %.2f", median_val_excluded), pos = 3, cex = 0.8, col = "red")

# 初始化存储未绘制和已绘制文件名的向量
excluded_files <- vector("character")
included_files <- vector("character")

# 遍历每个文件，为每个文件单独创建一个图形
for (i in seq_along(files)) {
  if (!(i %in% all_excluded_indices)) {
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:500]
    y_data <- data[, 6][1:500]
    included_files <- c(included_files, files[i])
  } else {
    # 添加未绘制的文件名到向量
    excluded_files <- c(excluded_files, files[i])
  }
}

# 创建报告文本内容
report <- paste("Plot Report\n",
                "Lower Cutoff Value: ", (1 - percentile_value) * 100, "% / ", lower_cutoff_value, "\n",
                "Upper Cutoff Value: ", percentile_value * 100, "% / ", upper_cutoff_value, "\n",
                "Files plotted: \n", paste(included_files, collapse = "\n"), "\n",
                "Files NOT plotted (the minimum force value above upper cutoff value or below lower cutoff value or x < 0): \n", paste(excluded_files, collapse = "\n"), "\n",
                "Files with minimum value absolute greater than or equal to upper cutoff or less than or equal to lower cutoff or with min x < 0 have been filtered.\n",
                sep="")

# 打印和保存报告
writeLines(report, "plot_filtering_report.txt")
cat(report)  # 输出报告到控制台