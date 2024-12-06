###对目录下所有csv文件（未包含plot_filtering_report.txt中筛选出去的文件）寻找minimum adhesive force
# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组

# 读取报告文件
report_content <- readLines("plot_filtering_report.txt")

# 提取 "Files NOT plotted" 部分
start_line <- which(grepl("Files NOT plotted", report_content))
end_line <- which(grepl("Files with minimum value absolute greater", report_content))
not_plotted_files <- report_content[(start_line + 1):(end_line - 1)]

# 清理提取的文件名
not_plotted_files <- trimws(not_plotted_files)

# 获取所有匹配模式的文件列表
files <- list.files(pattern = "curve\\d+\\.csv")

# 从文件列表中去除未绘制的文件
files <- setdiff(files, not_plotted_files)

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# First read all data to determine the axes range
for (i in seq_along(files)) {
  file <- files[i]
  data <- read.csv(file, header = TRUE)
  
  subset_data <- data[data[, 5] > 0 & data[, 5] < 30, ]
  all_x <- c(all_x, data[, 5][1:500])  # Collect all x data except 500-512
  all_y <- c(all_y, data[, 6][1:500])  # Collect all y data except 500-512
  
  # Calculate the minimum y value for the filtered data
  if (nrow(subset_data) > 0) {  # Check if there are any data points after filtering
    min_y_value <- min(subset_data[, 6])
    min_values[i] <- min_y_value
  } else {
    min_values[i] <- NA  # Assign NA if no data points meet the criteria
  }
  
  
  # Optional: Debugging output
  print(paste("Processing file:", file))
  print(paste("Minimum y value calculated:", min_y_value))
}

# 创建一个数据框来存储文件名和对应的最小值
results_df <- data.frame(FileName = files, MinYValue = min_values)

# 输出数据框到CSV文件
write.csv(results_df, "Minimum_Adhesive_Force_Values.csv", row.names = FALSE)

# 打印出数据框以供检查
print(results_df)


min_force_values <- results_df$MinYValue
# 设置图形设备

generate_plots_for_vector <- function(data, title) {
  # 检查输入是否为数值向量
  if (!is.numeric(data)) {
    stop("Provided data must be a numeric vector.")
  }
  
  # 设置图形参数
  par(mfrow = c(1, 2)) # 设置图表布局为1行2列
  
  # 频率分布直方图
  hist(data, breaks = 20, main = paste(title, "Histogram"), xlab = title, col = "lightblue")
  abline(v = mean(data,na.rm = TRUE), col = "red", lwd = 2) # 在直方图上标记平均数
  abline(v = median(data,na.rm = TRUE), col = "blue", lwd = 2) # 在直方图上标记中位数

  # 箱型图
  boxplot(data, main = paste(title, "Boxplot"), ylab = title, col = "lightblue")
  points(1, mean(data,na.rm = TRUE), col = "red", pch = 19) # 在箱型图上标记平均数
  points(1, median(data,na.rm = TRUE), col = "blue", pch = 19) # 在箱型图上标记中位数
  
  # 添加数值标签
  text(1, mean(data,na.rm = TRUE), paste("Mean:", round(mean(data,na.rm = TRUE), 2)), pos = 3, col = "red", cex = 0.75)  # pos = 3 位于点的正上方
  text(1, median(data,na.rm = TRUE), paste("Median:", round(median(data,na.rm = TRUE), 2)), pos = 1, col = "blue", cex = 0.75) # pos = 1 位于点的正下方
  
}


generate_plots_for_vector(min_force_values, "Min Force values")
