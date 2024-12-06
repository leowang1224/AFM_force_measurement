#可以实现的功能：将“数字.spm.txt”结尾的文件改成对应的“curve数字.txt”
#获取当前目录
getwd() 
setwd(getwd())
# Section 1 rename ---- 
# 获取当前工作目录下的所有文件名，筛选出符合特定模式的文件
files <- list.files(pattern = "\\d+\\.spm\\.txt$")

# 循环遍历符合条件的文件并重命名
for (file in files) {
  # 使用正则表达式匹配末尾的数字
  parts <- regmatches(file, regexec("(\\d+)\\.spm\\.txt$", file))
  index <- parts[[1]][2]  # 获取匹配的数字部分
  
  # 构造新文件名
  new_name <- paste0("curve", index, ".txt")
  
  # 重命名文件
  if (file.rename(file, new_name)) {
    cat("Renamed", file, "to", new_name, "\n")
  } else {
    cat("Failed to rename", file, "\n")
  }
}

# Section 2 multi preview ----
#可以实现的功能：将工作目录下所有的curve数字.txt都导入，且将所有txt中的retract数据绘制折线图。并且计算所有折线图的最低点，并且绘制箱型图于左上角。
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





# Section 3 reverse ----
#可以实现的功能：将y轴数据从后往前排列
# 加载必要的库
library(tidyverse)

# 读取目录下所有文件的名称
files <- list.files(pattern = "curve\\d+\\.txt")

# 遍历所有文件
for (file in files) {
  # 读取文件，假设是以制表符分隔
  data <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  # 检查是否有足够的列
  if (ncol(data) >= 4) {
    # 将第 4 列的数据从后往前排列
    reversed_column <- rev(data[[4]])
    
    # 覆盖第 4 列
    data[[4]] <- reversed_column
    
    # 生成新的文件名
    new_file_name <- gsub("curve(\\d+)\\.txt$", "reversed_curve\\1.txt", file)
    
    # 保存修改后的文件，使用相同的分隔符
    write.table(data, new_file_name, row.names = FALSE, sep = "\t", quote = FALSE)
    
    # 打印原始文件名和新文件名
    cat(sprintf("Processed: %s -> %s\n", file, new_file_name))
  } else {
    cat(sprintf("Skipping %s, not enough columns.\n", file))
  }
}

# Section remove curve.txt ----
file.remove(list.files(pattern = "^curve\\d+\\.txt$"))
# Section 4 find se... ----
#可以实现的功能：对所有“任意.curve1.txt”类似的文件进行sensitivity和intercept计算，并且将计算的结果写在该文件的文件名前面，类似于“sensitivity_intercept_reversed_curve数字.txt”，并且生成一个summary记录改名的过程。
library(dplyr)
library(readr)
library(stringr)
# 读取目录下所有文件的名称
files <- list.files(pattern = "curve\\d+\\.txt")

# 寻找灵敏度的函数
calc_sensitivity_from_last_30 <- function(x, y) {
  # 确保x和y的长度足够
  if (length(x) < 30 || length(y) < 30) {
    stop("Not enough data points.")
  }
  
  # 截取最后32个点，然后最后12个不要
  x_range_to_find_sensitivity <- tail(x, 32)[1:20]
  y_range_to_find_sensitivity <- tail(y, 32)[1:20]
  
  best_sensitivity <- NA
  best_r_squared <- -1  # 初始化R²值为一个非法值，确保任何有效的R²值都会更新它
  best_interval <- NULL
  best_intercept <- NA
  
  
  # 从选中点中检查，长度至少为8点，最多为15点
  for (start in 1:(20-8+1)) {
    for (length in 8:min(15, 20-start+1)) {
      # 选取从start开始的length长度的点
      current_x <- x_range_to_find_sensitivity[start:(start+length-1)]
      current_y <- y_range_to_find_sensitivity[start:(start+length-1)]
      
      
      
      # 进行线性回归
      model <- lm(current_y ~ current_x)
      r_squared <- summary(model)$r.squared
      
      # 检查是否是最好的模型
      if (r_squared > best_r_squared) {
        best_r_squared <- r_squared
        best_model <- model
        best_sensitivity <- -coefficients(best_model)['current_x']
        best_interval <- list(start = start, end = start + length - 1)
        best_intercept <- coef(best_model)[1]
      }
    }
  }
  
  if (!is.null(best_model)) {
    # 获取全局索引范围
    global_start <- length(x) - 32 + best_interval$start
    global_end <- length(x) - 32 + best_interval$end
    return(list(sensitivity = best_sensitivity, r_squared = best_r_squared, interval = c(global_start, global_end),intercept = best_intercept))
  } else {
    return(list(sensitivity = NA, r_squared = NA, interval = NA, intercept = NA))
  }
}



# 函数，处理单个文件
process_file <- function(file) {
  # 读取数据
  data <- read_delim(file, delim = "\t", col_names = TRUE)
  
  # 确保数据列足够
  if (ncol(data) < 4) {
    cat("File", file, "does not have enough columns.\n")
    return(NULL)
  }
  
  # 使用第2列作为 x，第4列作为 y
  x <- data[[2]]
  y <- data[[4]]
  
  # 计算灵敏度
  result <- calc_sensitivity_from_last_30(x, y)
  
  # 检查计算结果
  if (is.na(result$sensitivity)) {
    cat("Sensitivity calculation failed for", file, "\n")
    return(NULL)
  }
  
  # 保留四位小数
  sensitivity_formatted <- format(round(result$sensitivity, 4), nsmall = 4)
  
  # 储存对应线性回归区间
  interval_1 <- result$interval[1]
  interval_2 <- result$interval[2]
  
  # 储存对应截距
  intercept <- format(round(result$intercept, 4), nsmall = 4)
  
  # 构造新文件名
  new_file_name <- paste0(sensitivity_formatted, "_", intercept, "_", file)
  
  # 重命名文件
  file.rename(file, new_file_name)
  
  # 打印结果
  cat("File", file, "renamed to", new_file_name, "\n")
  
  return(list(original_name = file, new_name = new_file_name, sensitivity = sensitivity_formatted, interval_1 = interval_1, interval_2 = interval_2, intercept = intercept))
}

# 处理每个文件
results <- lapply(files, process_file)

# 保存结果到一个 CSV 文件
results_df <- do.call(rbind.data.frame, results)
write_csv(results_df, "sensitivity_and_intercept_results.csv")



# Section 5 find bl ---- 
#可以实现的功能：寻找baseline。对所有“任意.curve1.txt”类似的文件寻找baseline，并且将计算的结果写在该文件的文件名前面。并且生成一个summary记录改名的过程。
#以第100个点和第400个点为范围，每200个点为一组，10个点为间隔，计算斜率和标准误，查看平不平。
library(dplyr)
library(readr)
library(stringr)
# 读取目录下所有文件的名称
files <- list.files(pattern = "curve\\d+\\.txt")

# 寻找baseline的函数
find_baseline <- function(least_length, sensitivity, x, y) {
  # 检查输入长度是否足够
  if (length(x) < 400 || length(y) < 400) {
    stop("Input vectors must have at least 300 elements.")
  }
  
  # 调整 y 值
  y_sens_crct <- y / sensitivity
  
  # 初始化变量
  base_x <- NULL
  base_y <- NULL
  baseline_start_position <- NULL
  
  # 迭代每个窗口
  for (start in seq(100, 201, by = 10)) {
    window_x <- x[start:(start + 299)]
    window_y <- y_sens_crct[start:(start + 299)]
    
    # 进行线性回归
    model <- lm(window_y ~ window_x)
    
    # 提取斜率和标准误差
    slope <- coef(model)[["window_x"]]
    stderr <- summary(model)$coefficients["window_x", "Std. Error"]
    
    # 检查斜率和标准误差是否符合平稳性条件
    if (abs(slope) < 0.001 && stderr < 0.005) {
      base_x <- window_x
      base_y <- window_y * sensitivity  # 将 y 值调整回原始比例
      baseline_start_position <- start
      break
    }
  }
  
  if (is.null(base_x) || is.null(base_y)) {
    return(list(baseline = NA, start_position = NA))
  } else {
    baseline <- mean(base_y)
    return(list(baseline = baseline, start_position = baseline_start_position))
  }
}



# 函数，处理单个文件
process_file <- function(file) {
  # 读取数据
  data <- read_delim(file, delim = "\t", col_names = TRUE)
  
  # 确保数据列足够
  if (ncol(data) < 4) {
    cat("File", file, "does not have enough columns.\n")
    return(NULL)
  }
  
  # 使用第2列作为 x，第4列作为 y
  x <- data[[2]]
  y <- data[[4]]
  
  # 计算灵敏度
  result <- find_baseline(200,1,x, y)
  
  # 检查计算结果
  if (is.na(result$baseline)) {
    cat("Baseline calculation failed for", file, "\n")
    return(NULL)
  }
  
  # 保留四位小数
  baseline_formatted <- format(round(result$baseline, 4), nsmall = 4)
  
  
  # 构造新文件名
  new_file_name <- paste0(baseline_formatted, "_", file)
  
  # 重命名文件
  file.rename(file, new_file_name)
  
  # 打印结果
  cat("File", file, "renamed to", new_file_name, "\n")
  
  return(list(original_name = file, new_name = new_file_name, baseline = baseline_formatted))
}

# 处理每个文件
results <- lapply(files, process_file)

# 保存结果到一个 CSV 文件
results_df <- do.call(rbind.data.frame, results)
write_csv(results_df, "baseline_results.csv")



# Section 6 final calculation and to csv ----
#可以实现的功能：可以对目录下所有以“_curve数字.txt”结尾的文件进行处理。处理是：将文件名从前往后第一组四位小数定义为bl,第二组四位小数定义为se，第三组四位小数定义为in。之后用文件内第2列数都减去(bl-in)/-se得到的数再加上de，de是第4列的数减去bl的差除以se得到的结果。最终得到的结果定义为seperation，对应填充到第五列。之后，再用de乘以sc,sc是这个函数一开始需要给定的数值，de与sc的积定义为force，对应填充到第6列。

process_files <- function(sc) {
  # 获取当前工作目录
  directory <- getwd()
  
  # 获取目录中所有以"_curve数字.txt"结尾的文件
  files <- list.files(path = directory, pattern = "_curve\\d+\\.txt$", full.names = TRUE)
  
  for (file in files) {
    # 从文件名中提取四位小数
    file_name <- basename(file)
    file_parts <- unlist(strsplit(file_name, "_")) # 拆分文件名
    bl <- as.numeric(file_parts[1])
    se <- as.numeric(file_parts[2])
    inter <- as.numeric(file_parts[3])
    
    # 读取文件内容
    data <- read.table(file, header = TRUE)
    
    # 将数据转换为数值型
    data <- as.data.frame(sapply(data, as.numeric))
    
    # 计算de
    de <- (data[, 4] - bl) / se
    
    # 计算seperation
    seperation <- data[, 2] - ((bl - inter) / -se) + de
    
    # 计算force
    force <- de * sc
    
    # 添加新列到数据框
    data[,5] <- seperation #这里是并上了，而不是覆盖了。找到问题了
    data[,6] <- force
    
    # 设置列名
    colnames(data) <- c("Calc_Ramp_Ex_nm", "Calc_Ramp_Rt_nm", "Defl_V_Ex", "Defl_V_Rt", "seperation", "force")
    
    # 写回文件，转换为CSV格式
    write.csv(data, gsub(".txt$", ".csv", file), row.names = FALSE)
  }
}

# 使用函数，假设sc为给定值，比如0.08
process_files(0.06)
# Section devoff ----
dev.off()
# Section 7 multi view ----
#可以实现的功能：将工作目录下所有的curve数字.csv都导入，且将所有csv中的retract数据绘制折线图。并且计算所有折线图的最低点，并且绘制箱型图于左上角。

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
  all_x <- c(all_x, data[, 5])  # Collect all x data
  all_y <- c(all_y, data[, 6])  # Collect all y data
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.csv(files[i], header = TRUE)
  x_data <- data[, 5]
  y_data <- data[, 6]
  
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
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")


# Section devoff ----
dev.off()
# Section 8 multi view filtered ----
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

# Section 9 individually view ----
#可以实现的功能：单独对每个在总图中出现的处理过的数据进行绘图，且保存。用于人工筛选想要的图和文件。
# 创建用于保存图形的文件夹，如果不存在就创建一个
plot_dir <- "individual_plot_for_selection"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.csv")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# 遍历每个文件，为每个文件单独创建一个图形
for (i in seq_along(files)) {
  # 设置PDF输出文件路径
  pdf_file_path <- file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(files[i])), ".pdf"))
  
  # 开始PDF绘图设备，指定文件大小为10x12英寸
  tryCatch({
    pdf(pdf_file_path, width = 12, height = 10)
    
    # 绘制图形
    plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
         xlab = "X Axis", ylab = "Y Axis", main = paste("Curve Analysis for", basename(files[i])))
    
    # 从对应文件中读取数据
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:512]
    y_data <- data[, 6][1:512]
    
    # 绘制线条
    lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
    
    # 找出最小值及其索引
    min_index <- which.min(y_data)
    min_value <- y_data[min_index]
    min_x_value <- x_data[min_index]
    
    # 在图上标注最小值
    points(min_x_value, min_value, col = "red", pch = 19, cex = 1.5)
    text(min_x_value, min_value, labels = sprintf("Min: %.2f", min_value), pos = 1, offset = 1, col = "red")
    
  }, finally = {
    # 确保每次都关闭PDF设备
    dev.off()
  })
}






# Section devoff ----
dev.off()
# Section 10 integration ----
###对目录下所有csv文件（未包含plot_filtering_report.txt中筛选出去的文件）使用梯形积分方法进行积分。对x轴下方与曲线围成的面积积分（计为负），对x轴上方与曲线围成的面积积分（第一象限，计为正）。再将两个积分加和。同时生成报告。
# 指定你要检测和加载的包名
package_name <- "pracma" 

# 检测包是否安装
if (!require(package_name, character.only = TRUE)) {
  # 如果包没有安装，安装它
  install.packages(package_name)
  
  # 安装后加载该包
  library(package_name, character.only = TRUE)
} else {
  # 如果包已经安装，直接加载
  library(package_name, character.only = TRUE)
}
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
for (file in files) {
  data <- read.csv(file, header = TRUE)
  all_x <- c(all_x, data[, 5][1:500])  # Collect all x data except 500-512
  all_y <- c(all_y, data[, 6][1:500])  # Collect all y data except 500-512
}


plot_dir <- "individual_plot_for_integration"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# 初始化存储结果的向量
left_areas <- numeric(length(files))
right_areas <- numeric(length(files))
total_areas <- numeric(length(files))

###只有左边没有右边
for (i in seq_along(files)) {
  pdf_file_path <- file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(files[i])), ".pdf"))
  
  tryCatch({
    pdf(pdf_file_path, width = 12, height = 10)
    
    # 从对应文件中读取数据
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:512]
    y_data <- data[, 6][1:512]
    
    # 绘图
    plot(x_data, y_data, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
         xlab = "Separation Distance (nm)", ylab = "Interaction Force (nN)",
         main = paste("Curve Analysis for", basename(files[i])))
    lines(x_data, y_data, col = colors[i], lwd = 2)
    
    
    # 绘制通过原点的 X 和 Y 轴
    abline(h = 0, col = "blue", lwd = 0.5)  # 水平轴，通过y=0
    abline(v = 0, col = "blue", lwd = 0.5)  # 垂直轴，通过x=0
    
    # 计算零点交叉
    crossings <- which(diff(sign(y_data)) != 0)
    # 使用线性插值寻找零点
    zero_crossings_x <- numeric(length(crossings))
    for (j in 1:length(crossings)) {
      x1 <- x_data[crossings[j]]
      x2 <- x_data[crossings[j] + 1]
      y1 <- y_data[crossings[j]]
      y2 <- y_data[crossings[j] + 1]
      zero_crossings_x[j] <- x1 - (y1 * (x2 - x1) / (y2 - y1))
    }
    
    # 确定正零点
    positive_zero_crossings_x <- zero_crossings_x[zero_crossings_x > 0]
    sorted_zero_crossings <- sort(positive_zero_crossings_x)
    
    if (length(sorted_zero_crossings) >= 1) {
      min_positive_x <- sorted_zero_crossings[1]
      if (length(sorted_zero_crossings) >= 2) {
        second_positive_x <- min(sorted_zero_crossings[2], 30)
      } else {
        second_positive_x <- 30
      }
    }
    
    # 计算区域#这里有很大的问题
    indices_left <- which(x_data < min_positive_x)
    indices_right <- which(x_data > min_positive_x & x_data <= second_positive_x)
    
    area_left <- -trapz(x_data[indices_left], y_data[indices_left])
    area_right <- -trapz(x_data[indices_right], y_data[indices_right])
    
    # 存储计算的区域值
    left_areas[i] <- area_left
    right_areas[i] <- area_right
    total_areas[i] <- area_left + area_right
    
    # 绘制区域
    polygon(c(x_data[indices_left], rev(x_data[indices_left])), c(y_data[indices_left], rep(0, length(indices_left))), col = "lightblue", border = NA)
    polygon(c(x_data[indices_right], rev(x_data[indices_right])), c(y_data[indices_right], rep(0, length(indices_right))), col = "lightcoral", border = NA)
    
    # 在右下角添加面积标注
    mtext(sprintf("Left Area: %.2f", area_left), side = 1, line = -1, at = par("usr")[2], adj = 1, col = "blue", cex = 0.8)
    mtext(sprintf("Right Area: %.2f", area_right), side = 1, line = -2, at = par("usr")[2], adj = 1, col = "red", cex = 0.8)
    
    # 计算总面积
    total_area <- area_left + area_right
    
    # 添加总面积标注，位于两行之上，使用加粗字体
    mtext(sprintf("Total Area: %.2f", total_area), side = 1, line = -3, at = par("usr")[2], adj = 1, col = "black", cex = 0.8, font = 2)
    
  }, finally = {
    dev.off()
  })
}


# 生成报告 CSV 文件
integration_report <- data.frame(
  File = basename(files),
  LeftArea = left_areas,
  RightArea = right_areas,
  TotalArea = total_areas
)
write.csv(integration_report, "integration_report.csv", row.names = FALSE)

# 生成图表函数
generate_plots <- function(data, title, file_name) {
  par(mfrow = c(1, 2)) # 设置图表布局为1行2列
  
  # 频率分布直方图
  hist(data, breaks = 20, main = paste(title, "Histogram"), xlab = title, col = "lightblue")
  abline(v = mean(data), col = "red", lwd = 2) # 标记平均数
  abline(v = median(data), col = "blue", lwd = 2) # 标记中位数
  
  # 箱型图
  boxplot(data, main = paste(title, "Boxplot"), ylab = title, col = "lightblue")
  points(1, mean(data), col = "red", pch = 19) # 标记平均数
  points(1, median(data), col = "blue", pch = 19) # 标记中位数
  
  # 添加数值标签
  text(1, mean(data), paste("Mean:", round(mean(data), 2)), pos = 3, col = "red", cex = 0.75)  # pos = 3 位于点的正上方
  text(1, median(data), paste("Median:", round(median(data), 2)), pos = 1, col = "blue", cex = 0.75) # pos = 1 位于点的正下方
  
  
  dev.copy(png, file_name) # 保存图表
  dev.off()
}

# 生成left area图表
generate_plots(left_areas, "Left Area", "left_area_plot.png")

# 生成right area图表
generate_plots(right_areas, "Right Area", "right_area_plot.png")

# 生成total area图表
generate_plots(total_areas, "Total Area", "total_area_plot.png")

# Section devoff ----
dev.off()

# Section 11 minimum adhesive force ----
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