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






