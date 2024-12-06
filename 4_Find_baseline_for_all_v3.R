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




