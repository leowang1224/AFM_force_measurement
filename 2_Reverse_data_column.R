#可以实现的功能：将y轴数据从后往前排列
# 加载必要的库
library(tidyverse)

# 设置工作目录
setwd("/Users/zhaoyangwang/Desktop/GW_PhD_Research/R_workplace/AFM_data_analysis/test")  # 修改为你的文件目录

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




















