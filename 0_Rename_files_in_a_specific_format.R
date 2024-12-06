#可以实现的功能：将“数字.spm.txt”结尾的文件改成对应的“curve数字.txt”
#获取当前目录
getwd() 
# 设置工作目录
setwd("/Users/zhaoyangwang/Desktop/GW_PhD_Research/R_workplace/AFM_data_analysis/0606/PEG/pos1")

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