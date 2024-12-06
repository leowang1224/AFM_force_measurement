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
process_files(0.08)













