###对目录下所有csv文件（未包含plot_filtering_report.txt中筛选出去的文件）使用梯形积分方法进行积分。对x轴下方与曲线围成的面积积分（计为负），对x轴上方与曲线围成的面积积分（第一象限，计为正）。再将两个积分加和。同时生成报告。
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

