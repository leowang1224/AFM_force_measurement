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