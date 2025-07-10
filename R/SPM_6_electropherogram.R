library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
source("R/parameter.R")
##50_stirred
# 设置目录路径
dir_path <- "D:/github/Degas_experiment/Data_SPM/stir_50spm"
# 获取带完整路径的所有 .csv 文件
file_list <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# 读取所有 CSV，跳过第一行，添加文件名（去除RUN）
stir50 <- map_df(file_list, function(f) {
  df <- read_csv(f, skip = 1)
  df$source_file <- as.numeric(str_extract(basename(f), "(?<=Run).*?(?=\\.csv)"))
  df$condition <- "50_stirred"
  df
})
p1<-ggplot(stir50,aes(x=`X(Minutes)`,y=`Y(Counts)`,color=as.factor(source_file)))+
  geom_line()+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  labs(title = "",x="",y="Intensity")+
  scale_x_continuous(limits = c(12.5,22.5),
                     breaks=c(12.5,15,17.5,20,22.5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = "black",family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = capillarycolor[1], color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "black"),  # 保留主网格线
        panel.grid.minor = element_line(color = "black"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        ggbreak.axis = element_blank(),  # 👈 隐藏断轴区域的重复横轴
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
p1
ggsave(plot=p1,"pic/SPM/BPC_50_stirred.pdf",width = 160,height=45,units = "mm",dpi = 600)



##50_unstirred
# 设置目录路径
dir_path <- "D:/github/Degas_experiment/Data_SPM/unstir_50spm"
# 获取带完整路径的所有 .csv 文件
file_list <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# 读取所有 CSV，跳过第一行，添加文件名（去除RUN）
unstir50 <- map_df(file_list, function(f) {
  df <- read_csv(f, skip = 1)
  df$source_file <- as.numeric(str_extract(basename(f), "(?<=Run).*?(?=\\.csv)"))
  df$condition <- "50_unstirred"
  df
})
p2<-ggplot(unstir50,aes(x=`X(Minutes)`,y=`Y(Counts)`,color=as.factor(source_file)))+
  geom_line()+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  labs(title = "",x="",y="Intensity")+
  scale_x_continuous(limits = c(12.5,22.5),
                     breaks=c(12.5,15,17.5,20,22.5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = "black",family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = capillarycolor[2], color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "black"),  # 保留主网格线
        panel.grid.minor = element_line(color = "black"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
p2
ggsave(plot=p2,"pic/SPM/BPC_50_unstirred.pdf",width = 160,height=45,units = "mm",dpi = 600)











##75_stirred
# 设置目录路径
dir_path <- "D:/github/Degas_experiment/Data_SPM/stir_75spm"
# 获取带完整路径的所有 .csv 文件
file_list <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# 读取所有 CSV，跳过第一行，添加文件名（去除RUN）
stir75 <- map_df(file_list, function(f) {
  df <- read_csv(f, skip = 1)
  df$source_file <- as.numeric(str_extract(basename(f), "(?<=75stir_).*?(?=\\.csv)"))
  df$condition <- "75_stirred"
  df
})
p3<-ggplot(stir75,aes(x=`X(Minutes)`,y=`Y(Counts)`,color=as.factor(source_file)))+
  geom_line()+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  labs(title = "",x="",y="Intensity")+
  scale_x_continuous(limits = c(12.5,22.5),
                     breaks=c(12.5,15,17.5,20,22.5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = "black",family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = capillarycolor[3], color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "black"),  # 保留主网格线
        panel.grid.minor = element_line(color = "black"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
p3
ggsave(plot=p3,"pic/SPM/BPC_75_stirred.pdf",width = 160,height=45,units = "mm",dpi = 600)










##75_unstirred
# 设置目录路径
dir_path <- "D:/github/Degas_experiment/Data_SPM/unstir_75spm"
# 获取带完整路径的所有 .csv 文件
file_list <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# 读取所有 CSV，跳过第一行，添加文件名（去除RUN）
unstir75 <- map_df(file_list, function(f) {
  df <- read_csv(f, skip = 1)
  df$source_file <- as.numeric(str_extract(basename(f), "(?<=75_unstir_).*?(?=\\.csv)"))
  df$condition <- "75_unstirred"
  df
})
p4<-ggplot(unstir75,aes(x=`X(Minutes)`,y=`Y(Counts)`,color=as.factor(source_file)))+
  geom_line()+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  labs(title = "",x="",y="Intensity")+
  scale_x_continuous(limits = c(12.5,22.5),
                     breaks=c(12.5,15,17.5,20,22.5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = "black",family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = capillarycolor[4], color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "black"),  # 保留主网格线
        panel.grid.minor = element_line(color = "black"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
p4
ggsave(plot=p4,"pic/SPM/BPC_75_unstirred.pdf",width = 160,height=45,units = "mm",dpi = 600)

