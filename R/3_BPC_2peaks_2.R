library(ggplot2)
library(dplyr)
library(patchwork)
source("Rcode/parameter.R")

###第二组
#PEAK1
# 获取所有 csv 文件路径
folder_path <- "D:/Degas_Experiment/Data_Ecoli/P1/2_50_unstirred"
csv_files_21 <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 逐个读取并添加文件名列
df_list_21 <- lapply(csv_files_21, function(file) {
  df <- read.csv(file, skip=1,stringsAsFactors = FALSE)
  
  # 删除 .csv 后缀：只保留文件名主体
  df$Run <- tools::file_path_sans_ext(basename(file))
  
  return(df)
})

# 合并为一个大数据框
df_combined_21 <- do.call(rbind, df_list_21)
colnames(df_combined_21)<-c("points","Time (min)","Relative Intensity","Run")
df_combined_21$Capillary<-rep("50_unstirred",dim(df_combined_21)[1])

#PEAK2
# 获取所有 csv 文件路径
folder_path <- "D:/Degas_Experiment/Data_Ecoli/P2/2_50_unstirred"
csv_files_22 <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# 逐个读取并添加文件名列
df_list_22 <- lapply(csv_files_22, function(file) {
  df <- read.csv(file, skip=1,stringsAsFactors = FALSE)
  
  # 删除 .csv 后缀：只保留文件名主体
  df$Run <- tools::file_path_sans_ext(basename(file))
  
  return(df)
})
# 合并为一个大数据框
df_combined_22 <- do.call(rbind, df_list_22)
colnames(df_combined_22)<-c("points","Time (min)","Relative Intensity","Run")
df_combined_22$Capillary<-rep("50_unstirred",dim(df_combined_22)[1])


plot_list <- list() 
for (i in 1:20) {
  df1<-df_combined_21%>%
    mutate(Run=as.numeric(Run),
           `Time (min)`=as.numeric(`Time (min)`),
           `Relative Intensity`=as.numeric(`Relative Intensity`))%>%
    filter(Run==i)
  df2<-df_combined_22%>%
    mutate(Run=as.numeric(Run),
           `Time (min)`=as.numeric(`Time (min)`),
           `Relative Intensity`=as.numeric(`Relative Intensity`))%>%
    filter(Run==i)
  p<-ggplot(data=df1,aes(x=`Time (min)`,y=`Relative Intensity`))+
    geom_line(aes(group=Run),color=Run20[i],linewidth=line_width+2)+
    geom_line(data=df2,aes(x=`Time (min)`,y=`Relative Intensity`,group = Run),
              color= Run20[i],linewidth=line_width+2)+
    geom_ribbon(data = df1,aes(x=`Time (min)`,ymin=0,ymax=`Relative Intensity`,group=Run),fill=P1_Ecoli,alpha=0.7)+
    geom_ribbon(data = df2,aes(x=`Time (min)`,ymin=0,ymax=`Relative Intensity`,group=Run),fill=P2_Ecoli,alpha=0.7)+
    #annotate("text", x = 22.3, y = 105, label = paste0("Run", i), size = geom_text_size+2,color=capillarycolor[2])+
    scale_x_continuous(limits=c(24,31),
                       breaks=c(24,25,26,27,28,29,30,31),
                       expand=c(0,0))+
    scale_y_continuous(limits = c(0,110),
                       breaks = c(0,25,50,75,100),
                       expand = c(0,0))+
    labs(title="",x="",y="")+
    theme(axis.title =  element_text(size = axis_title,family = "sans",color = capillarycolor[2], face = "bold"),
          axis.ticks = element_line(color = capillarycolor[2],size=axis_ticks),
          axis.text.y = element_blank(),
          axis.text.x= element_blank(),
          panel.background = element_rect(fill = "white", color = NA), 
          plot.background = element_rect(fill = "white", color = NA),   # 透明背景
          panel.grid.major = element_line(color = "grey90"),   # 保留次网格线
          axis.line = element_blank(),  # 保留横纵轴的黑色竖线
          #标签位置
          panel.border = element_rect(color = capillarycolor[2],fill=NA,linewidth = 4),  
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0)
    )
  p
  plot_list <- append(plot_list, list(p))
}
pall2<-wrap_plots(plot_list, ncol = 1)
pall2
ggsave(plot = pall2,"pic/test1/BPC2peaks_2.tiff",width = 200/4*8,height=200*8.5,units = "mm",dpi = 600,limitsize = F)

