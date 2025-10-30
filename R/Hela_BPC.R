library(dplyr)
library(patchwork)
library(ggplot2)
source("R/parameter.R")

df1 <- read.csv("Data_Hela/Hela_RUN1_3.16E8.csv", skip = 3)
df1<-df1%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))
Hela1<-ggplot(data=df1,aes(x=Time,y=Intensity))+
  geom_line(linewidth=line_width,color="#FFB6C1")+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  annotate("text", x = 60, y = 2E8, label = "RUN 1\nNL: 3.16E8", size = geom_text_size+2, color = "#FFB6C1")+
  scale_x_continuous(limits = c(0,75), breaks = c(0,15,30,45,60,75))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
Hela1




#5
df <- read.csv("Data_Hela/Hela_RUN5_3.36E8.csv", skip = 3)
df<-df%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))
Hela<-ggplot(data=df,aes(x=Time,y=Intensity))+
  geom_line(linewidth=line_width,color="#FF69B4")+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  scale_x_continuous(limits = c(0,75), breaks = c(0,15,30,45,60,75))+
  annotate("text", x = 60, y = 2E8, label = "RUN 5\nNL: 3.36E8", size = geom_text_size+2, color = "#FF69B4")+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
Hela


#9
df9 <- read.csv("Data_Hela/Hela_RUN9_3.19E8.csv", skip = 3)
df9<-df9%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))
Hela9<-ggplot(data=df9,aes(x=Time,y=Intensity))+
  geom_line(linewidth=line_width,color="#C71585")+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  annotate("text", x = 60, y = 2E8, label = "RUN 9\nNL: 3.19E8", size = geom_text_size+2, color = "#C71585")+
  scale_x_continuous(limits = c(0,75), breaks = c(0,15,30,45,60,75))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
Hela9
All<-Hela1/Hela/Hela9
All

ggsave(plot = All,"pic/Hela/BPC.pdf",width = 130,height=140,units = "mm",dpi = 600)
