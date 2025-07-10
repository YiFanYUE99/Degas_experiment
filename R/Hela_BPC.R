library(readxl)
source("R/parameter.R")
df <- read_excel("Data_Hela/Hela_Run5.xlsx", skip = 3)
df<-df%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))
Hela<-ggplot(data=df,aes(x=Time,y=Intensity))+
  geom_line(linewidth=line_width,color="grey30")+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  annotate("text", x = 75, y = 3E8, label = "RUN 5\nNL: 6.12E9", size = geom_text_size+2, color = "grey30")+
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
ggsave(plot = Hela,"pic/Hela/BPC.pdf",width = 89,height=70,units = "mm",dpi = 600)
