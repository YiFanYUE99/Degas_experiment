library(ggplot2)
#0) spectrum
getwd()
source("Rcode/parameter.R")
spectrum1<-read.csv("Data_Ecoli/P1/P1_spectrum.csv",skip = 1)
colnames(spectrum1)<-c("point","mz","intensity")
s1<-ggplot(data=spectrum1,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=P1_Ecoli,
                 linewidth=line_width)+
  labs(title="Peak 1",x="m/z",y="Intensity")+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = P1_Ecoli, face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P1_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P1_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P1_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
s1
ggsave(plot = s1,"pic/test/Spectrum1.pdf",width = 105,height=70,units = "mm",dpi = 600)
#zoom in s1
ss1<-ggplot(data=spectrum1,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=P1_Ecoli,
                 linewidth=line_width)+
  geom_text(
    data = subset(spectrum1, mz >= 1082.4 & mz <= 1084),
    aes(label = mz),
    vjust = -0.5,
    size=geom_text_size,
    color=P1_Ecoli
  ) +
  scale_x_continuous(limits=c(1082.4,1084))+
  scale_y_continuous(limits=c(0,3.5e5),
                     breaks = c(0,1e5,2e5,3e5))+
  labs(title="",x="",y="")+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = P1_Ecoli, face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        #panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        #panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P1_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P1_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P1_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
ss1
ggsave(plot = ss1,"pic/test/Spectrum1_zoomin.pdf",bg="transparent",
       width = 105,height=70,units = "mm",dpi = 600)


#spectrum2
spectrum2<-read.csv("Data_Ecoli/P2/P2_spectrum.csv",skip = 1)
colnames(spectrum2)<-c("point","mz","intensity")
s2<-ggplot(data=spectrum2,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=P2_Ecoli,
                 linewidth=line_width)+
  labs(title="Peak 2",x="m/z",y="Intensity")+
  scale_y_continuous(position = "right")+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = P2_Ecoli, face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.right = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P2_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P2_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P2_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
  )
s2
ggsave(plot = s2,"pic/test/Spectrum2.pdf",width = 105,height=70,units = "mm",dpi = 600)

#zoom in s2
ss2<-ggplot(data=spectrum2,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=P2_Ecoli,
                 linewidth=line_width)+
  geom_text(
    data = subset(spectrum2, mz >= 1331 & mz <= 1332),
    aes(label = mz),
    vjust = -0.5,
    size=geom_text_size,
    color=P2_Ecoli
  ) +
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(1331,1332))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = P2_Ecoli, face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P2_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P2_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P2_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
ss2
ggsave(plot = ss2,"pic/test/Spectrum2_zoomin.pdf",bg="transparent",
       width = 105,height=70,units = "mm",dpi = 600)




