library(ggplot2)
library(dplyr)
df<-read.csv("Data_Ecoli/Ecoli_75_unstirred_Run7_BPC.csv",skip=1)
colnames(df)<-c("Point","Time","Intensity")
df<-df%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))
df<-na.omit(df)
BPC07<-ggplot(data=df,aes(x=Time,y=Intensity))+
  geom_line(linewidth=line_width,color=capillarycolor[4])+
  geom_vline(xintercept=25.953,color=P2_Ecoli,linetype="dashed",linewidth=line_width-0.4)+
  geom_vline(xintercept=25.441,color=P1_Ecoli,linetype="dashed",linewidth=line_width-0.4)+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
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
BPC07
ggsave(plot = BPC07,"pic/test/BPC07.pdf",width = 210,height=70,units = "mm",dpi = 600)


P1_07<-read.csv("Data_Ecoli/P1/4_75_unstirred/07.csv",skip=1)
colnames(P1_07)<-c("Point","Time","Intensity")
P1_ribbon <- P1_07 %>% filter(Time >= 25.291 & Time <= 25.291+0.682)
P2_07<-read.csv("Data_Ecoli/P2/4_75_unstirred/07.csv",skip=1)
colnames(P2_07)<-c("Point","Time","Intensity")
P2_ribbon <- P2_07 %>% filter(Time >= 25.823 & Time <= 25.823+0.594)
BPCzoomin<-ggplot(data=df,aes(x=Time,y=Intensity))+
  #曲线下阴影
  geom_ribbon(data=P1_ribbon,aes(ymin = 0,ymax = Intensity),fill=P1_Ecoli,alpha=0.4)+
  geom_ribbon(data=P2_ribbon,aes(ymin = 0,ymax = Intensity),fill=P2_Ecoli,alpha=0.4)+
  geom_line(linewidth=line_width,color=capillarycolor[4])+
  geom_linerange(aes(y=0,xmin=25.291,xmax = 25.291+0.682),color=P1_Ecoli,linewidth=line_width+0.5)+
  geom_linerange(aes(y=0,xmin=25.823,xmax=25.823+0.594),,color=P2_Ecoli,linewidth=line_width+0.5)+
  geom_vline(xintercept=25.953,color=P2_Ecoli,linetype="dashed",linewidth=line_width)+
  geom_vline(xintercept=25.441,color=P1_Ecoli,linetype="dashed",linewidth=line_width)+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  scale_x_continuous(limits = c(25,26.5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
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
BPCzoomin
ggsave(plot = BPCzoomin,"pic/test/BPCzoomin.pdf",width = 210,height=70,units = "mm",dpi = 600)
