library(dplyr)
source("Rcode/parameter.R")
df<-read.csv("Data_SPM/unstir_75spm/75_unstir_004.csv",skip = 1)
colnames(df)<-c("points","Time","Intensity")
df<-df%>%
  mutate(Time=as.numeric(Time),
         Intensity=as.numeric(Intensity))

ubq_ribbon<-df%>%
  filter(Time>=14.451 &Time<=14.917)
myo_ribbon<-df%>%
  filter(Time>=15.549 &Time<=16.114)
CA_ribbon<-df%>%
  filter(Time>=16.879 &Time<=17.611)
spmplot<-ggplot(data=df,aes(x=Time,y=Intensity))+
  geom_ribbon(data=ubq_ribbon,aes(x=Time,ymin = 0,ymax=Intensity),fill=spmcolor[1],alpha=0.8)+
  geom_ribbon(data=myo_ribbon,aes(x=Time,ymin = 0,ymax=Intensity),fill=spmcolor[2],alpha=0.8)+
  geom_ribbon(data=CA_ribbon,aes(x=Time,ymin = 0,ymax=Intensity),fill=spmcolor[3],alpha=0.8)+
  geom_line(linewidth=line_width,color=capillarycolor[4])+
  geom_segment(aes(x=14.717,xend=14.717,y=0,yend=470679.11),color=spmcolor[1],linetype="dashed",linewidth=line_width-0.4)+
  geom_segment(aes(x=15.831,xend=15.831,y=0,yend=138023.46),color=spmcolor[2],linetype="dashed",linewidth=line_width-0.4)+
  geom_segment(aes(x=17.162,xend=17.162,y=0,yend=69246.11),color=spmcolor[3],linetype="dashed",linewidth=line_width-0.4)+
  labs(title = "",x="Time",y="Relative Intensity (%)")+
  scale_x_continuous(limits = c(0,50))+
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
        plot.margin = margin(0, 0, 0, 0) 
  )
spmplot
ggsave(plot = spmplot,"pic/SPM/BPC.pdf",width = 210,height=70,units = "mm",dpi = 600)
