library(ggplot2)
source("Rcode/parameter.R")

##Ubq
Ubq<-read.csv("Data_SPM/Ubq.csv",skip=1)
colnames(Ubq)<-c("points","mz","intensity")

Ubqplot<-ggplot(data=Ubq,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[1],
                 linewidth=line_width)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(600,3000))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[1], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[1], face = "bold"),
        axis.ticks = element_line(color = spmcolor[1],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[1],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[1],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[1],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
Ubqplot
ggsave(plot = Ubqplot,"pic/SPM/Spectrum1_Ubq.pdf",width = 210,height=210/2,units = "mm",dpi = 600)
#zoomin
Ubqzoomin<-ggplot(data=Ubq,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[1],
                 linewidth=line_width)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(1223.5,1225.5))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[1], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[1], face = "bold"),
        axis.ticks = element_line(color = spmcolor[1],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[1],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[1],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[1],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
Ubqzoomin
ggsave(plot = Ubqzoomin,"pic/SPM/Spectrum1_Ubq_zoomin.pdf",width = 210,height=170,units = "mm",dpi = 600)
Ubqh<-Ubq%>%filter(mz>1223.5 & mz<1225.5)









#Myo
Myo<-read.csv("Data_SPM/Myo.csv",skip=1)
colnames(Myo)<-c("points","mz","intensity")

Myoplot<-ggplot(data=Myo,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[2],
                 linewidth=line_width)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(550,1500))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[2], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[2], face = "bold"),
        axis.ticks = element_line(color = spmcolor[2],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[2],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[2],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[2],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
Myoplot
ggsave(plot = Myoplot,"pic/SPM/Spectrum1_Myo.pdf",width = 210,height=210/2,units = "mm",dpi = 600)
#zoomin
Myozoomin<-ggplot(data=Myo,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[2],
                 linewidth=line_width)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(892,894))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[2], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[2], face = "bold"),
        axis.ticks = element_line(color = spmcolor[2],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[2],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[2],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[2],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
Myozoomin
ggsave(plot = Myozoomin,"pic/SPM/Spectrum1_Myo_zoomin.pdf",width = 210,height=170,units = "mm",dpi = 600)
Myoh<-Myo%>%filter(mz>892 & mz<894)










##CA
CA<-read.csv("Data_SPM/CA.csv",skip=1)
colnames(CA)<-c("points","mz","intensity")

CAplot<-ggplot(data=CA,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[3],
                 linewidth=line_width)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(600,1700))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[2], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[2], face = "bold"),
        axis.ticks = element_line(color = spmcolor[3],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[3],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[3],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[3],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
CAplot
ggsave(plot = CAplot,"pic/SPM/Spectrum1_CA.pdf",width = 210,height=180,units = "mm",dpi = 600)
#zoomin
CAzoomin<-ggplot(data=CA,aes(x=mz,y=intensity))+
  geom_linerange(aes(x=mz,ymin=0,ymax = intensity),
                 color=spmcolor[3],
                 linewidth=line_width-0.6)+
  labs(title="",x="",y="")+
  scale_x_continuous(limits=c(879.5,881))+
  theme(plot.title =  element_text(size = plot_title,family = "sans",color = spmcolor[3], face = "bold",hjust = 0.5),
        axis.title =  element_text(size = axis_title,family = "sans",color = spmcolor[3], face = "bold"),
        axis.ticks = element_line(color = spmcolor[3],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[3],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,color = spmcolor[3],family = "sans",face="bold"),
        #axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[3],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",
  )
CAzoomin
ggsave(plot = CAzoomin,"pic/SPM/Spectrum1_CA_zoomin.pdf",width = 170,height=170,units = "mm",dpi = 600)
CAh<-CA%>%filter(mz>879 & mz<881)



