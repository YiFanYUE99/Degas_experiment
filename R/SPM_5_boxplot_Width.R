library(readxl)
library(ggpubr)#用于标出显著性
library(ggplot2)
library(patchwork)
source("Rcode/parameter.R")
df<-read_excel("Data_SPM/capillary_standardprotein.xlsx")
df<-df%>%filter(capillary!="50_unstirred")

#Ubq
Ubqdf<-df%>%
  filter(Peak=="Ubiquitin")%>%
  mutate(Width=as.numeric(Width))
G<-c("50_stirred","75_stirred","75_unstirred")
UbqWidth<-ggplot(data = Ubqdf,aes(x=capillary,y=Width))+
  geom_boxplot(aes(fill = capillary),
               color=spmcolor[1],
               width=boxsize+0.4,
               outlier.shape = 8,#隐藏outlier
               outlier.size = point_size,
               linewidth=boxline_width,
               position = position_nudge(x = 0))+
  scale_fill_manual(values=capillarycolor)+
  #显著性检验
  stat_compare_means(comparisons = list(c(G[1],G[2]),c(G[1],G[3]),c(G[2],G[3])),

                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=spmcolor[1],
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.05) +     # 每组标注的高度递增值
  labs(title="Ubiquitin",x="",y="Peak Width")+
  xlim(G)+
  theme(plot.title = element_blank(),
        axis.title = element_text(size = axis_title,family = "sans",color = spmcolor[1], face = "bold"),
        axis.ticks = element_line(color = spmcolor[1],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[1],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=25,hjust = 1,color = spmcolor[1],family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[1],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = spmcolor[1],family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = spmcolor[1],family = "sans",face="bold"),
  )
UbqWidth
#ggsave(plot =UbqWidth,"pic/SPM/Migration_Ubq.tiff",width = 70,height=70,units = "mm",dpi = 600)














#Myo
Myodf<-df%>%
  filter(Peak=="Myoglobin")%>%
  mutate(Width=as.numeric(Width))
MyoWidth<-ggplot(data = Myodf,aes(x=capillary,y=Width))+
  geom_boxplot(aes(fill = capillary),
               color=spmcolor[2],
               width=boxsize+0.4,
               outlier.shape = 8,#隐藏outlier
               outlier.size = point_size,
               linewidth=boxline_width,
               position = position_nudge(x = 0))+
  scale_fill_manual(values=capillarycolor)+
  #显著性检验
  stat_compare_means(comparisons = list(c(G[1],G[2]),c(G[1],G[3]),c(G[2],G[3])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=spmcolor[2],
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.05) +     # 每组标注的高度递增值
  labs(title="Myoglobin",x="",y="Width")+
  xlim(G)+
  theme(plot.title = element_blank(),
        axis.title =  element_blank(),
        axis.ticks = element_line(color = spmcolor[2],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[2],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=25,hjust = 1,color = spmcolor[2],family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[2],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = spmcolor[2],family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = spmcolor[2],family = "sans",face="bold"),
  )
MyoWidth








#Ubq
CAdf<-df%>%
  filter(Peak=="Carbonic anhydrase")%>%
  mutate(Width=as.numeric(Width))
CAWidth<-ggplot(data = CAdf,aes(x=capillary,y=Width))+
  geom_boxplot(aes(fill = capillary),
               color=spmcolor[3],
               width=boxsize+0.4,
               outlier.shape = 8,#隐藏outlier
               outlier.size = point_size,
               linewidth=boxline_width,
               position = position_nudge(x = 0))+
  scale_fill_manual(values=capillarycolor)+
  #显著性检验
  stat_compare_means(comparisons = list(c(G[1],G[2]),c(G[1],G[3]),c(G[2],G[3])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=spmcolor[3],
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.05) +     # 每组标注的高度递增值
  labs(title="Carbonic anhydrase",x="",y="Width")+
  xlim(G)+
  theme(plot.title = element_blank(),
        axis.title =  element_blank(),
        axis.ticks = element_line(color = spmcolor[3],size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = spmcolor[3],family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=25,hjust = 1,color = spmcolor[3],family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = spmcolor[3],fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = spmcolor[3],family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = spmcolor[3],family = "sans",face="bold"),
  )
CAWidth

Widthall<-wrap_plots(UbqWidth, MyoWidth, CAWidth, nrow = 1)
ggsave(plot =Widthall,"pic/SPM/Width_all.pdf",width = 210,height=70,units = "mm",dpi = 600)

