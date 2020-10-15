library(ggplot2)
library(cowplot)

file <- 'data.dat'
data <- read.csv(file,sep=",",stringsAsFactors=F,header=F)

#Filter snap
valores <- unique(data$V1)
dataT1 <- subset(data,V1 == valores[1], select=c(V1,V2,V3,V4))

q1 <- ggplot (data=dataT1,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
#ggsave("t1.pdf")

dataT2 <- subset(data,V1 == valores[2], select=c(V1,V2,V3,V4))

q2 <- ggplot (data=dataT2,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
#ggsave("t2.pdf")

dataT4 <- subset(data,V1 == valores[3], select=c(V2,V3,V4))

q4 <- ggplot (data=dataT4,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())

dataT6 <- subset(data,V1 == valores[4], select=c(V2,V3,V4))
q6 <- ggplot (data=dataT6,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
dataT10 <- subset(data,V1 == valores[5], select=c(V2,V3,V4))
q10 <- ggplot (data=dataT10,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
dataT20 <- subset(data,V1 == valores[6], select=c(V2,V3,V4))
q20 <- ggplot (data=dataT20,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
dataT40 <- subset(data,V1 == valores[7], select=c(V2,V3,V4))
q40 <- ggplot (data=dataT40,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
dataT100 <- subset(data,V1 == valores[8], select=c(V2,V3,V4))

q100 <- ggplot (data=dataT100,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
#ggsave("t100.pdf")

dataT100mil <- subset(data,V1 == valores[9], select=c(V2,V3,V4))

q100mil <- ggplot (data=dataT100mil,aes(x=V2,y=V3,colour=V4 ) ) + geom_point(shape=15,size=0.5) + theme(axis.line=element_blank(),
																					      axis.text.x=element_blank(),
																					      axis.text.y=element_blank(),
																					      axis.ticks=element_blank(),
																					      axis.title.x=element_blank(),
																					      axis.title.y=element_blank(),
																					      legend.position="none",
																					      panel.background=element_blank(),
																					      panel.border=element_blank(),
																					      panel.grid.major=element_blank(),
																					      panel.grid.minor=element_blank(),
																					      plot.background=element_blank())
#ggsave("t100mil.pdf")

g <- plot_grid(q1,q2,q4,q6,q10,q20,q40,q100,q100mil, labels=c("t=1", "t=2","t=4","t=6","t=10","t=20","t=40","t=100","t=100mil"), ncol = 3, nrow = 3,label_size=10)
ggsave('grid.pdf')