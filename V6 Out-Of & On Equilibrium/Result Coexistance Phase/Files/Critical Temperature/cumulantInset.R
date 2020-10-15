require(ggplot2)
require(grid)

#theme_white <- function() {
#	theme_update(panel.background = element_blank(),
#	panel.grid.major = element_blank())
#}
#theme_set(element_blank())
#theme_white()

file32 <- 'L=032/OutputFile.log'
file64 <- 'L=064/OutputFile.log'
file128 <- 'L=128/OutputFile.log'
file256 <- 'L=256/OutputFile.log'
file512 <- 'L=512/OutputFile.log'

data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep=',')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep=',')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')

temps <- unique(data128$Temperatura)

#Fixed Alfa
alphaVals <- unique(data32$Alfa)

dataL32 <- data32[data32$Alfa==alphaVals[1],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[1],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[1],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[1],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[1],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

dataL32 <- cbind(dataL32,size='dataL32')
dataL64 <- cbind(dataL64,size='dataL64')
dataL128 <- cbind(dataL128,size='dataL128')
dataL256 <- cbind(dataL256,size='dataL256')
dataL512 <- cbind(dataL512,size='dataL512')

#Merge all the dataframes
df <- rbind(dataL32,dataL64,dataL128,dataL256,dataL512)
p <- ggplot(df, aes(Temperatura, cumulantAnti,colour=size)) + labs(x =("Temperatura"),y = ("Cumulante U_4"))
mainplot <- p %+% subset(df, Temperatura > 0.22 & Temperatura<0.23) +
	geom_point() + geom_line() +	labs(title = "Cumulante U_4") + ylim(0.4,0.7) +#+ geom_smooth(method='loess',se = FALSE) 
	scale_color_manual(breaks = c(dataL32,dataL64,dataL128,dataL256,dataL512),
                        values=c("red", "blue", "green","cyan","brown")) #+
	#scale_shape_discrete(name  ="Tamaño",
    #                      breaks=c("Female", "Male"),
     #                     labels=c("Woman", "Man"))
p1 <- p + geom_rect(aes(xmin = 0.22, xmax = 0.23,
	ymin = 0.4, ymax = 0.7),
	fill = alpha("lightblue", 0.2)) + scale_x_continuous(breaks = NULL) +
	scale_y_continuous(breaks = NULL) + labs(y = NULL) +
	labs(title = "Full data") +
	theme(plot.title = element_text(face = "bold")) +
	theme(panel.border = element_blank())
subplot <- p1 + geom_line(colour = I("grey"),size = 0.8) + 
			geom_smooth(method='loess',se = FALSE, data=subset(df,(Temperatura > 0.22 & Temperatura < 0.23)))
vp <- grid::viewport(width = 0.4, height = 0.4, 
		x = unit(0.3,"npc"),y = unit(0.3, "npc"))
# 
#just = c("left","bottom")
pdf("cumulant.pdf")
#pushViewport(viewport())
print(mainplot)
#theme_set(element_blank(base_size = 8))
#theme_white()
#pushViewport(viewport(x=.6,y=.8,width=.25,height=.25,just=c("left","top")))
print(subplot, vp = vp)
#theme_set(element_blank())
#ggsave("cumulant.pdf", width = 20, height = 20, units = "cm")
#popViewport(2)
dev.off()