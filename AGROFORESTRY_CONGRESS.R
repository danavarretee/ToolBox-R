library(readxl)
library(ggplot2)
library(ggthemes)
library(MASS)
library(jpeg)
library(grid)


setwd("C:/Users/diego.navarrete/Pictures/PruebaR/ToolBox-R/CODES")
image_file <- 
img <- readJPEG(file.path('..','DATA','CACAO_PLATANO_4_v2.jpg'))
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)


DATA_CO2_EMISSIONS <- read_excel(file.path('..','DATA','DATOS_AGROFORESTRY_CONGRESS.xlsx'))
str(DATA_CO2_EMISSIONS)


p <- ggplot(data=DATA_CO2_EMISSIONS, aes(x = Tiempo)) + 
  annotation_custom(g, -Inf, Inf, -Inf, Inf)  
p <- p + geom_line(aes(y = Carbono, colour = "Carbon"), size = 1.5) +
  geom_ribbon(aes(ymin=SE_C_MIN, ymax=SE_C_MAX), alpha=0.6, fill="salmon1") +
  theme(axis.text.x = element_text(colour="black",size=16)) +
  theme(axis.text.y = element_text(colour="black",size=16)) +
  theme(axis.title.y = element_text(colour="black",size=18)) +
  theme(axis.title.x = element_text(colour="black",size=18)) +
  ylab(bquote('SOC (Mg C ' ~ha^-1~')')) +
  xlab(bquote('Time (yr)')) +
  geom_vline(aes(xintercept=30), size=1, colour="#BB0000", linetype="dashed") +
  theme(legend.position = c(0.36, 0.85)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(colour="black",size=16)) +
  scale_colour_discrete(name  ="Colour",
                        breaks=c("Carbon", "Densidad"),
                        labels=c("SOC", "Bulk density"))
  p <- p + geom_line(aes(y = Densidad*20, colour = "Densidad"), size = 1.5) + 
  geom_ribbon(aes(ymin=SE_D_MIN*20, ymax=SE_D_MAX*20), alpha=0.6, fill="aquamarine3")
p <- p + scale_y_continuous(sec.axis = sec_axis(~./20, name = 'Bulk density (g ' ~cm^-3~')'))

p1 <- p + annotate("text", x = 13.8, y = 2.5, label = "Forest-to-pasture conversion and pasture degradation", size=6)
p1

p2 <- p1 + annotate("text", x = 37, y = 2.5, label = "Agroforestry establishment", size=6)
p2

p3 <- p2 + annotate("segment", x = 0, xend = 29.7, y = 0, yend = 0, colour = "purple", size=3, alpha=0.6, arrow=arrow())
p3

p4 <- p3 + annotate("segment", x = 30.3, xend = 42, y = 0, yend = 0, colour = "blue", size=3, alpha=0.6, arrow=arrow())
p4


jpeg(file = file.path('..','RESULTS','AGROFORESTRY.jpeg'), width = 1600, height = 1000, res = 150)

p4

dev.off()



#https://stackoverflow.com/questions/42333085/how-to-fit-the-plot-over-a-background-image-in-r-and-ggplot2
