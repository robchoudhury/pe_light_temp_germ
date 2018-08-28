library(tidyverse)
#the lightboxes that make the colors
lightbox=read_csv(file=('data/highlowlight.csv'))
lightbox1=subset(lightbox,Wavelength < 700 & Boxcolor==c("dark","blue","yellow","red","green","clear"))
col=c("blue", "ghostwhite", "black", "black","#32CD32", "ghostwhite","red", "yellow")
lightbox1=lightbox1 %>%
  mutate(Photon=Value*4.8)
lightbox1$Lightintensity<- factor(lightbox1$Lightintensity, labels = c("Full Light", "Low Light"))
p <- ggplot(lightbox1, aes(Wavelength, Photon))
p + geom_line(aes(color=Boxcolor), lwd=2)+ 
  scale_color_manual(values=c("blue","white","black","green","red","yellow"))+
  facet_grid(Lightintensity~., scales = "free_x")+
  #theme_dark()+
  theme( panel.background = element_rect(fill="grey50"),
         panel.grid.major = element_line(colour = "grey42"),
         panel.grid.minor = element_line(color = "grey42"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=25,face="bold"), 
         plot.title=element_text(size=30,face="bold"),
         strip.text.y = element_text(size=30, face="bold"),
         legend.position="none",
         legend.title=element_blank()) +
  xlab("Wavelength (nm)") +
  #ylab(expression(bold(umol/m^2/s))) 
  ylab(expression(bold(mu~mol/m^2/s))) 


###########
###########

germination3=read_csv(file=('data/germination3.csv'))
ggplot(germination3, aes(temp, per_germ, group=temp))+
  geom_jitter(aes(color=Isolate))+
  geom_boxplot(alpha=0.3, outlier.shape = NA)

###########
###########

time=read_csv(file='data/timecourse.csv')
ggplot(time, aes(Hour, per_germ, group=Hour))+
  geom_jitter()+
  geom_boxplot(alpha=0.3, outlier.shape = NA)

###########
###########

color=read_csv(file='data/uv1.csv')
col=c("ghostwhite", "black", "blue", "#32CD32", "red", "yellow")
ggplot(color, aes(color, per.germ, fill=factor(color))) +
  geom_jitter(alpha=0.7, shape=21)+
  geom_boxplot(alpha=0.3)+
  ylim(0,100)+
  scale_fill_manual(values = col, name = "Color") +
  scale_x_discrete(limits=c("all", "blue", "green", "yellow", "red","black" ),
                   labels=c( "No \nFilter", "Blue", "Green", "Yellow", "Red","Dark"))  + 
  facet_grid(Light~Temp) +
  theme(legend.position = "bottom") +
  theme_minimal()
