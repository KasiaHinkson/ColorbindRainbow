#trying different visualizations
library(scales)
library(directlabels)
library(extrafont)

font_install('fontcm')
loadfonts()

vlevels<-levels(pewnewdat$variable)
vlevels<-rev(vlevels)
hvlevels<-levels(hisnewdat$variable)
hvlevels<-rev(hvlevels)

plots<-ggplot(pewnewdat, aes(x = variable, y = value,group=identity)) +
  geom_line(aes(linetype=factor(identity)),size=1) + 
  theme_bw()+  
  theme(text = element_text(family="CM Roman"))+
  labs(title="African Americans",x="Discrimination",y="Predicted Probabilities") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18)) + 
  theme(plot.title = element_text(size = 18)) + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_discrete(limits=vlevels,labels=c("None","A Little","Some","A Lot")) +
  geom_dl(aes(label = identity),method=list("last.qp", dl.trans(x=x+.2) ,cex = 0.8))

hisplots<-ggplot(hisnewdat, aes(x = variable, y = value,group=identity)) +
  geom_line(aes(linetype = factor(identity)),size=1) + 
  theme_bw()+ 
  theme(text = element_text(family="CM Roman")) +
  labs(title="Hispanic Americans",x="Discrimination",y="Predicted Probabilities") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18)) + 
  theme(plot.title = element_text(size = 18)) + 
  theme(legend.position="none")+ 
  scale_x_discrete(limits=hvlevels,labels=c("None","A Little","Some","A Lot")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  geom_dl(aes(label = identity),method=list("last.qp", dl.trans(x=x+.2) ,cex = 0.8))

prow <- plot_grid( plots + theme(legend.position="none"),
                   hisplots + theme(legend.position="none"),
                   align = 'hv',
                   hjust = -1,
                   nrow = 2
)

y.grob<-textGrob("Predicted Probabilities", gp=gpar(fontfamily= "CM Roman",fontsize=18), rot=90)
grid.arrange(arrangeGrob(prow,left=y.grob))
plot <- plot_grid( prow, label_y = "Predicted Probabilities", ncol=1, rel_heights = c(2, .2))
plot
