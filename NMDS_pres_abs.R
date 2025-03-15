#install.packages("ggrepel")
library(ggrepel)
library(vegan)
library(ggplot2)
library(ggrepel)
library(grid)


data.NMDS01<-read.csv("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/NMDS_Daten_pres_abs_FINAL.csv",sep=";", header = T)

com01=data.NMDS01[,3:ncol(data.NMDS01)] #w?hlt Daten f?r die "community-matrix" aus der Tabelle aus, also alle Daten, die f?r den NMDSplot verwendet werden sollen

com01

NMDS01.matrix=as.matrix(com01) #f?r NMDS m?ssen Daten von "data frame"-Format in matrix Format umgewandelt werden 


rankindex(NMDS01.matrix, com01)
#result:
#euc       man       gow       bra       kul 
#0.8292673 0.8292673 0.8292673 0.3587112 0.3610829  
#best value: gow, euc, man

nmds01.gow = metaMDS(NMDS01.matrix, distance = "gower")
nmds01.gow 
#->Ergebnis stress:  0.1709266  
nmds01.man = metaMDS(NMDS01.matrix, distance = "manhattan")
nmds01.man 
#->Ergebnis stress:  0.1707929  
nmds01.euc = metaMDS(NMDS01.matrix, distance = "euclidean")
nmds01.euc
#->Ergebnis stress:  0.1707859   

###NMDS plot with method gow
x11()
plot(nmds01.gow)

NMDS01.scores.gow = as.data.frame(scores(nmds01.gow)$sites) 

NMDS01.scores.gow$SITE = data.NMDS01$SITE
NMDS01.scores.gow$MONTH = data.NMDS01$MONTH


head(NMDS01.scores.gow)

x11()
NMDS01.plot.gow <- ggplot(data = NMDS01.scores.gow, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = NMDS01.scores.gow, aes(colour = MONTH), size = 6, alpha = 0.6)+
  geom_text(
    label=NMDS01.scores.gow$SITE,
    nudge_x=0, nudge_y=-0.005,
    check_overlap=T,
    parse = T,
    size = 6)+
  geom_text(x=0.2, y=0.16, label="stress = 0.17", size = 6) 
NMDS01.plot.gow + theme_bw(base_size = 18)  


