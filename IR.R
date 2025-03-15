######################### models IR masterscript ########################
#########################################################################
library(lme4)
library(r2glmm)
library(glmmTMB)
library(ggeffects)
library(sjPlot)
library(tidyverse)
library(patchwork)
library(stats)
library(MuMIn)
library(GGally)
library(MuMIn)
library(prediction)

data_modIR <- read.table("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/Variablen_Weiden_alle_Jahre_DataIR.csv",
                       sep = ";",
                       dec = ",",
                       header = T,
                       text = "",
                       stringsAsFactors = T)

data_modIR <- data_modIR %>%
  mutate(AGRI.s22 = scale(AGRI_2022),
         SETTL.s22 = scale(SETTL_2022),
         WOOD.s22 = scale(WOOD_2022),
         PAST.s22 = scale(PAST_2022),
         AGRI.s90 = scale(AGRI_1990),
         SETTL.s90 = scale(SETTL_1990),
         WOOD.s90 = scale(WOOD_1990),
         PAST.s90 = scale(PAST_1990),
         AGRI.s50 = scale(AGRI_1950),
         SETTL.s50 = scale(SETTL_1950),
         WOOD.s50 = scale(WOOD_1950),
         PAST.s50 = scale(PAST_1950),
         AGRI.s00 = scale(AGRI_1900),
         SETTL.s00 = scale(SETTL_1900),
         WOOD.s00 = scale(WOOD_1900),
         PAST.s00 = scale(PAST_1900))


#Bernd: um dein Modell zum convergieren zu bringen gibt es sogenannte Optimizer die du in die Formel stecken kannst. Im endeffekt machen die folgende. Das Model versucht eine Lösung für die Parameter zu bekommen in dem es versucht sich durch mehrere Iterationen der Lösung anzunähern. Wenn ein Modell nicht konvergiert kann das sein dass es einfach innerhalb der default Iterationen keine Lösung gefunden hat und die Optimizer erhöhen vereinfacht gesagt die Anzahl dieser Iterationen und optimieren die Lösungssuche. Heißt du fügst einfach folgenden Term in deine Funktion ein: ",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))"
#So convergiert das Modell, gibt dir aber als Warnung "boundary (singular) fit: see help('isSingular')" aus. DAs gibt die an, dass in deiner Randomeffektstruktur effekte drin sind die nicht wirklich einen einfluss haben. Wenn du in die Summary schaust, siehst du dass der Effekt von SA_MONTH nicht wirklich relevant ist (Variance 4.051e-16). Entsprechend würde ich den Term rausnehmen.

#######alle Arten zusammen#######

modIR.2022 <- glmer(data=data_modIR, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022 <- summary(modIR.2022)
sum.modIR.2022
AICc(modIR.2022) #3323.325
r.squaredGLMM(modIR.2022) #0.9715023

modIR.1990 <- glmer(data=data_modIR, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990 <- summary(modIR.1990)
sum.modIR.1990
AICc(modIR.1990) #3322.882
r.squaredGLMM(modIR.1990) #0.9715244

modIR.1950 <- glmer(data=data_modIR, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950 <- summary(modIR.1950)
sum.modIR.1950
AICc(modIR.1950) #3323.554
r.squaredGLMM(modIR.1950) #0.9715176

modIR.1900 <- glmer(data=data_modIR, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900 <- summary(modIR.1900)
sum.modIR.1900
AICc(modIR.1900) #3315.753
r.squaredGLMM(modIR.1900) #0.9717674

#Akaike weights
weights.modIR<-model.sel(modIR.2022,modIR.1990,modIR.1950,modIR.1900)
weights.modIR
#                                  df logLik   AICc    delta weight
#modIR.1900                        7 -1650.754 3315.8  0.00  0.934
#modIR.1990                        7 -1654.318 3322.9  7.13  0.026
#modIR.2022                        7 -1654.540 3323.3  7.57  0.021
#modIR.1950                        7 -1654.655 3323.6  7.80  0.019

#######endokopride#######
#alle Modelle singularfit -> wahrscheinlich zu wenig Daten
data_modIR.end <- read.table("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/Variablen_Weiden_alle_Jahre_DataIR_end.csv",
                         sep = ";",
                         dec = ",",
                         header = T,
                         text = "",
                         stringsAsFactors = T)

data_modIR.end <- data_modIR.end %>%
  mutate(AGRI.s22 = scale(AGRI_2022),
         SETTL.s22 = scale(SETTL_2022),
         WOOD.s22 = scale(WOOD_2022),
         PAST.s22 = scale(PAST_2022),
         AGRI.s90 = scale(AGRI_1990),
         SETTL.s90 = scale(SETTL_1990),
         WOOD.s90 = scale(WOOD_1990),
         PAST.s90 = scale(PAST_1990),
         AGRI.s50 = scale(AGRI_1950),
         SETTL.s50 = scale(SETTL_1950),
         WOOD.s50 = scale(WOOD_1950),
         PAST.s50 = scale(PAST_1950),
         AGRI.s00 = scale(AGRI_1900),
         SETTL.s00 = scale(SETTL_1900),
         WOOD.s00 = scale(WOOD_1900),
         PAST.s00 = scale(PAST_1900))

modIR.2022.end <- glmer(data=data_modIR.end, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.end <- summary(modIR.2022.end)
sum.modIR.2022.end
AICc(modIR.2022.end) #1533.463
r.squaredGLMM(modIR.2022.end) #0.9544725

modIR.1990.end <- glmer(data=data_modIR.end, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.end <- summary(modIR.1990.end)
sum.modIR.1990.end
AICc(modIR.1990.end) #1531.135
r.squaredGLMM(modIR.1990.end) #0.9545049

modIR.1950.end <- glmer(data=data_modIR.end, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.end <- summary(modIR.1950.end)
sum.modIR.1950.end
AICc(modIR.1950.end) #3323.554
r.squaredGLMM(modIR.1950.end) #0.9715176

modIR.1900.end <- glmer(data=data_modIR.end, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.end <- summary(modIR.1900.end)
sum.modIR.1900.end
AICc(modIR.1900.end) #1532.971
r.squaredGLMM(modIR.1900.end) #0.9545170

#Akaike weights
weights.modIR.end<-model.sel(modIR.2022.end,modIR.1990.end,modIR.1950.end,modIR.1900.end)
weights.modIR.end
#                                      df logLik   AICc    delta weight
#modIR.1900.end                        7 -757.015  1528.5  0.00  0.683
#modIR.1990.end                        7 -758.320  1531.1  2.61  0.185
#modIR.1950.end                        7 -759.238  1533.0  4.45  0.074
#modIR.2022.end                        7 -759.484  1533.5  4.94  0.058


#######parakopride#######
data_modIR.para <- read.table("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/Variablen_Weiden_alle_Jahre_DataIR_para.csv",
                             sep = ";",
                             dec = ",",
                             header = T,
                             text = "",
                             stringsAsFactors = T)

data_modIR.para <- data_modIR.para %>%
  mutate(AGRI.s22 = scale(AGRI_2022),
         SETTL.s22 = scale(SETTL_2022),
         WOOD.s22 = scale(WOOD_2022),
         PAST.s22 = scale(PAST_2022),
         AGRI.s90 = scale(AGRI_1990),
         SETTL.s90 = scale(SETTL_1990),
         WOOD.s90 = scale(WOOD_1990),
         PAST.s90 = scale(PAST_1990),
         AGRI.s50 = scale(AGRI_1950),
         SETTL.s50 = scale(SETTL_1950),
         WOOD.s50 = scale(WOOD_1950),
         PAST.s50 = scale(PAST_1950),
         AGRI.s00 = scale(AGRI_1900),
         SETTL.s00 = scale(SETTL_1900),
         WOOD.s00 = scale(WOOD_1900),
         PAST.s00 = scale(PAST_1900))

modIR.2022.para <- glmer(data=data_modIR.para, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.para <- summary(modIR.2022.para)
sum.modIR.2022.para
AICc(modIR.2022.para) #1742.402
r.squaredGLMM(modIR.2022.para) #0.9802214

modIR.1990.para <- glmer(data=data_modIR.para, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.para <- summary(modIR.1990.para)
sum.modIR.1990.para
AICc(modIR.1990.para) #1743.336
r.squaredGLMM(modIR.1990.para) #0.9801881

modIR.1950.para <- glmer(data=data_modIR.para, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.para <- summary(modIR.1950.para)
sum.modIR.1950.para
AICc(modIR.1950.para) #1742.969
r.squaredGLMM(modIR.1950.para) #0.9801870

modIR.1900.para <- glmer(data=data_modIR.para, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.para <- summary(modIR.1900.para)
sum.modIR.1900.para
AICc(modIR.1900.para) #1739.823
r.squaredGLMM(modIR.1900.para) #0.9801966

#Akaike weights
weights.modIR.para<-model.sel(modIR.2022.para,modIR.1990.para,modIR.1950.para,modIR.1900.para)
weights.modIR.para
#                                       df logLik     AICc    delta    weight
#modIR.1900.para                        7 -862.652   1739.8    0.00    0.604
#modIR.2022.para                        7 -863.942   1742.4    2.58    0.166
#modIR.1950.para                        7 -864.225   1743.0    3.15    0.125
#modIR.1990.para                        7 -864.409   1743.3    3.51    0.104

#######increasing#######
#alle Modelle singularfit -> wahrscheinlich zu wenig Daten
inc.spec <- subset(data_modIR, SPECIES %in% c("Acanthobodilus immundus" , "Bodilopsis rufus", "Esymus pusillus", "Labarrus lividus", "Limarus maculatus", "Melinopterus consputus", "Otophorus haemorrhoidalis", "Oxyomus sylvestris", "Rhodaphodius foetens", "Sigorus porcus", "Subrinus sturmi", "Trypocopris vernalis"))

modIR.2022.inc <- glmer(data=inc.spec, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.inc <- summary(modIR.2022.inc)
sum.modIR.2022.inc
AICc(modIR.2022.inc) #685.3468
r.squaredGLMM(modIR.2022.inc) #0.9520272

modIR.1990.inc <- glmer(data=inc.spec, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.inc <- summary(modIR.1990.inc)
sum.modIR.1990.inc
AICc(modIR.1990.inc) #685.1731
r.squaredGLMM(modIR.1990.inc) #0.9520657

modIR.1950.inc <- glmer(data=inc.spec, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.inc <- summary(modIR.1950.inc)
sum.modIR.1950.inc
AICc(modIR.1950.inc) #687.1433
r.squaredGLMM(modIR.1950.inc) #0.9518711

modIR.1900.inc <- glmer(data=inc.spec, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.inc <- summary(modIR.1900.inc)
sum.modIR.1900.inc
AICc(modIR.1900.inc) #681.2124
r.squaredGLMM(modIR.1900.inc) #0.9521051

#Akaike weights
weights.modIR.inc<-model.sel(modIR.2022.inc,modIR.1990.inc,modIR.1950.inc,modIR.1900.inc)
weights.modIR.inc
#                                       df logLik     AICc      delta     weight
#modIR.1900.inc                        7 -333.017    681.2      0.00      0.760
#modIR.1990.inc                        7 -334.997    685.2      3.96      0.105
#modIR.2022.inc                        7 -335.084    685.3      4.13      0.096
#modIR.1950.inc                        7 -335.982    687.1      5.93      0.039

#######decreasing#######
#alle Modelle singularfit -> wahrscheinlich zu wenig Daten
dec.spec <- subset(data_modIR, SPECIES %in% c("Acrossus depressus", "Acrossus luridus", "Acrossus rufipes", "Agrilinus ater", "Aphodius fimetarius", "Biralus satellitius", "Bodilus lugens", "Calamosternus granarius", "Chilothorax distinctus", "Colobopterus erraticus", "Coprimorphus scrutator", "Copris lunaris", "Eupleurus subterraneus", "Geotrupes spiniger", "Geotrupes stercorarius", "Melinopterus prodromus", "Melinopterus sphacelatus"," Nialus varians", "Onthophagus coenobita", "Onthophagus fracticornis", "Onthophagus furcatus", "Onthophagus lemur", "Onthophagus medius", "Onthophagus nuchicornis", "Onthophagus ruficapillus", "Onthophagus vacca", "Onthophagus verticicornis", "Onthophagus vitulus", "Phalacronothus biguttatus", "Plagiogonus arenarius","Pleurophorus caesus", "Pleurophorus pannonicus", "Sisyphus schaefferi", "Teuchestes fossor", "Trichonotulus scrofa", "Volinus sticticus"))

modIR.2022.dec <- glmer(data=dec.spec, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.dec <- summary(modIR.2022.dec)
sum.modIR.2022.dec
AICc(modIR.2022.dec) #1657.137
r.squaredGLMM(modIR.2022.dec) #0.9662374

modIR.1990.dec <- glmer(data=dec.spec, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.dec <- summary(modIR.1990.dec)
sum.modIR.1990.dec
AICc(modIR.1990.dec) #1655.248
r.squaredGLMM(modIR.1990.dec) #0.9662719

modIR.1950.dec <- glmer(data=dec.spec, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.dec <- summary(modIR.1950.dec)
sum.modIR.1950.dec
AICc(modIR.1950.dec) #1657.166
r.squaredGLMM(modIR.1950.dec) #0.9662140

modIR.1900.dec <- glmer(data=dec.spec, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.dec <- summary(modIR.1900.dec)
sum.modIR.1900.dec
AICc(modIR.1900.dec) #1654.329
r.squaredGLMM(modIR.1900.dec) #0.9663224

#Akaike weights
weights.modIR.dec<-model.sel(modIR.2022.dec,modIR.1990.dec,modIR.1950.dec,modIR.1900.dec)
weights.modIR.dec
#                                       df logLik     AICc      delta       weight
#modIR.1900.dec                        7 -819.926     1654.3    0.00        0.472
#modIR.1990.dec                        7 -820.386     1655.2    0.92        0.298
#modIR.2022.dec                        7 -821.330     1657.1    2.81        0.116
#modIR.1950.dec                        7 -821.345     1657.2    2.84        0.116


#######wood species#######
wood.spec <- subset(data_modIR, SPECIES %in% c("Acrossus depressus", "Acrossus luridus", "Acrossus rufipes", "Agrilinus ater"," Aphodius fimetarius", "Biralus satellitius", "Bodilopsis rufus", "Calamosternus granarius", "Chilothorax distinctus", "Colobopterus erraticus", "Coprimorphus scrutator", "Copris lunaris", "Esymus pusillus", "Euorodalus coenosus", "Eupleurus subterraneus", "Geotrupes spiniger", "Geotrupes stercorarius", "Labarrus lividus", "Limarus maculatus", "Melinopterus prodromus", "Melinopterus sphacelatus", "Onthophagus coenobita", "Onthophagus fracticornis", "Onthophagus illyricus", "Onthophagus joannae", "Onthophagus nuchicornis", "Onthophagus ovatus", "Onthophagus taurus", "Onthophagus verticicornis", "Otophorus haemorrhoidalis", "Oxyomus sylvestris", "Phalacronothus biguttatus", "Planolinoides borealis", "Rhodaphodius foetens", "Sigorus porcus", "Sisyphus schaefferi", "Teuchestes fossor", "Trichonotulus scrofa", "Volinus sticticus", "Trypocopris vernalis"))

modIR.2022.wood <- glmer(data=wood.spec, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.wood <- summary(modIR.2022.wood)
sum.modIR.2022.wood
AICc(modIR.2022.wood) #2182.838
r.squaredGLMM(modIR.2022.wood) #0.9638023

modIR.1990.wood <- glmer(data=wood.spec, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.wood <- summary(modIR.1990.wood)
sum.modIR.1990.wood
AICc(modIR.1990.wood) #2182.638
r.squaredGLMM(modIR.1990.wood) #0.9638077

modIR.1950.wood <- glmer(data=wood.spec, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.wood <- summary(modIR.1950.wood)
sum.modIR.1950.wood
AICc(modIR.1950.wood) #2184.077
r.squaredGLMM(modIR.1950.wood) #0.9638171

modIR.1900.wood <- glmer(data=wood.spec, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.wood <- summary(modIR.1900.wood)
sum.modIR.1900.wood
AICc(modIR.1900.wood) #2175.043
r.squaredGLMM(modIR.1900.wood) #0.9640002

#Akaike weights
weights.modIR.wood<-model.sel(modIR.2022.wood,modIR.1990.wood,modIR.1950.wood,modIR.1900.wood)
weights.modIR.wood
#                                       df logLik        AICc        delta         weight
#modIR.1900.wood                        7 -1080.337      2175.0      0.00          0.949
#modIR.1990.wood                        7 -1084.135      2182.6      7.60          0.021
#modIR.2022.wood                        7 -1084.235      2182.8      7.80          0.019
#modIR.1950.wood                        7 -1084.855      2184.1      9.03          0.010


#######openland species#######
#alle Modelle singularfit -> wahrscheinlich zu wenig Daten
open.spec <- subset(data_modIR, SPECIES %in% c("Acanthobodilus immundus", "Bodilus lugens", "Euoniticellus fulvus", "Euorodalus paracoenosus", "Melinopterus consputus", "Nialus varians", "Onthophagus furcatus", "Onthophagus lemur", "Onthophagus medius", "Onthophagus ruficapillus", "Onthophagus vacca", "Onthophagus vitulus", "Plagiogonus arenarius", "Pleurophorus caesus", "Pleurophorus pannonicus", "Subrinus sturmi"
))

modIR.2022.open <- glmer(data=open.spec, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.2022.open <- summary(modIR.2022.open)
sum.modIR.2022.open
AICc(modIR.2022.open) #1032.403
r.squaredGLMM(modIR.2022.open) #0.9856798

modIR.1990.open <- glmer(data=open.spec, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1990.open <- summary(modIR.1990.open)
sum.modIR.1990.open
AICc(modIR.1990.open) #1032.615
r.squaredGLMM(modIR.1990.open) #0.9857080

modIR.1950.open <- glmer(data=open.spec, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1950.open <- summary(modIR.1950.open)
sum.modIR.1950.open
AICc(modIR.1950.open) #1032.763
r.squaredGLMM(modIR.1950.open) #0.9856814

modIR.1900.open <- glmer(data=open.spec, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE/SPECIES), family = "poisson", 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sum.modIR.1900.open <- summary(modIR.1900.open)
sum.modIR.1900.open
AICc(modIR.1900.open) #1033.463
r.squaredGLMM(modIR.1900.open) #0.9856822

#Akaike weights
weights.modIR.open<-model.sel(modIR.2022.open,modIR.1990.open,modIR.1950.open,modIR.1900.open)
weights.modIR.open
#                                       df logLik        AICc          delta         weight
#modIR.2022.wood                        7 -508.760       1032.4        0.00          0.301
#modIR.1990.wood                        7 -508.866       1032.6        0.21          0.271
#modIR.1950.wood                        7 -508.941       1032.8        0.36          0.251
#modIR.1900.wood                        7 -509.291       1033.5        1.06          0.177

#########################weitere Analysen alle Arten#########################
#Einfluess der einzelnen predictors aufgedröselt für alle Arten auf allen Standorten auf denen sie vorkommen (Werte aber gleich!?)
coef(modIR.1900)$SPECIES

#plotting random slope/ random intercept (sjplot): -> INTERPRETATION??
pp <- plot_model(modIR.1900,type="pred",
                 terms=c("WOOD.s00","SPECIES[Calamosternus granarius, Onthophagus ruficapillus]"),pred.type="re")
pp

#alle Arten anzeigen lassen:  unique(data_modIR$SPECIES)
unique(data_modIR$SPECIES)

#######Arten einzeln###############
#Modell nur mit eine (oder mehreren) ausgewählte/n Arten:
modIR.sp.1900 <- glmer(data=data_modIR[data_modIR$SPECIES == "Colobopterus erraticus",], IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SITE), family = poisson)
sum.modIR.sp.1900 <- summary(modIR.sp.1900)
sum.modIR.sp.1900
AICc(modIR.sp.2022) #
r.squaredGLMM(modIR.sp.2022) #
#predictor Werte für alle Weiden auf denen die Art vorkommt:
coef(modIR.sp.1900)



#Akaike weights
weights.modIR<-model.sel(modIR.sp.2022,modIR.sp.1990,modIR.sp.1950,modIR.sp.1900)
weights.modIR
#                               df logLik   AICc    delta weight
#modIR.sp.1990                  7 -1654.318 3322.9  0.00  0.284
#modIR.sp.1900                  7 -1654.318 3322.9  0.00  0.284
#modIR.sp.2022                  7 -1654.540 3323.3  0.44  0.228
#modIR.sp.1950                  7 -1654.655 3323.6  0.67  0.203



