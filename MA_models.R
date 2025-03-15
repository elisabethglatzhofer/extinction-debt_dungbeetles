########################################################
######################## models ########################
########################################################
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


data_mod <- read.table("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/Variablen_Weiden_alle_Jahre_finalMasterskript.csv",
                            sep = ";",
                            dec = ",",
                            header = T,
                            text = "",
                            stringsAsFactors = T)

data_mod <- data_mod %>%
  mutate(AGRI.s22 = scale(AGRI_2022),
         SETTL.s22 = scale(SETTL_2022),
         WOOD.s22 = scale(WOOD_2022),
         PAST.s22 = scale(PAST_2022),
         anth22 = as.factor(anth_2022),
         gr_history.s22 = scale(gr_history_2022),
         n_livestock.s22 = scale(n_livestock_2022),
         GVE.s22 = scale(GVE_2022),
         AGRI.s90 = scale(AGRI_1990),
         SETTL.s90 = scale(SETTL_1990),
         WOOD.s90 = scale(WOOD_1990),
         PAST.s90 = scale(PAST_1990),
         gr_history.s90 = scale(gr_history_1990),
         AGRI.s50 = scale(AGRI_1950),
         SETTL.s50 = scale(SETTL_1950),
         WOOD.s50 = scale(WOOD_1950),
         PAST.s50 = scale(PAST_1950),
         gr_history.s50 = scale(gr_history_1950),
         AGRI.s00 = scale(AGRI_1900),
         SETTL.s00 = scale(SETTL_1900),
         WOOD.s00 = scale(WOOD_1900),
         PAST.s00 = scale(PAST_1900),
         gr_history.s00 = scale(gr_history_1900),
         border.s22 = scale(border_2022),
         border.s90 = scale(border_1990),
         border.s50 = scale(border_1950),
         border.s00 = scale(border_1900),
         sum.pas.woo.s22 = scale(sum.pas.woo_22),
         sum.pas.woo.s90 = scale(sum.pas.woo_90),
         sum.pas.woo.s50 = scale(sum.pas.woo_50),
         sum.pas.woo.s00 = scale(sum.pas.woo_00),
         border.wood.s22 = scale(border.wood_2022),
         border.wood.s90 = scale(border.wood_1990),
         border.wood.s50 = scale(border.wood_1950),
         border.wood.s00 = scale(border.wood_1900))

x11()
ggcorr(data_mod[,c("WOOD_2022","SETTL_2022","AGRI_2022","GRASS_2022","PAST_2022", "anth_2022","gr_history_2022", "border_2022")], label = T, label_alpha = T)

x11()
ggpairs(data=data_mod,
        columns = c("WOOD_2022","SETTL_2022","AGRI_2022","GRASS_2022","PAST_2022", "anth_2022","gr_history_2022", "border_2022"))

x11()
ggcorr(data_mod[,c("WOOD_1990","SETTL_1990","AGRI_1990","GRASS_1990","PAST_1990", "border_1990")], label = T, label_alpha = T)

x11()
ggcorr(data_mod[,c("WOOD_1950","SETTL_1950","AGRI_1950","GRASS_1950","PAST_1950", "border_1950")], label = T, label_alpha = T)

x11()
ggcorr(data_mod[,c("WOOD_1900","SETTL_1900","AGRI_1900","GRASS_1900","PAST_1900", "border_1900")], label = T, label_alpha = T)

x11()
ggcorr(data_mod[,c("SR","IR")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_open","IR_open")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_closed","IR_closed")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_pos","IR_pos")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_neg","IR_neg")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_end","IR_end")], label = T, label_alpha = T)
x11()
ggcorr(data_mod[,c("SR_para","IR_para")], label = T, label_alpha = T)


####models ohne randon effect & ohne grazing history### (to be able to test for spatial autocorrelation - not possible with glmER)
mod.re_month.2022.anth <- glm(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22, family = poisson())
sum.mod.2022.anth <- summary(mod.re_month.2022.anth)
sum.mod.2022.anth
AICc(mod.re_month.2022.anth)
d.squared.lin <- (mod.re_month.2022.anth$null.deviance - mod.re_month.2022.anth$deviance) / mod.re_month.2022.anth$null.deviance
d.squared.lin #0.177
#r^2 for glm not defined. pseudo-r^2 would be an option, but not directly comparable to and interpretable as r^2
#AICc corrects for small sample size, for bigger sample sizes AICc and AIC become similar
acf(mod.re_month.2022.anth$residuals, type = "correlation")#no autocorrelation

?acf

#2022 without anth
mod.re_month.2022 <- glm(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22, family = poisson)
sum.mod.2022 <- summary(mod.re_month.2022)
sum.mod.2022    
AICc(mod.re_month.2022)
#1990
mod.re_month.1990 <- glm(data=data_mod, SR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90, family = poisson)
sum.mod.1990 <- summary(mod.re_month.1990)
sum.mod.1990
AICc(mod.re_month.1990)
#1950
mod.re_month.1950 <- glm(data=data_mod, SR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50, family = poisson)
sum.mod.1950 <- summary(mod.re_month.1950)
sum.mod.1950   
AICc(mod.re_month.1950)
#1900
mod.re_month.1900 <- glm(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00, family = poisson)
sum.mod.1900 <- summary(mod.re_month.1900)
sum.mod.1900  
AICc(mod.re_month.1900)
####

####model 1, random effect: month####
#2022 with anth
mod1.re_month.2022.anth <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth <- summary(mod1.re_month.2022.anth)
sum.mod1.2022.anth#AIC 222.3
AICc(mod1.re_month.2022.anth)
r.squaredGLMM(mod1.re_month.2022.anth)

mod1.re_month.2022.anth.end <- glmer(data=data_mod, SR_end ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.end <- summary(mod1.re_month.2022.anth.end)
sum.mod1.2022.anth.end
AICc(mod1.re_month.2022.anth.end)
r.squaredGLMM(mod1.re_month.2022.anth.end)

mod1.re_month.2022.anth.para <- glmer(data=data_mod, SR_para ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.para <- summary(mod1.re_month.2022.anth.para)
sum.mod1.2022.anth.para
AICc(mod1.re_month.2022.anth.para)
r.squaredGLMM(mod1.re_month.2022.anth.para)

mod1.re_month.2022.anth.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.neg <- summary(mod1.re_month.2022.anth.neg)
sum.mod1.2022.anth.neg
AICc(mod1.re_month.2022.anth.neg)
r.squaredGLMM(mod1.re_month.2022.anth.neg)

mod1.re_month.2022.anth.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.pos <- summary(mod1.re_month.2022.anth.pos)
sum.mod1.2022.anth.pos
AICc(mod1.re_month.2022.anth.pos)
r.squaredGLMM(mod1.re_month.2022.anth.pos)

mod1.re_month.2022.anth.all <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + sum.pas.woo.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.all <- summary(mod1.re_month.2022.anth.all)
sum.mod1.2022.anth.all
AICc(mod1.re_month.2022.anth.all)
r.squaredGLMM(mod1.re_month.2022.anth.all)

mod1.re_month.2022.anth.op <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.op <- summary(mod1.re_month.2022.anth.op)
sum.mod1.2022.anth.op 
AICc(mod1.re_month.2022.anth.op)
r.squaredGLMM(mod1.re_month.2022.anth.op)

mod1.re_month.2022.anth.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.op.bo <- summary(mod1.re_month.2022.anth.op.bo)
sum.mod1.2022.anth.op.bo 
AICc(mod1.re_month.2022.anth.op.bo)
r.squaredGLMM(mod1.re_month.2022.anth.op.bo)

mod1.re_month.2022.anth.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson())
sum.mod1.2022.anth.cl <- summary(mod1.re_month.2022.anth.cl)
sum.mod1.2022.anth.cl 
AICc(mod1.re_month.2022.anth.cl)
r.squaredGLMM(mod1.re_month.2022.anth.cl)

mod1.re_month.2022.anth.cl.bo <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson())
sum.mod1.2022.anth.cl.bo <- summary(mod1.re_month.2022.anth.cl.bo)
sum.mod1.2022.anth.cl.bo 
AICc(mod1.re_month.2022.anth.cl.bo)
r.squaredGLMM(mod1.re_month.2022.anth.cl.bo)

mod1.re_month.2022.anth.cl.all <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + sum.pas.woo.s22 + anth22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson())
sum.mod1.2022.anth.cl.all <- summary(mod1.re_month.2022.anth.cl.all)
sum.mod1.2022.anth.cl.all 
AICc(mod1.re_month.2022.anth.cl.all)
r.squaredGLMM(mod1.re_month.2022.anth.cl.all)

mod1.re_month.2022.anth.bo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + PAST.s22 + anth22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.bo <- summary(mod1.re_month.2022.anth.bo)
sum.mod1.2022.anth.bo
AICc(mod1.re_month.2022.anth.bo)
r.squaredGLMM(mod1.re_month.2022.anth.bo)

mod1.re_month.2022.anth.bo.woo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + border.wood.s22 + PAST.s22 + anth22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.anth.bo.woo <- summary(mod1.re_month.2022.anth.bo.woo)
sum.mod1.2022.anth.bo.woo
AICc(mod1.re_month.2022.anth.bo.woo)
r.squaredGLMM(mod1.re_month.2022.anth.bo.woo)
#2022 without anth
mod1.re_month.2022 <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022 <- summary(mod1.re_month.2022)
sum.mod1.2022 #AIC 227.4    
AICc(mod1.re_month.2022)
r.squaredGLMM(mod1.re_month.2022)

mod1.re_month.2022.end <- glmer(data=data_mod, SR_end ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.end <- summary(mod1.re_month.2022.end)
sum.mod1.2022.end     
AICc(mod1.re_month.2022.end)
r.squaredGLMM(mod1.re_month.2022.end)

mod1.re_month.2022.para <- glmer(data=data_mod, SR_para ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.end.para <- summary(mod1.re_month.2022.para)
sum.mod1.2022.end.para    
AICc(mod1.re_month.2022.para)
r.squaredGLMM(mod1.re_month.2022.para)

mod1.re_month.2022.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.neg <- summary(mod1.re_month.2022.neg)
sum.mod1.2022.neg    
AICc(mod1.re_month.2022.neg)
r.squaredGLMM(mod1.re_month.2022.neg)

mod1.re_month.2022.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.pos <- summary(mod1.re_month.2022.pos)
sum.mod1.2022.pos    
AICc(mod1.re_month.2022.pos)
r.squaredGLMM(mod1.re_month.2022.pos)

mod1.re_month.2022.all <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + sum.pas.woo.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.all <- summary(mod1.re_month.2022.all)
sum.mod1.2022.all  
AICc(mod1.re_month.2022.all)
r.squaredGLMM(mod1.re_month.2022.all)

mod1.re_month.2022.op <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.op <- summary(mod1.re_month.2022.op)
sum.mod1.2022.op     
AICc(mod1.re_month.2022.op)
r.squaredGLMM(mod1.re_month.2022.op)

mod1.re_month.2022.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.op.bo <- summary(mod1.re_month.2022.op.bo)
sum.mod1.2022.op.bo     
AICc(mod1.re_month.2022.op.bo)
r.squaredGLMM(mod1.re_month.2022.op.bo)

mod1.re_month.2022.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.cl <- summary(mod1.re_month.2022.cl)
sum.mod1.2022.cl     
AICc(mod1.re_month.2022.cl)
r.squaredGLMM(mod1.re_month.2022.cl)

mod1.re_month.2022.cl.bo <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.cl.bo <- summary(mod1.re_month.2022.cl.bo)
sum.mod1.2022.cl.bo  
AICc(mod1.re_month.2022.cl.bo)
r.squaredGLMM(mod1.re_month.2022.cl.bo)

mod1.re_month.2022.cl.all <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + sum.pas.woo.s22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022.cl.all <- summary(mod1.re_month.2022.cl.all)
sum.mod1.2022.cl.all  
AICc(mod1.re_month.2022.cl.all)
r.squaredGLMM(mod1.re_month.2022.cl.all)


mod1.re_month.2022.bo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + gr_history.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod1.2022 <- summary(mod1.re_month.2022.bo)
sum.mod1.2022   
AICc(mod1.re_month.2022.bo)
r.squaredGLMM(mod1.re_month.2022.bo)
#1990
mod1.re_month.1990 <- glmer(data=data_mod, SR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + gr_history.s90 + (1|SA_MONTH), family = poisson)
sum.mod1.1990 <- summary(mod1.re_month.1990)
sum.mod1.1990 #AIC 224.6    
AICc(mod1.re_month.1990)
r.squaredGLMM(mod1.re_month.1990)
#1950
mod1.re_month.1950 <- glmer(data=data_mod, SR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + gr_history.s50 + (1|SA_MONTH), family = poisson)
sum.mod1.1950 <- summary(mod1.re_month.1950)
sum.mod1.1950 #AIC 224.5    
AICc(mod1.re_month.1950)
r.squaredGLMM(mod1.re_month.1950)
#1900
mod1.re_month.1900 <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + gr_history.s00 + (1|SA_MONTH), family = poisson)
sum.mod1.1900 <- summary(mod1.re_month.1900)
sum.mod1.1900 #AIC 220.3        
AICc(mod1.re_month.1900)
r.squaredGLMM(mod1.re_month.1900)

x11()
plot(SR ~ gr_history.s22, data_mod)

x11()
boxplot(SR ~ anth_2022, data=data_mod)

anova(mod1.re_month.2022, mod1.re_month.2022.anth, test = "Chisq")
anova(mod1.re_month.2022.op, mod1.re_month.2022.anth.op, test = "Chisq")
anova(mod1.re_month.2022.cl, mod1.re_month.2022.anth.cl, test = "Chisq")
anova(mod1.re_month.2022.end, mod1.re_month.2022.anth.end, test = "Chisq")
anova(mod1.re_month.2022.para, mod1.re_month.2022.anth.para, test = "Chisq")
anova(mod1.re_month.2022.anth.neg, mod1.re_month.2022.neg, test = "Chisq")
anova(mod1.re_month.2022.pos, mod1.re_month.2022.anth.pos, test = "Chisq")


####model 2, random effect: month OHNE GR_HIST####
#2022 with anth
mod2.re_month.2022.anth <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth <- summary(mod2.re_month.2022.anth)
sum.mod2.2022.anth #AIC 220.6  
AICc(mod2.re_month.2022.anth)
r.squaredGLMM(mod2.re_month.2022.anth)

mod2.re_month.2022.anth.end <- glmer(data=data_mod, SR_end ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.end <- summary(mod2.re_month.2022.anth.end)
sum.mod2.2022.end
AICc(mod2.re_month.2022.anth.end)
r.squaredGLMM(mod2.re_month.2022.anth.end)

mod2.re_month.2022.anth.para <- glmer(data=data_mod, SR_para ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.para <- summary(mod2.re_month.2022.anth.para)
sum.mod2.2022.para
AICc(mod2.re_month.2022.anth.para)
r.squaredGLMM(mod2.re_month.2022.anth.para)

mod2.re_month.2022.anth.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.neg <- summary(mod2.re_month.2022.anth.neg)
sum.mod2.2022.anth.neg   
AICc(mod2.re_month.2022.anth.neg)
r.squaredGLMM(mod2.re_month.2022.anth.neg)

mod2.re_month.2022.anth.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.pos <- summary(mod2.re_month.2022.anth.pos)
sum.mod2.2022.anth.pos   
AICc(mod2.re_month.2022.anth.pos)
r.squaredGLMM(mod2.re_month.2022.anth.pos)

mod2.re_month.2022.anth.all <- glmer(data=data_mod, SR ~ AGRI.s22 + sum.pas.woo.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.all <- summary(mod2.re_month.2022.anth.all)
sum.mod2.2022.anth.all  
AICc(mod2.re_month.2022.anth.all)
r.squaredGLMM(mod2.re_month.2022.anth.all)

mod2.re_month.2022.anth.op <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.op <- summary(mod2.re_month.2022.anth.op)
sum.mod2.2022.anth.op
AICc(mod2.re_month.2022.anth.op)
r.squaredGLMM(mod2.re_month.2022.anth.op)

mod2.re_month.2022.anth.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.op.bo <- summary(mod2.re_month.2022.anth.op.bo)
sum.mod2.2022.anth.op.bo
AICc(mod2.re_month.2022.anth.op.bo)
r.squaredGLMM(mod2.re_month.2022.anth.op.bo)

mod2.re_month.2022.anth.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.cl <- summary(mod2.re_month.2022.anth.cl)
sum.mod2.2022.anth.cl
AICc(mod2.re_month.2022.anth.cl)
r.squaredGLMM(mod2.re_month.2022.anth.cl)

mod2.re_month.2022.anth.cl.bo <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.cl.bo <- summary(mod2.re_month.2022.anth.cl.bo)
sum.mod2.2022.anth.cl.bo
AICc(mod2.re_month.2022.anth.cl.bo)
r.squaredGLMM(mod2.re_month.2022.anth.cl.bo)

mod2.re_month.2022.anth.cl.all <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 +sum.pas.woo.s22 + anth22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.cl.all <- summary(mod2.re_month.2022.anth.cl.all)
sum.mod2.2022.anth.cl.all
AICc(mod2.re_month.2022.anth.cl.all)
r.squaredGLMM(mod2.re_month.2022.anth.cl.all)

mod2.re_month.2022.anth.bo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + anth22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.bo <- summary(mod2.re_month.2022.anth.bo)
sum.mod2.2022.anth.bo 
AICc(mod2.re_month.2022.anth.bo)
r.squaredGLMM(mod2.re_month.2022.anth.bo)

mod2.re_month.2022.anth.bo.woo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + border.wood.s22 + PAST.s22 + anth22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.bo.woo <- summary(mod2.re_month.2022.anth.bo.woo)
sum.mod2.2022.anth.bo.woo
AICc(mod2.re_month.2022.anth.bo.woo)
r.squaredGLMM(mod2.re_month.2022.anth.bo.woo)
#2022 without anth
mod2.re_month.2022 <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022 <- summary(mod2.re_month.2022)
sum.mod2.2022 #AIC 226.3        
AICc(mod2.re_month.2022)
r.squaredGLMM(mod2.re_month.2022)

mod2.re_month.2022.end <- glmer(data=data_mod, SR_end ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.end <- summary(mod2.re_month.2022.end)
sum.mod2.2022.end      
AICc(mod2.re_month.2022.end)
r.squaredGLMM(mod2.re_month.2022.end)

mod2.re_month.2022.para <- glmer(data=data_mod, SR_para ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.para <- summary(mod2.re_month.2022.para)
sum.mod2.2022.para      
AICc(mod2.re_month.2022.para)
r.squaredGLMM(mod2.re_month.2022.para)

mod2.re_month.2022.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.neg <- summary(mod2.re_month.2022.neg)
sum.mod2.2022.neg        
AICc(mod2.re_month.2022.neg)
r.squaredGLMM(mod2.re_month.2022.neg)

mod2.re_month.2022.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.pos <- summary(mod2.re_month.2022.pos)
sum.mod2.2022.pos        
AICc(mod2.re_month.2022.pos)
r.squaredGLMM(mod2.re_month.2022.pos)

mod2.re_month.2022.all <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + sum.pas.woo.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.all <- summary(mod2.re_month.2022.all)
sum.mod2.2022.all        
AICc(mod2.re_month.2022.all)
r.squaredGLMM(mod2.re_month.2022.all)

mod2.re_month.2022.op <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.op <- summary(mod2.re_month.2022.op)
sum.mod2.2022.op        
AICc(mod2.re_month.2022.op)
r.squaredGLMM(mod2.re_month.2022.op)

mod2.re_month.2022.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.op.bo <- summary(mod2.re_month.2022.op.bo)
sum.mod2.2022.op.bo        
AICc(mod2.re_month.2022.op.bo)
r.squaredGLMM(mod2.re_month.2022.op.bo)

mod2.re_month.2022.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.cl <- summary(mod2.re_month.2022.cl)
sum.mod2.2022.cl        
AICc(mod2.re_month.2022.cl)
r.squaredGLMM(mod2.re_month.2022.cl)

mod2.re_month.2022.cl.bo <- glmer(data=data_mod, SR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + border.s22 +(1|SA_MONTH), family = poisson)
sum.mod2.2022.cl.bo <- summary(mod2.re_month.2022.cl.bo)
sum.mod2.2022.cl.bo        
AICc(mod2.re_month.2022.cl.bo)
r.squaredGLMM(mod2.re_month.2022.cl.bo)

mod2.re_month.2022.bo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.bo <- summary(mod2.re_month.2022.bo)
sum.mod2.2022.anth.bo
AICc(mod2.re_month.2022.bo)
r.squaredGLMM(mod2.re_month.2022.bo)

mod2.re_month.2022.bo.woo <- glmer(data=data_mod, SR ~ AGRI.s22 + SETTL.s22 + border.wood.s22 + PAST.s22 + border.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.anth.bo.woo <- summary(mod2.re_month.2022.bo.woo)
sum.mod2.2022.anth.bo.woo
AICc(mod2.re_month.2022.bo.woo)
r.squaredGLMM(mod2.re_month.2022.bo.woo)


anova(mod2.re_month.2022.op, mod2.re_month.2022.anth.op, test = "Chisq")
anova(mod2.re_month.2022, mod2.re_month.2022.bo.woo, test = "Chisq")
anova(mod2.re_month.2022, mod2.re_month.2022.bo, test = "Chisq")

#1990
mod2.re_month.1990 <- glmer(data=data_mod, SR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990 <- summary(mod2.re_month.1990)
sum.mod2.1990 #AIC 223.6        
AICc(mod2.re_month.1990)
r.squaredGLMM(mod2.re_month.1990)

mod2.re_month.1990.end <- glmer(data=data_mod, SR_end ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.end <- summary(mod2.re_month.1990.end)
sum.mod2.1990.end        
AICc(mod2.re_month.1990.end)
r.squaredGLMM(mod2.re_month.1990.end)

mod2.re_month.1990.para <- glmer(data=data_mod, SR_para ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.para <- summary(mod2.re_month.1990.para)
sum.mod2.1990.para        
AICc(mod2.re_month.1990.para)
r.squaredGLMM(mod2.re_month.1990.para)

mod2.re_month.1990.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.neg <- summary(mod2.re_month.1990.neg)
sum.mod2.1990.neg        
AICc(mod2.re_month.1990.neg)
r.squaredGLMM(mod2.re_month.1990.neg)

mod2.re_month.1990.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.pos <- summary(mod2.re_month.1990.pos)
sum.mod2.1990.pos        
AICc(mod2.re_month.1990.pos)
r.squaredGLMM(mod2.re_month.1990.pos)

mod2.re_month.1990.op <- glmer(data=data_mod, SR_open ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.op <- summary(mod2.re_month.1990.op)
sum.mod2.1990.op       
AICc(mod2.re_month.1990.op)
r.squaredGLMM(mod2.re_month.1990.op)

mod2.re_month.1990.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + border.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.op.bo <- summary(mod2.re_month.1990.op.bo)
sum.mod2.1990.op.bo       
AICc(mod2.re_month.1990.op.bo)
r.squaredGLMM(mod2.re_month.1990.op.bo)

mod2.re_month.1990.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.op.cl <- summary(mod2.re_month.1990.cl)
sum.mod2.1990.op.cl       
AICc(mod2.re_month.1990.cl)
r.squaredGLMM(mod2.re_month.1990.cl)

mod2.re_month.1990.bo <- glmer(data=data_mod, SR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + border.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.bo <- summary(mod2.re_month.1990.bo)
sum.mod2.1990.bo     
AICc(mod2.re_month.1990.bo)
r.squaredGLMM(mod2.re_month.1990.bo)
#1950
mod2.re_month.1950 <- glmer(data=data_mod, SR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950 <- summary(mod2.re_month.1950)
sum.mod2.1950 #AIC 223.2        
AICc(mod2.re_month.1950)
r.squaredGLMM(mod2.re_month.1950)

mod2.re_month.1950.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.pos <- summary(mod2.re_month.1950.pos)
sum.mod2.1950.pos       
AICc(mod2.re_month.1950.pos)
r.squaredGLMM(mod2.re_month.1950.pos)

mod2.re_month.1950.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.neg <- summary(mod2.re_month.1950.neg)
sum.mod2.1950.neg       
AICc(mod2.re_month.1950.neg)
r.squaredGLMM(mod2.re_month.1950.neg)

mod2.re_month.1950.end <- glmer(data=data_mod, SR_end ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.end <- summary(mod2.re_month.1950.end)
sum.mod2.1950.end       
AICc(mod2.re_month.1950.end)
r.squaredGLMM(mod2.re_month.1950.end)

mod2.re_month.1950.para <- glmer(data=data_mod, SR_para ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.para <- summary(mod2.re_month.1950.para)
sum.mod2.1950.para       
AICc(mod2.re_month.1950.para)
r.squaredGLMM(mod2.re_month.1950.para)

mod2.re_month.1950.op <- glmer(data=data_mod, SR_open ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.op <- summary(mod2.re_month.1950.op)
sum.mod2.1950.op       
AICc(mod2.re_month.1950.op)
r.squaredGLMM(mod2.re_month.1950.op)

mod2.re_month.1950.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + border.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.op.bo <- summary(mod2.re_month.1950.op.bo)
sum.mod2.1950.op.bo       
AICc(mod2.re_month.1950.op.bo)
r.squaredGLMM(mod2.re_month.1950.op.bo)

mod2.re_month.1950.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.cl <- summary(mod2.re_month.1950.cl)
sum.mod2.1950.cl       
AICc(mod2.re_month.1950.cl)
r.squaredGLMM(mod2.re_month.1950.cl)

mod2.re_month.1950.bo <- glmer(data=data_mod, SR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + border.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.bo <- summary(mod2.re_month.1950.bo)
sum.mod2.1950.bo #AIC 223.2        
AICc(mod2.re_month.1950.bo)
r.squaredGLMM(mod2.re_month.1950.bo)
#1900
mod2.re_month.1900 <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900 <- summary(mod2.re_month.1900)
sum.mod2.1900 #AIC 218.6            
AICc(mod2.re_month.1900)
r.squaredGLMM(mod2.re_month.1900)

mod2.re_month.1900.end <- glmer(data=data_mod, SR_end ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.end <- summary(mod2.re_month.1900.end)
sum.mod2.1900.end            
AICc(mod2.re_month.1900.end)
r.squaredGLMM(mod2.re_month.1900.end)

mod2.re_month.1900.para <- glmer(data=data_mod, SR_para ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.para <- summary(mod2.re_month.1900.para)
sum.mod2.1900.para            
AICc(mod2.re_month.1900.para)
r.squaredGLMM(mod2.re_month.1900.para)

mod2.re_month.1900 <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900 <- summary(mod2.re_month.1900)
sum.mod2.1900 #AIC 218.6            
AICc(mod2.re_month.1900)
r.squaredGLMM(mod2.re_month.1900)

mod2.re_month.1900.neg <- glmer(data=data_mod, SR_neg ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.neg <- summary(mod2.re_month.1900.neg)
sum.mod2.1900.neg             
AICc(mod2.re_month.1900.neg)
r.squaredGLMM(mod2.re_month.1900.neg)

mod2.re_month.1900.pos <- glmer(data=data_mod, SR_pos ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.pos <- summary(mod2.re_month.1900.pos)
sum.mod2.1900.pos          
AICc(mod2.re_month.1900.pos)
r.squaredGLMM(mod2.re_month.1900.pos)

mod2.re_month.1900.all <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + sum.pas.woo.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.all <- summary(mod2.re_month.1900.all)
sum.mod2.1900.all             
AICc(mod2.re_month.1900.all)
r.squaredGLMM(mod2.re_month.1900.all)

mod2.re_month.1900.op <- glmer(data=data_mod, SR_open ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.op <- summary(mod2.re_month.1900.op)
sum.mod2.1900.op        
AICc(mod2.re_month.1900.op)
r.squaredGLMM(mod2.re_month.1900.op)

mod2.re_month.1900.op.bo <- glmer(data=data_mod, SR_open ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + border.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.op.bo <- summary(mod2.re_month.1900.op.bo)
sum.mod2.1900.op.bo        
AICc(mod2.re_month.1900.op.bo)
r.squaredGLMM(mod2.re_month.1900.op.bo)

mod2.re_month.1900.cl <- glmer(data=data_mod, SR_closed ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.cl <- summary(mod2.re_month.1900.cl)
sum.mod2.1900.cl        
AICc(mod2.re_month.1900.cl)
r.squaredGLMM(mod2.re_month.1900.cl)

mod2.re_month.1900.cl.bo <- glmer(data=data_mod, SR_closed ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + border.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.cl.bo <- summary(mod2.re_month.1900.cl.bo)
sum.mod2.1900.cl.bo        
AICc(mod2.re_month.1900.cl.bo)
r.squaredGLMM(mod2.re_month.1900.cl.bo)

mod2.re_month.1900.cl.all <- glmer(data=data_mod, SR_closed ~ AGRI.s00 + SETTL.s00 + sum.pas.woo.s00 + border.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.cl.bo <- summary(mod2.re_month.1900.cl.all)
sum.mod2.1900.cl.bo        
AICc(mod2.re_month.1900.cl.all)
r.squaredGLMM(mod2.re_month.1900.cl.all)

mod2.re_month.1900.bo <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + border.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.bo <- summary(mod2.re_month.1900.bo)
sum.mod2.1900.bo            
AICc(mod2.re_month.1900.bo)
r.squaredGLMM(mod2.re_month.1900.bo)

mod2.re_month.1900.bo.woo <- glmer(data=data_mod, SR ~ AGRI.s00 + SETTL.s00 + border.wood.s00 + PAST.s00 + border.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.bo.woo <- summary(mod2.re_month.1900.bo.woo)
sum.mod2.1900.bo.woo            
AICc(mod2.re_month.1900.bo.woo)
r.squaredGLMM(mod2.re_month.1900.bo.woo)

anova(mod2.re_month.2022.anth, mod2.re_month.2022, test = "Chisq")
anova(mod2.re_month.2022.anth.cl, mod2.re_month.2022.cl, test = "Chisq")


weights.mod2<-model.sel(mod2.re_month.2022,mod2.re_month.1990,mod2.re_month.1950,mod2.re_month.1900)
weights.mod2
weights.mod2.op<-model.sel(mod2.re_month.2022.op,mod2.re_month.1990.op,mod2.re_month.1950.op,mod2.re_month.1900.op)
weights.mod2.op
weights.mod2.cl<-model.sel(mod2.re_month.2022.cl,mod2.re_month.1990.cl,mod2.re_month.1950.cl,mod2.re_month.1900.cl)
weights.mod2.cl
weights.mod2.pos<-model.sel(mod2.re_month.2022.pos,mod2.re_month.1990.pos,mod2.re_month.1950.pos,mod2.re_month.1900.pos)
weights.mod2.pos
weights.mod2.neg<-model.sel(mod2.re_month.2022.neg,mod2.re_month.1990.neg,mod2.re_month.1950.neg,mod2.re_month.1900.neg)
weights.mod2.neg
weights.mod2.para<-model.sel(mod2.re_month.2022.para,mod2.re_month.1990.para,mod2.re_month.1950.para,mod2.re_month.1900.para)
weights.mod2.para
weights.mod2.endo<-model.sel(mod2.re_month.2022.end,mod2.re_month.1990.end,mod2.re_month.1950.end,mod2.re_month.1900.end)
weights.mod2.endo

###Visualisation 2022.anth
#with anth vs. without
windows()
par(mfrow=c(2,1))
qqnorm(resid(mod2.re_month.2022), main = "2022; random effect: month")
qqline(resid(mod2.re_month.2022))
qqnorm(resid(mod2.re_month.2022.anth), main = "2022 + anth; random effect: month")
qqline(resid(mod2.re_month.2022.anth))

p1 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "AGRI.s")
p2 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "SETTL.s")
p3 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "WOOD.s")
p4 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "PAST.s")
p5 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "anth")
p6 <- plot_model(mod1.re_month.2022.anth, type = "eff", terms = "gr_history.s")
(p1 | p2 | p3)/
  (p4 | p5 | p6)+
  plot_annotation(title = "2022 + anth; random effect: month")

p7 <- plot_model(mod1.re_month.2022.anth, type = "est", show.values = TRUE, value.offset = .3, value.size = 3)
p8 <- plot_model(mod1.re_month.2022.anth,type = "diag")
(p7 | p8)+
  plot_annotation(title = "2022 + anth; random effect: month")

x11()
plot_model(mod1.re_month.2022.anth, type = "est", title = "2022 anth", show.values = TRUE, value.offset = .3)
x11()
plot_model(mod1.re_month.2022.anth, type = "eff", terms = "anth22")
###

###comparizson all years
windows()
par(mfrow=c(2,2))
qqnorm(resid(mod2.re_month.2022), main = "2022, random effect: month")
qqline(resid(mod2.re_month.2022))
qqnorm(resid(mod2.re_month.1990), main = "1990, random effect: month")
qqline(resid(mod2.re_month.1990))
qqnorm(resid(mod2.re_month.1950), main = "1950, random effect: month")
qqline(resid(mod2.re_month.1950))
qqnorm(resid(mod2.re_month.1900), main = "1900, random effect: month")
qqline(resid(mod2.re_month.1900))

re_month.2022.est <- plot_model(mod2.re_month.2022, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
re_month.1990.est <- plot_model(mod2.re_month.1990, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
re_month.1950.est <- plot_model(mod2.re_month.1950, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
re_month.1900.est <- plot_model(mod2.re_month.1900, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
(re_month.2022.est | re_month.1990.est | re_month.1950.est | re_month.1900.est) +
  plot_annotation(title = "estimates. random effect: month")

x11()
plot_model(mod2.re_month.1900, type = "eff", terms = "WOOD.s00")

mod2.op.1900.est <- plot_model(mod2.re_month.1900.op, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.op.1950.est <- plot_model(mod2.re_month.1950.op, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.op.1990.est <- plot_model(mod2.re_month.1990.op, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.op.2022.est <- plot_model(mod2.re_month.2022.op, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.op.1900.est | mod2.op.1950.est | mod2.op.1990.est | mod2.op.2022.est) +
  plot_annotation(title = "estimates. open land species")

mod2.cl.1900.est <- plot_model(mod2.re_month.1900.cl, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.cl.1950.est <- plot_model(mod2.re_month.1950.cl, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.cl.1990.est <- plot_model(mod2.re_month.1990.cl, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.cl.2022.est <- plot_model(mod2.re_month.2022.cl, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.cl.1900.est | mod2.cl.1950.est | mod2.cl.1990.est | mod2.cl.2022.est) +
  plot_annotation(title = "estimates. semiopen/ forest species")

mod2.para.1900.est <- plot_model(mod2.re_month.1900.para, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.para.1950.est <- plot_model(mod2.re_month.1950.para, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.para.1990.est <- plot_model(mod2.re_month.1990.para, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.para.2022.est <- plot_model(mod2.re_month.2022.para, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.para.1900.est | mod2.para.1950.est | mod2.para.1990.est | mod2.para.2022.est) +
  plot_annotation(title = "estimates. paracoprid species")

mod2.end.1900.est <- plot_model(mod2.re_month.1900.end, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.end.1950.est <- plot_model(mod2.re_month.1950.end, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.end.1990.est <- plot_model(mod2.re_month.1990.end, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.end.2022.est <- plot_model(mod2.re_month.2022.end, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.end.1900.est | mod2.end.1950.est | mod2.end.1990.est | mod2.end.2022.est) +
  plot_annotation(title = "estimates. endocoprid species")

mod2.neg.1900.est <- plot_model(mod2.re_month.1900.neg, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.neg.1950.est <- plot_model(mod2.re_month.1950.neg, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.neg.1990.est <- plot_model(mod2.re_month.1990.neg, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.neg.2022.est <- plot_model(mod2.re_month.2022.neg, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.neg.1900.est | mod2.neg.1950.est | mod2.neg.1990.est | mod2.neg.2022.est) +
  plot_annotation(title = "estimates. declining species")

mod2.pos.1900.est <- plot_model(mod2.re_month.1900.pos, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
mod2.pos.1950.est <- plot_model(mod2.re_month.1950.pos, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
mod2.pos.1990.est <- plot_model(mod2.re_month.1990.pos, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
mod2.pos.2022.est <- plot_model(mod2.re_month.2022.pos, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
(mod2.pos.1900.est | mod2.pos.1950.est | mod2.pos.1990.est | mod2.pos.2022.est) +
  plot_annotation(title = "estimates. increasing species")

####################################
###models with individual numbers###
####################################
x11()
ggcorr(data_mod[,c("SR","IR")], label = T, label_alpha = T)

cor(data_mod$SR, data_mod$IR) #0.6093975
cor.test(data_mod$SR, data_mod$IR) #p-value = 0.0001306

#2022 without anth
mod2.re_month.2022.IR <- glmer(data=data_mod, IR ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.IR <- summary(mod2.re_month.2022.IR)
sum.mod2.2022.IR #AIC 226.3        
AICc(mod2.re_month.2022.IR)
r.squaredGLMM(mod2.re_month.2022.IR)

mod2.re_month.2022.end.IR <- glmer(data=data_mod, IR_end ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.end.IR <- summary(mod2.re_month.2022.end.IR)
sum.mod2.2022.end.IR   
AICc(mod2.re_month.2022.end.IR)
r.squaredGLMM(mod2.re_month.2022.end.IR)

mod2.re_month.2022.para.IR <- glmer(data=data_mod, IR_para ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.para.IR <- summary(mod2.re_month.2022.para.IR)
sum.mod2.2022.para.IR      
AICc(mod2.re_month.2022.para.IR)
r.squaredGLMM(mod2.re_month.2022.para.IR)

mod2.re_month.2022.neg.IR <- glmer(data=data_mod, IR_neg ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.neg.IR <- summary(mod2.re_month.2022.neg.IR)
sum.mod2.2022.neg.IR        
AICc(mod2.re_month.2022.neg.IR)
r.squaredGLMM(mod2.re_month.2022.neg.IR)

mod2.re_month.2022.pos.IR <- glmer(data=data_mod, IR_pos ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.pos.IR <- summary(mod2.re_month.2022.pos.IR)
sum.mod2.2022.pos.IR        
AICc(mod2.re_month.2022.pos.IR)
r.squaredGLMM(mod2.re_month.2022.pos.IR)

mod2.re_month.2022.op.IR <- glmer(data=data_mod, IR_open ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.op.IR <- summary(mod2.re_month.2022.op.IR)
sum.mod2.2022.op.IR        
AICc(mod2.re_month.2022.op.IR)
r.squaredGLMM(mod2.re_month.2022.op.IR)

mod2.re_month.2022.cl.IR <- glmer(data=data_mod, IR_closed ~ AGRI.s22 + SETTL.s22 + WOOD.s22 + PAST.s22 + (1|SA_MONTH), family = poisson)
sum.mod2.2022.cl.IR <- summary(mod2.re_month.2022.cl.IR)
sum.mod2.2022.cl.IR        
AICc(mod2.re_month.2022.cl.IR)
r.squaredGLMM(mod2.re_month.2022.cl.IR)

#1990
mod2.re_month.1990.IR <- glmer(data=data_mod, IR ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.IR <- summary(mod2.re_month.1990.IR)
sum.mod2.1990.IR #AIC 223.6        
AICc(mod2.re_month.1990.IR)
r.squaredGLMM(mod2.re_month.1990.IR)

mod2.re_month.1990.end.IR <- glmer(data=data_mod, IR_end ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.end.IR <- summary(mod2.re_month.1990.end.IR)
sum.mod2.1990.end.IR       
AICc(mod2.re_month.1990.end.IR)
r.squaredGLMM(mod2.re_month.1990.end.IR)

mod2.re_month.1990.para.IR <- glmer(data=data_mod, IR_para ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.para.IR <- summary(mod2.re_month.1990.para.IR)
sum.mod2.1990.para.IR        
AICc(mod2.re_month.1990.para.IR)
r.squaredGLMM(mod2.re_month.1990.para.IR)

mod2.re_month.1990.neg.IR <- glmer(data=data_mod, IR_neg ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.neg.IR <- summary(mod2.re_month.1990.neg.IR)
sum.mod2.1990.neg.IR        
AICc(mod2.re_month.1990.neg.IR)
r.squaredGLMM(mod2.re_month.1990.neg.IR)

mod2.re_month.1990.pos.IR <- glmer(data=data_mod, IR_pos ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.pos.IR <- summary(mod2.re_month.1990.pos.IR)
sum.mod2.1990.pos.IR        
AICc(mod2.re_month.1990.pos.IR)
r.squaredGLMM(mod2.re_month.1990.pos.IR)

mod2.re_month.1990.op.IR <- glmer(data=data_mod, IR_open ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.op.IR <- summary(mod2.re_month.1990.op.IR)
sum.mod2.1990.op.IR       
AICc(mod2.re_month.1990.op.IR)
r.squaredGLMM(mod2.re_month.1990.op.IR)

mod2.re_month.1990.cl.IR <- glmer(data=data_mod, IR_closed ~ AGRI.s90 + SETTL.s90 + WOOD.s90 + PAST.s90 + (1|SA_MONTH), family = poisson)
sum.mod2.1990.cl.IR <- summary(mod2.re_month.1990.cl.IR)
sum.mod2.1990.cl.IR       
AICc(mod2.re_month.1990.cl.IR)
r.squaredGLMM(mod2.re_month.1990.cl.IR)

#1950
mod2.re_month.1950.IR <- glmer(data=data_mod, IR ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.IR <- summary(mod2.re_month.1950.IR)
sum.mod2.1950.IR #AIC 223.2        
AICc(mod2.re_month.1950.IR)
r.squaredGLMM(mod2.re_month.1950.IR)

mod2.re_month.1950.pos.IR <- glmer(data=data_mod, IR_pos ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.pos.IR <- summary(mod2.re_month.1950.pos.IR)
sum.mod2.1950.pos.IR       
AICc(mod2.re_month.1950.pos.IR)
r.squaredGLMM(mod2.re_month.1950.pos.IR)

mod2.re_month.1950.neg.IR <- glmer(data=data_mod, IR_neg ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.neg.IR <- summary(mod2.re_month.1950.neg.IR)
sum.mod2.1950.neg.IR      
AICc(mod2.re_month.1950.neg.IR)
r.squaredGLMM(mod2.re_month.1950.neg.IR)

mod2.re_month.1950.end.IR <- glmer(data=data_mod, IR_end ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.end.IR <- summary(mod2.re_month.1950.end.IR)
sum.mod2.1950.end.IR       
AICc(mod2.re_month.1950.end.IR)
r.squaredGLMM(mod2.re_month.1950.end.IR)

mod2.re_month.1950.para.IR <- glmer(data=data_mod, IR_para ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.para.IR <- summary(mod2.re_month.1950.para.IR)
sum.mod2.1950.para.IR       
AICc(mod2.re_month.1950.para.IR)
r.squaredGLMM(mod2.re_month.1950.para.IR)

mod2.re_month.1950.op.IR <- glmer(data=data_mod, IR_open ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.op.IR <- summary(mod2.re_month.1950.op.IR)
sum.mod2.1950.op.IR       
AICc(mod2.re_month.1950.op.IR)
r.squaredGLMM(mod2.re_month.1950.op.IR)

mod2.re_month.1950.cl.IR <- glmer(data=data_mod, IR_closed ~ AGRI.s50 + SETTL.s50 + WOOD.s50 + PAST.s50 + (1|SA_MONTH), family = poisson)
sum.mod2.1950.cl.IR <- summary(mod2.re_month.1950.cl.IR)
sum.mod2.1950.cl.IR       
AICc(mod2.re_month.1950.cl.IR)
r.squaredGLMM(mod2.re_month.1950.cl.IR)

#1900
mod2.re_month.1900.IR <- glmer(data=data_mod, IR ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.IR <- summary(mod2.re_month.1900.IR)
sum.mod2.1900.IR #AIC 218.6            
AICc(mod2.re_month.1900.IR)
r.squaredGLMM(mod2.re_month.1900.IR)

mod2.re_month.1900.end.IR <- glmer(data=data_mod, IR_end ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.end.IR <- summary(mod2.re_month.1900.end.IR)
sum.mod2.1900.end.IR            
AICc(mod2.re_month.1900.end.IR)
r.squaredGLMM(mod2.re_month.1900.end.IR)

mod2.re_month.1900.para.IR <- glmer(data=data_mod, IR_para ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.para.IR <- summary(mod2.re_month.1900.para.IR)
sum.mod2.1900.para.IR            
AICc(mod2.re_month.1900.para.IR)
r.squaredGLMM(mod2.re_month.1900.para.IR)

mod2.re_month.1900.neg.IR <- glmer(data=data_mod, IR_neg ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.neg.IR <- summary(mod2.re_month.1900.neg.IR)
sum.mod2.1900.neg.IR             
AICc(mod2.re_month.1900.neg.IR)
r.squaredGLMM(mod2.re_month.1900.neg.IR)

mod2.re_month.1900.pos.IR <- glmer(data=data_mod, IR_pos ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.pos.IR <- summary(mod2.re_month.1900.pos.IR)
sum.mod2.1900.pos.IR          
AICc(mod2.re_month.1900.pos.IR)
r.squaredGLMM(mod2.re_month.1900.pos.IR)

mod2.re_month.1900.op.IR <- glmer(data=data_mod, IR_open ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.op.IR <- summary(mod2.re_month.1900.op.IR)
sum.mod2.1900.op.IR        
AICc(mod2.re_month.1900.op.IR)
r.squaredGLMM(mod2.re_month.1900.op.IR)

mod2.re_month.1900.cl.IR <- glmer(data=data_mod, IR_closed ~ AGRI.s00 + SETTL.s00 + WOOD.s00 + PAST.s00 + (1|SA_MONTH), family = poisson)
sum.mod2.1900.cl.IR <- summary(mod2.re_month.1900.cl.IR)
sum.mod2.1900.cl.IR        
AICc(mod2.re_month.1900.cl.IR)
r.squaredGLMM(mod2.re_month.1900.cl.IR)


weights.mod2.IR<-model.sel(mod2.re_month.2022.IR,mod2.re_month.1990.IR,mod2.re_month.1950.IR,mod2.re_month.1900.IR)
weights.mod2.IR
weights.mod2.op.IR<-model.sel(mod2.re_month.2022.op.IR,mod2.re_month.1990.op.IR,mod2.re_month.1950.op.IR,mod2.re_month.1900.op.IR)
weights.mod2.op.IR
weights.mod2.cl.IR<-model.sel(mod2.re_month.2022.cl.IR,mod2.re_month.1990.cl.IR,mod2.re_month.1950.cl.IR,mod2.re_month.1900.cl.IR)
weights.mod2.cl.IR
weights.mod2.pos.IR<-model.sel(mod2.re_month.2022.pos.IR,mod2.re_month.1990.pos.IR,mod2.re_month.1950.pos.IR,mod2.re_month.1900.pos.IR)
weights.mod2.pos.IR
weights.mod2.neg.IR<-model.sel(mod2.re_month.2022.neg.IR,mod2.re_month.1990.neg.IR,mod2.re_month.1950.neg.IR,mod2.re_month.1900.neg.IR)
weights.mod2.neg.IR
weights.mod2.para.IR<-model.sel(mod2.re_month.2022.para.IR,mod2.re_month.1990.para.IR,mod2.re_month.1950.para.IR,mod2.re_month.1900.para.IR)
weights.mod2.para.IR
weights.mod2.endo.IR<-model.sel(mod2.re_month.2022.end.IR,mod2.re_month.1990.end.IR,mod2.re_month.1950.end.IR,mod2.re_month.1900.end.IR)
weights.mod2.endo.IR



#################################################################################
######################VERWORFENE MODELLE#########################################
#################################################################################

####model 2, random effect: region1 (Einteilung alle nach geografischer Lage)####

#2022 with anth
mod2.re_region.2022.anth <- glmer(data=data_corr2022, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + anth + gr_history.s + (1|REGION), family = poisson)
sum.mod2.2022.anth <- summary(mod2.re_region.2022.anth)
sum.mod2.2022.anth #AIC 217.9 
#2022 without anth
mod2.re_region.2022 <- glmer(data=data_corr2022, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION), family = poisson)
sum.mod2.2022 <- summary(mod2.re_region.2022)
sum.mod2.2022 #AIC 217.9 
#1990 
mod2.re_region.1990 <- glmer(data=data_corr1990, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION), family = poisson)
sum.mod2.1990 <- summary(mod2.re_region.1990)
sum.mod2.1990 #AIC 217.7
#1950 
mod2.re_region.1950 <- glmer(data=data_corr1950, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION), family = poisson)
sum.mod2.1950 <- summary(mod2.re_region.1950)
sum.mod2.1950 #AIC 215.4 
#1900 
mod2.re_region.1900 <- glmer(data=data_corr1900, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION), family = poisson)
sum.mod2.1900 <- summary(mod2.re_region.1900)
sum.mod2.1900 #AIC 215.8 
#p-values 
#                       2022.anth    2022        1990      1950       1900
#           anth1                  
#    gr_history.s              
#         SETTL.s               
#          AGRI.s              
#          WOOD.s           
#          PAST.s              


#r^2-values
r2beta(mod2.re_region.2022) #0.176
r2beta(mod2.re_region.2022.anth) #0.243
r2beta(mod2.re_region.1990) #0.172
r2beta(mod2.re_region.1950) #0.226
r2beta(mod2.re_region.1900) #0.221

###Visualisation 2022.anth
x11()
qqnorm(resid(mod2.re_region.2022.anth), main = "mod2 2022 anth, random effect: region1")
qqline(resid(mod2.re_region.2022.anth))
legend(x = "bottomright", legend = "r^2=0.243, AIC=217.9")
#with anth vs. without
windows()
par(mfrow=c(2,1))
qqnorm(resid(mod2.re_region.2022), main = "2022; random effect: region1")
qqline(resid(mod2.re_region.2022))
legend(x = "bottomright", legend = "r^2=0.176, AIC=217.9")
qqnorm(resid(mod2.re_region.2022.anth), main = "2022 + anth; random effect: region")
qqline(resid(mod2.re_region.2022.anth))
legend(x = "bottomright", legend = "r^2=0.243, AIC=217.9")

p1 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "AGRI.s")
p2 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "SETTL.s")
p3 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "WOOD.s")
p4 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "PAST.s")
p5 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "anth")
p6 <- plot_model(mod2.re_region.2022.anth, type = "eff", terms = "gr_history.s")
(p1 | p2 | p3)/
  (p4 | p5 | p6)+
  plot_annotation(title = "2022 + anth; random effect: region")

p7 <- plot_model(mod2.re_region.2022.anth, type = "est", show.values = TRUE, value.offset = .3, value.size = 3)
p8 <- plot_model(mod2.re_region.2022.anth,type = "diag")
(p7 | p8)+
  plot_annotation(title = "2022 + anth; random effect: region")

x11()
plot_model(mod2.re_region.2022.anth, type = "est", title = "2022 anth", show.values = TRUE, value.offset = .3)
x11()
plot_model(mod2.re_region.2022.anth,type = "diag")
###

###comparison all years
windows()
par(mfrow=c(2,2))
qqnorm(resid(mod2.re_region.2022), main = "2022, random effect: region")
qqline(resid(mod2.re_region.2022))
legend(x = "bottomright", legend = "r^2=0.176, AIC=217.9")
qqnorm(resid(mod2.re_region.1990), main = "1990, random effect: region")
qqline(resid(mod2.re_region.1990))
legend(x = "bottomright", legend = "r^2=0.172, AIC=217.7")
qqnorm(resid(mod2.re_region.1950), main = "1950, random effect: region")
qqline(resid(mod2.re_region.1950))
legend(x = "bottomright", legend = "r^2=0.226, AIC=215.4")
qqnorm(resid(mod2.re_region.1900), main = "1900, random effect: region")
qqline(resid(mod2.re_region.1900))
legend(x = "bottomright", legend = "r^2=0.221, AIC=215.8")

re_region.2022.est <- plot_model(mod2.re_region.2022, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
re_region.1990.est <- plot_model(mod2.re_region.1990, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
re_region.1950.est <- plot_model(mod2.re_region.1950, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
re_region.1900.est <- plot_model(mod2.re_region.1900, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
(re_region.2022.est | re_region.1990.est | re_region.1950.est | re_region.1900.est) +
  plot_annotation(title = "estimates. random effect: region")

x11()
plot_model(mod2.re_region.2022,type = "diag")
x11()
plot_model(mod2.re_region.1990,type = "diag")
x11()
plot_model(mod2.re_region.1950,type = "diag")
x11()
plot_model(mod2.re_region.1900,type = "diag")
###

####model 3, random effects: month & region1####
#2022 with anth
mod3.re_month_region.2022.anth <- glmer(data=data_corr2022, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + anth + gr_history.s + (1|REGION) + (1|SA_MONTH), family = poisson)
sum.mod3.2022.anth <- summary(mod3.re_month_region.2022.anth)
sum.mod3.2022.anth #AIC 219.9    
#2022 without anth
mod3.re_month_region.2022 <- glmer(data=data_corr2022, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION) + (1|SA_MONTH), family = poisson)
sum.mod3.2022 <- summary(mod3.re_month_region.2022)
sum.mod3.2022 #AIC 219.9   
#1990
mod3.re_month_region.1990 <- glmer(data=data_corr1990, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION) + (1|SA_MONTH), family = poisson)
sum.mod3.1990 <- summary(mod3.re_month_region.1990)
sum.mod3.1990 #AIC 219.7
#1950
mod3.re_month_region.1950 <- glmer(data=data_corr1950, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION) + (1|SA_MONTH), family = poisson)
sum.mod3.1950 <- summary(mod3.re_month_region.1950)
sum.mod3.1950 #AIC 217.4 
#1900
mod3.re_month_region.1900 <- glmer(data=data_corr1900, SR ~ AGRI.s + SETTL.s + WOOD.s + PAST.s + gr_history.s + (1|REGION) + (1|SA_MONTH), family = poisson)
sum.mod3.1900 <- summary(mod3.re_month_region.1900)
sum.mod3.1900 #AIC 217.8
#p-values   
#                       2022.anth    2022        1990      1950       1900
#           anth1                     
#    gr_history.s              
#         SETTL.s               
#          AGRI.s              
#          WOOD.s          
#          PAST.s             


#r^2-values
r2beta(mod3.re_month_region.2022, method = "nsj") #0.122 singular fit!
r2beta(mod3.re_month_region.2022.anth, method = "nsj") #0.167 singular fit!
r2beta(mod3.re_month_region.1990, method = "nsj") #0.119 singular fit!
r2beta(mod3.re_month_region.1950, method = "nsj") #0.158 singular fit!
r2beta(mod3.re_month_region.1900, method = "nsj") #0.155 singular fit!

###Visualisation 2022.anth
x11()
qqnorm(resid(mod3.re_month_region.2022.anth), main = "mod3 2022 anth, random effect: region1 + month")
qqline(resid(mod3.re_month_region.2022.anth))
legend(x = "bottomright", legend = "r^2=0.167, AIC=219.9")
#with anth vs. without
windows()
par(mfrow=c(2,1))
qqnorm(resid(mod3.re_month_region.2022), main = "2022; random effect: region1 + month")
qqline(resid(mod3.re_month_region.2022))
legend(x = "bottomright", legend = "r^2=0.122, AIC=219.9")
qqnorm(resid(mod3.re_month_region.2022.anth), main = "2022 + anth; random effect: region1 + month")
qqline(resid(mod3.re_month_region.2022.anth))
legend(x = "bottomright", legend = "r^2=0.167, AIC=219.9")

p1 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "AGRI.s")
p2 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "SETTL.s")
p3 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "WOOD.s")
p4 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "PAST.s")
p5 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "anth")
p6 <- plot_model(mod3.re_month_region.2022.anth, type = "eff", terms = "gr_history.s")
(p1 | p2 | p3)/
  (p4 | p5 | p6)+
  plot_annotation(title = "2022 + anth; random effect: region1 + month")

p7 <- plot_model(mod3.re_month_region.2022.anth, type = "est", show.values = TRUE, value.offset = .3, value.size = 3)
p8 <- plot_model(mod3.re_month_region.2022.anth,type = "diag")
(p7 | p8)+
  plot_annotation(title = "2022 + anth; random effect: region1 + month")

x11()
plot_model(mod3.re_month_region.2022.anth, type = "est", title = "2022 anth", show.values = TRUE, value.offset = .3)
x11()
plot_model(mod3.re_month_region.2022.anth,type = "diag")
###

###comparizson all years
windows()
par(mfrow=c(2,2))
qqnorm(resid(mod3.re_month_region.2022), main = "2022, random effect: region1 + month")
qqline(resid(mod3.re_month_region.2022))
legend(x = "bottomright", legend = "r^2=0.122, AIC=219.9")
qqnorm(resid(mod3.re_month_region.1990), main = "1990, random effect: region1 + month")
qqline(resid(mod3.re_month_region.1990))
legend(x = "bottomright", legend = "r^2=0.119, AIC=219.7")
qqnorm(resid(mod3.re_month_region.1950), main = "1950, random effect: region1 + month")
qqline(resid(mod3.re_month_region.1950))
legend(x = "bottomright", legend = "r^2=0.158, AIC=217.4")
qqnorm(resid(mod3.re_month_region.1900), main = "1900, random effect: region1 + month")
qqline(resid(mod3.re_month_region.1900))
legend(x = "bottomright", legend = "r^2=0.155, AIC=217.8")


re_region.month.2022.est <- plot_model(mod3.re_month_region.2022, type = "est", title = "2022", show.values = TRUE, value.offset = .3)
re_region.month.1990.est <- plot_model(mod3.re_month_region.1990, type = "est", title = "1990", show.values = TRUE, value.offset = .3)
re_region.month.1950.est <- plot_model(mod3.re_month_region.1950, type = "est", title = "1950", show.values = TRUE, value.offset = .3)
re_region.month.1900.est <- plot_model(mod3.re_month_region.1900, type = "est", title = "1900", show.values = TRUE, value.offset = .3)
(re_region.month.2022.est | re_region.month.1990.est | re_region.month.1950.est | re_region.month.1900.est) +
  plot_annotation(title = "estimates. random effect: region1 + month")

x11()
plot_model(mod3.re_month_region.2022,type = "diag")
x11()
plot_model(mod3.re_month_region.1990,type = "diag")
x11()
plot_model(mod3.re_month_region.1950,type = "diag")
x11()
plot_model(mod3.re_month_region.1900,type = "diag")
