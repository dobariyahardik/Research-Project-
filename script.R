library(readxl)
library(tidyr)
library(patchwork)
library(ggplot2)
library(psych)
library(lme4)
library(car)
library(effects)



# GB: Did you really use ALL these libraries??? If not remove the unused ones
#Hardik : package remove
#------- data set -----------



final_sheet <- read_excel("final.sheet.xlsx")

RPC1 <- final_sheet

names(RPC1)[names(RPC1) == "0.5MM"] <- "half"
names(RPC1)[names(RPC1) == "2MM"] <- "two"
names(RPC1)[names(RPC1) == "4MM"] <- "four"
names(RPC1)[names(RPC1) == "8MM"] <- "eight"
names(RPC1)[names(RPC1) == "time.point"] <- "time"

RPC1$year <- as.factor(RPC1$year)
RPC1$time <- as.factor(RPC1$time)
RPC1$position <- as.factor(RPC1$position)
RPC1$CH <- as.factor(RPC1$CH)


#Each CH in data frame have two different treatment(apply filter in dataset to see the treatment per CH(channel)
#model fit with CH(as random factor) make random effect not interpretable( example see in model fitting step)
#create new variable treatment1(month+ treatment= treatment1)
#please see "random factor inclusion" in MODEL FITTING step in script 

RPC1<-  RPC1 %>% unite(treatment1,time,treatment,remove = FALSE)
RPC1$treatment1 <- as.factor(RPC1$treatment1)




#-- NA CHECK IN DATA  ---------
options(max.print=1000000)
which(is.na(RPC1))
is.na(RPC1[,1:13])




#---- DATASET OVERALL REVIEW ----------


# RPC1  

summary(RPC1$abundance)
summary(RPC1$taxa.richness)
summary(RPC1$"half")
summary(RPC1$"two")
summary(RPC1$"four")
summary(RPC1$"eight")
summary(RPC1$"TTL")





#POM box plot fraction wise -raw data

My_Theme = theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 10,face = "bold"),
  axis.title.y = element_text(size = 10))
  


RPC1$time <- factor(RPC1$time, levels = c("June", "August", "October", "May", "July"))

om1 <-     ggplot2::ggplot(RPC1, aes(x=time, y=half, fill=year)) +
  geom_boxplot() + facet_wrap(~position) +
  scale_fill_manual(values=c("green", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("collection time") +
  ylab(" weight of POM(in mgs)") +
  ggtitle(" 0.5 mm POM -raw data ")+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+theme(plot.title = element_text(hjust = 0.5))





om2 <-     ggplot2::ggplot(RPC1, aes(x=time, y=two, fill=year)) +
  geom_boxplot() + facet_wrap(~position) +
  scale_fill_manual(values=c("green", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("collection time") +
  ylab(" weight of POM(in mgs)") +
  ggtitle(" 2 mm  POM -raw data ")+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))





om3 <-  ggplot2::ggplot(RPC1, aes(x=time, y=four, fill=year)) +
  geom_boxplot() + facet_wrap(~position) +
  scale_fill_manual(values=c("green", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("collection time") +
  ylab(" weight of POM(in mgs)") +
  ggtitle(" 4 mm  POM -raw data ")+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




om4 <-  ggplot2::ggplot(RPC1, aes(x=time, y=eight, fill=year)) +
  geom_boxplot() + facet_wrap(~position) +
  scale_fill_manual(values=c("green", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("collection time") +
  ylab(" weight of POM(in mgs)") +
  ggtitle(" 8 mm  POM -raw data ")+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




om5<-  ggplot2::ggplot(RPC1, aes(x=time, y=TTL, fill=year)) +
  geom_boxplot() + facet_wrap(~position) +
  scale_fill_manual(values=c("green", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("collection time") +
  ylab(" weight of POM(in mgs)") +
  ggtitle(" total POM -raw data ")+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))



#combine plot with patchwork package

legend_patch <- plot_layout(guides = "collect") 

om1 <- (om1+om2)+legend_patch+plot_annotation(tag_levels = 'A')
om2 <- (om3+om4)+legend_patch+plot_annotation(tag_levels = 'A')

om3 <- (om5)+legend_patch+plot_annotation(tag_levels = 'A')

om1
om2
om3


dev.off()
while (!is.null(dev.list()))  dev.off()




#------------- pom plot against richness and abundance ------------


# GB: These boxplots do not make any sense
# GB: It does not make sense to plot together the different POM fractions and taxa richness and abundance
# GB: Remove taxa richness and abundance from the boxplots
# GB: Use the following plots
# Hardik: I make new box plot as you said


#abundance box plot from raw data

a1 <-ggplot(RPC1, aes(x=half, y=abundance, color=year)) +
     geom_point() + facet_wrap(~position) +
     stat_smooth(method = "lm")+
     theme_bw()+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("0.5 mm POM and abundance-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))


  

#
a2 <- ggplot(RPC1, aes(x=two, y=abundance, color=year)) +
      geom_point() + facet_wrap(~position) +
      stat_smooth(method = "lm")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       xlab("2 mm POM") +
      ylab("abundance") +
       ggtitle("2 mm POM and abundance-Raw data")+
  theme(plot.title = element_text(hjust = 0.5))



#
a3 <- ggplot(RPC1, aes(x=four, y=abundance, color=year)) +
      geom_point() + facet_wrap(~position) +
       stat_smooth(method = "lm")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("4 mm POM") +
  ylab(" abundance") +
  ggtitle("4 mm POM and abundance-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))



#
a4 <- ggplot(RPC1, aes(x=eight, y=abundance, color=year)) +
      geom_point() + facet_wrap(~position) +
      stat_smooth(method = "lm")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       xlab("8 mm POM") +
       ylab(" abundance") +
      ggtitle("8 mm POM and abundance-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))


#
a5<-  ggplot(RPC1, aes(x=TTL, y=abundance, color=year)) +
      geom_point() + facet_wrap(~position) +
      stat_smooth(method = "lm")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       xlab("total POM") +
       ylab(" abundance") +
       ggtitle("total POM and abundance-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))





#richness box plot from raw data 


r1 <-ggplot(RPC1, aes(x=half, y=taxa.richness, color=year)) +
  geom_point() + facet_wrap(~position) +
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("0.5 mm POM") +
  ylab(" taxa richness") +
  ggtitle("0.5 mm POM and taxa richness-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))


#
r2 <- ggplot(RPC1, aes(x=two, y=taxa.richness, color=year)) +
  geom_point() + facet_wrap(~position) +
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("2 mm POM") +
  ylab(" taxa richness") +
  ggtitle("2 mm POM and taxa richness-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))



#
r3 <- ggplot(RPC1, aes(x=four, y=taxa.richness, color=year)) +
  geom_point() + facet_wrap(~position) +
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("4 mm POM") +
  ylab(" taxa richness") +
  ggtitle("4 mm POM and taxa richness-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))




#
r4 <- ggplot(RPC1, aes(x=eight, y=taxa.richness, color=year)) +
  geom_point() + facet_wrap(~position) +
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
   xlab("8 mm POM") +
  ylab(" taxa richness") +
  ggtitle("8 mm POM  and taxa richness-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))


#
r5<-  ggplot(RPC1, aes(x=TTL, y=taxa.richness, color=year)) +
  geom_point() + facet_wrap(~position) +
  stat_smooth(method = "lm")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("total POM") +
  ylab(" taxa richness") +
  ggtitle("total POM and taxa richness-Raw data ")+
  theme(plot.title = element_text(hjust = 0.5))


#combine plot



legend_patch <- plot_layout(guides = "collect")

fig1 = (r1+a1)/(r2+a2)+legend_patch+plot_annotation(tag_levels = 'A')


fig2 = (r3+a3)/(r4+a4)+legend_patch+plot_annotation(tag_levels = 'A')


fig3 = r5+a5+legend_patch+plot_annotation(tag_levels = 'A')

fig1
fig2
fig3


dev.off()
while (!is.null(dev.list()))  dev.off()





#----- check raw data for model assumption ---------------

hist(RPC1$abundance)#skewness
shapiro.test(RPC1$abundance)#skewness
qqnorm(RPC1$abundance,main='abundance')
qqline(RPC1$abundance)# normality not found in data so not go for next test
qqnorm(RPC1$taxa.richness,main='Taxa richness')
qqline(RPC1$taxa.richness)# normality not found in data so not go for next test

dev.off()
while (!is.null(dev.list()))  dev.off()

#skewness,distribution overall view,co linearity check 

RPC.s<- RPC1[,c(7:13)]

spm(RPC.s)


dev.off()
while (!is.null(dev.list()))  dev.off()
#
pairs.panels(RPC.s,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

dev.off()
while (!is.null(dev.list()))  dev.off()
  






# GB: why did you exclude the channel? It should be used in mixed models
# hardik : did it but no meaning full out put in visualization  of random effect                    
# fit with month wise with each treatment (month+ treatment= treatment1) model fit seem to be ok

 

#-outlier removal ## use  box plot and script at last first and then after remove it
# GB: no, outliers are usually removed after model check and in general it's not a good practice to remove them
#Hardik : i agreed with 



# scale predictor: 

RPC1$half  <-  scale(RPC1$half, center = TRUE,scale = TRUE)
RPC1$two <-  scale(RPC1$two, center = TRUE,scale = TRUE)
RPC1$four <-  scale(RPC1$four, center = TRUE,scale = TRUE)
RPC1$eight <-  scale(RPC1$eight, center = TRUE,scale = TRUE)
RPC1$TTL <-   scale(RPC1$TTL , center = TRUE,scale = TRUE)



#----- MODEL FITTING--------------


# GB: If you transform abundance and richness there's no need for poisson distribution
# GB: You can use lmer instead of glmer
# GB: model

var1 <- RPC1$abundance #---------
var2 <- RPC1$taxa.richness #--------------

#
expl1 <- RPC1$half
expl2 <- RPC1$two 
expl3 <- RPC1$four
expl4 <- RPC1$eight
expl5 <- RPC1$TTL 


# log transformation
hist(var1)
hist(log(1+var1))

hist(var2)
hist(log(1+var2))


#ref paper : Nested by design:model fitting and interpretation in a mixed model era
# position column  in experiment & data frame crossed
  


#------random factor inclusion -------


var.lmea1.CH1 <- lmer(log(1+var1) ~ expl4 + year
                    +(1|CH/position) + (1|time), data = RPC1)#singular,



var.lmea1.CH2 <- lmer(log(1+var1) ~ expl1 + year
                      +(1|CH/position) + (1|time/position), data = RPC1)##singular

var.lmea1.CH3 <- lmer(log(1+var1) ~ expl2 + year
                      +(1|CH:position) + (1|time:position), data = RPC1)## treatment is change between two group of months (may, july similar) vs(june,august,october other) so random effect have no meaningful  output 





ggplot2::ggplot(RPC1, aes(x=expl1, y=var1))+
      geom_line(aes(y = predict(var.lmea1.CH3),group = CH,color = CH),size = 0.4)+
      facet_wrap(~position)+
      scale_linewidth(range = c(0, 4))+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("month wise 0.5 mm POM effect on abundance-RE")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        My_Theme


## fit WITH (treatment1 = treatment+month)month wise each treatment meaningful interpretation ,model fitting is ok
## meaningful out put in month wise as well as treatment wise per month
## Reference script available at the end of project script to see how channel wise pom effect on abundance 


var.lmea1.d <- lmer(log(1+var1) ~ expl1 + year
                    +(1|treatment1:position)+(1|time:position) , data = RPC1)


ggplot2::ggplot(RPC1, aes(x=expl1, y=var1))+
      geom_line(aes(y = predict(var.lmea1.d),group = treatment1,color = treatment1),size = 0.4)+
      facet_wrap(~position)+
      scale_linewidth(range = c(0, 4))+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("0.5 mm POM effect on abundance-treatment wise/month ")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        My_Theme

dev.off()
while (!is.null(dev.list()))  dev.off()

#--- model check
plot(var.lmea1.d,type=c("p","smooth")) # default residuals-vs.-fitted plot

plot(var.lmea1.d,sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))) # scale-location plot 
text(qqnorm(resid(var.lmea1.d))) # Q-Q plot
qqline(resid(var.lmea1.d))

# R2
MuMIn::r.squaredGLMM(var.lmea1.d)
AIC(var.lmea1.d) 


Anova(var.lmea1.d)



#-------- ALL MODEL FIT WITH   (1|treatment1:position)+(1|time:position) RANDOM FACTOR -------------------



#ABUNDANCE

var.lmea1 <- lmer(log(1+var1) ~ expl1 + year
                  +(1|treatment1:position)+(1|time:position) , data = RPC1)




#


var.lmea2 <- lmer(log(1+var1) ~ expl2 + year
                  +(1|treatment1:position) +(1|time:position), data = RPC1)



var.lmea3 <- lmer(log(1+var1) ~ expl3 + year
                  +(1|treatment1:position) +(1|time:position), data = RPC1)



var.lmea4 <- lmer(log(1+var1) ~ expl4 + year
                  +(1|treatment1:position) +(1|time:position), data = RPC1)


var.lmea5 <- lmer(log(1+var1) ~ expl5 + year
                  +(1|treatment1:position) +(1|time:position), data = RPC1)
				  


#TAXA RICHNESS

var.lmetr1 <- lmer(log(1+var2) ~ expl1 + year
                   +(1|treatment1:position) +(1|time:position), data = RPC1)

var.lmetr2 <- lmer(log(1+var2) ~ expl2 + year
                   +(1|treatment1:position) +(1|time:position), data = RPC1)

var.lmetr3 <- lmer(log(1+var2) ~ expl3 + year
                   +(1|treatment1:position) +(1|time:position), data = RPC1)

var.lmetr4 <- lmer(log(1+var2) ~ expl4 + year
                   +(1|treatment1:position) +(1|time:position), data = RPC1)

var.lmetr5 <- lmer(log(1+var2) ~ expl5 + year
                   +(1|treatment1:position) +(1|time:position), data = RPC1)



coef(var.lmetr5)


# model checks


plot(var.lmea1,type=c("p","smooth")) # default residuals-vs.-fitted plot

plot(var.lmea1,sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))) # scale-location plot 
text(qqnorm(resid(var.lmea1))) # Q-Q plot
qqline(resid(var.lmea1))






# R2
MuMIn::r.squaredGLMM(var.lmea1) # marginal R2 & conditional R2
MuMIn::r.squaredGLMM(var.lmea2)
MuMIn::r.squaredGLMM(var.lmea3)
MuMIn::r.squaredGLMM(var.lmea4)
MuMIn::r.squaredGLMM(var.lmea5)

MuMIn::r.squaredGLMM(var.lmetr1)
MuMIn::r.squaredGLMM(var.lmetr2)
MuMIn::r.squaredGLMM(var.lmetr3)
MuMIn::r.squaredGLMM(var.lmetr4)
MuMIn::r.squaredGLMM(var.lmetr5)



Anova(var.lmea1) # unbalanced design (more controls than other treatments)
Anova(var.lmea2)
Anova(var.lmea3)
Anova(var.lmea4)
Anova(var.lmea5)

#
Anova(var.lmetr1)
Anova(var.lmetr2)
Anova(var.lmetr3)
Anova(var.lmetr4)
Anova(var.lmetr5)



#AIC

AIC(var.lmea1)
AIC(var.lmea2)
AIC(var.lmea3)
AIC(var.lmea4)
AIC(var.lmea5)

AIC(var.lmetr1)
AIC(var.lmetr2)
AIC(var.lmetr3)
AIC(var.lmetr4)
AIC(var.lmetr5)





# -- FIXED EFFECT visualization with "effects" and "ggplot2" package-------------


My_Theme = theme(
  axis.title.x = element_text(size = 11),
  axis.text.x = element_text(size = 10,face = "bold"),
  axis.title.y = element_text(size = 10))


#- 0.5mm -- model abundance(halfa1) and taxa richness(halftr1)

halfa1 <- effect(c("year","expl1"),interval =TRUE,var.lmea1)


halfa1<-  ggplot(as.data.frame(halfa1),
                aes(expl1,fit,colour=year,fill=year))+
  
         geom_line()+
        ## colour=NA suppresses edges of the ribbon
         geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
        ## add rug plot based on original data
         geom_rug(data=halfa1$data,aes(y=NULL),sides="b")+
  
         # lable and title
        xlab("0.5 mm POM") +
         ylab(" abundance") +
         ggtitle("0.5mm POM effect on abudance ")+
          #
         theme_bw()+
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
         My_Theme+
  theme(plot.title = element_text(hjust = 0.5))



#
halftr1 <- effect(c("year","expl1"),interval =TRUE,var.lmetr1)

halftr1<-  ggplot(as.data.frame(halftr1),
                aes(expl1,fit,colour=year,fill=year))+
  
  geom_line()+
  
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halftr1$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("0.5 mm POM") +
  ylab(" taxa richness") +
  ggtitle("0.5mm POM effect on taxa richness ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
My_Theme+
  theme(plot.title = element_text(hjust = 0.5))






#-------------- 2mm---------------------



#- 2mm -- model abundance(halfa2) and taxa richness(halftr2)

halfa2 <- effect(c("year","expl2"),interval =TRUE,var.lmea2)


halfa2<-  ggplot(as.data.frame(halfa2),
                 aes(expl2,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halfa2$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("2 mm POM") +
  ylab(" abundance") +
  ggtitle("2 mm POM effect on abudance ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
                My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




#--------
halftr2 <- effect(c("year","expl2"),interval =TRUE,var.lmetr2)

halftr2<-  ggplot(as.data.frame(halftr2),
                  aes(expl2,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halftr2$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("2 mm POM") +
  ylab(" taxa richness") +
  ggtitle("2 mm POM effect on taxa richness ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
My_Theme+
  theme(plot.title = element_text(hjust = 0.5))







#---------------4 mm ----------------------


#- 4 mm -- model abundance(halfa3) and taxa richness(halftr3)

halfa3 <- effect(c("year","expl3"),interval =TRUE,var.lmea3)


halfa3<-  ggplot(as.data.frame(halfa3),
                 aes(expl3,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halfa3$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("4 mm POM") +
  ylab(" abundance") +
  ggtitle("4 mm POM effect on abudance ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    My_Theme+
  theme(plot.title = element_text(hjust = 0.5))



#--------
halftr3 <- effect(c("year","expl3"),interval =TRUE,var.lmetr3)

halftr3<-  ggplot(as.data.frame(halftr3),
                  aes(expl3,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halftr3$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("4 mm POM") +
  ylab(" taxa richness") +
  ggtitle("4 mm POM effect on taxa richness ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
My_Theme+
  theme(plot.title = element_text(hjust = 0.5))





#---------------8 mm ----------------------


#- 8 mm -- model abundance(halfa4) and taxa richness(halftr4)

halfa4 <- effect(c("year","expl4"),interval =TRUE,var.lmea4)


halfa4<-  ggplot(as.data.frame(halfa4),
                 aes(expl4,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halfa4$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("8 mm POM") +
  ylab(" abundance") +
  ggtitle("8 mm POM effect on abudance ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
My_Theme+
  theme(plot.title = element_text(hjust = 0.5))



#--------
halftr4 <- effect(c("year","expl4"),interval =TRUE,var.lmetr4)

halftr4<-  ggplot(as.data.frame(halftr4),
                  aes(expl4,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halftr4$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("8 mm POM") +
  ylab(" taxa richness") +
  ggtitle("8 mm POM effect on taxa richness ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
My_Theme+
  theme(plot.title = element_text(hjust = 0.5))





#---------------total POM ----------------------


#- total pom  -- model abundance(halfa5) and taxa richness(halftr5)

halfa5 <- effect(c("year","expl5"),interval =TRUE,var.lmea5)


halfa5<-  ggplot(as.data.frame(halfa5),
                 aes(expl5,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halfa5$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("total POM") +
  ylab(" abundance") +
  ggtitle("total POM effect on abudance ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




#--------

halftr5 <- effect(c("year","expl5"),interval =TRUE,var.lmetr5)

halftr5<-  ggplot(as.data.frame(halftr5),
                  aes(expl5,fit,colour=year,fill=year))+
  
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  
  ## add rug plot based on original data
  geom_rug(data=halftr5$data,aes(y=NULL),sides="b")+
  
  # lable and title
  xlab("total POM") +
  ylab(" taxa richness") +
  ggtitle("total POM effect on taxa richness ")+
  #
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




#------- combine plot with pathwork package----------------

legend_patch <- plot_layout(guides = "collect")


fixed1 <- (halfa1+halftr1)/(halfa2+halftr2)+legend_patch+plot_annotation(tag_levels = 'A')
fixed2 <- (halfa3+halftr3)/(halfa4+halftr4)+legend_patch+plot_annotation(tag_levels = 'A')
fixed3 <- (halfa5+halftr5)+legend_patch+plot_annotation(tag_levels = 'A')

fixed1
fixed2
fixed3


dev.off()
while (!is.null(dev.list()))  dev.off()







#RANDOM EFFECT -TREATMENTWISE PER MONTH FROM CHANNEL





My_Theme = theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 10,face = "bold"),
  axis.title.y = element_text(size = 10))


#--0.5mm 



ab1<- ggplot2::ggplot(RPC1, aes(x=expl1, y=var1))+
      geom_line(aes(y = predict(var.lmea1),group = treatment1,color = treatment1),size = 0.4)+
      facet_wrap(~position)+
      scale_linewidth(range = c(0, 4))+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("0.5 mm POM effect on abundance-monthwise/treatment")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr1<- ggplot2::ggplot(RPC1, aes(x=half, y=var2))+
      geom_line(aes(y =predict(var.lmetr1),group= treatment1,color =treatment1),size =0.4)+
      scale_linewidth(range = c(0.5,3))+
      facet_wrap(~position)+
      xlab("0.5 mm POM") +
      ylab(" taxa richness") +
      ggtitle("0.5 mm POM effect on taxa richness-monthwise/treatment")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))





#-2mm POM 


ab2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var1))+
  geom_line(aes(y = predict(var.lmea2),group = treatment1,color = treatment1),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("2 mm POM") +
  ylab(" abundance") +
  ggtitle("2 mm POM effect on abundance-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var2))+
  geom_line(aes(y =predict(var.lmetr2),group= treatment1,color =treatment1),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("2 mm POM") +
  ylab(" taxa richness") +
  ggtitle("2 mm POM effect on taxa richness-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))





#-4 mm POM 


ab3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var1))+
  geom_line(aes(y = predict(var.lmea3),group = treatment1,color = treatment1),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("4 mm POM") +
  ylab(" abundance") +
  ggtitle("4 mm POM effect on abundance-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var2))+
  geom_line(aes(y =predict(var.lmetr3),group= treatment1,color =treatment1),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("4 mm POM") +
  ylab(" taxa richness") +
  ggtitle("4 mm POM effect on taxa richness-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




#-8 mm POM 


ab4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var1))+
  geom_line(aes(y = predict(var.lmea4),group = treatment1,color = treatment1),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("8 mm POM") +
  ylab(" abundance") +
  ggtitle("8 mm POM effect on abundance-monthwise/treatment  ")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var2))+
  geom_line(aes(y =predict(var.lmetr4),group= treatment1,color =treatment1),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("8 mm POM") +
  ylab(" taxa richness") +
  ggtitle("8 mm POM effect on taxa richness-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




#--total POM 


ab5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var1))+
  geom_line(aes(y = predict(var.lmea5),group = treatment1,color = treatment1),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("total POM") +
  ylab(" abundance") +
  ggtitle("total POM effect on abundance-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var2))+
  geom_line(aes(y =predict(var.lmetr5),group= treatment1,color =treatment1),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("total POM") +
  ylab(" taxa richness") +
  ggtitle("total POM effect on taxa richness-monthwise/treatment")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))






legend_patch <- plot_layout(guides = "collect") 

random1 <- (ab1+tr1)/(ab2+tr2)+legend_patch+plot_annotation(tag_levels = 'A')
random2 <- (ab3+tr3)/(ab4+tr4)+legend_patch+plot_annotation(tag_levels = 'A')
random3 <- (ab5+tr5)+legend_patch+plot_annotation(tag_levels = 'A')

random1
random2
random3


dev.off()
while (!is.null(dev.list()))  dev.off()





#--RANDOM EFFECT MONTHWISE VISULIZATION


#-0.5mm POM 

ab1<- ggplot2::ggplot(RPC1, aes(x=expl1, y=var1))+
      geom_line(aes(y = predict(var.lmea1),group = time,color = time),size = 0.4)+
      facet_wrap(~position)+
      scale_linewidth(range = c(0, 4))+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("0.5 mm POM effect on abundance-month wise ")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr1<- ggplot2::ggplot(RPC1, aes(x=half, y=var2))+
      geom_line(aes(y =predict(var.lmetr1),group= time,color = time),size =0.4)+
      scale_linewidth(range = c(0.5,3))+
      facet_wrap(~position)+
      xlab("0.5 mm POM") +
      ylab(" taxa richness") +
      ggtitle("0.5 mm POM effect on taxa richness- month wise")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))





#-2mm POM 

ab2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var1))+
  geom_line(aes(y = predict(var.lmea2),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("2 mm POM") +
  ylab(" abundance") +
  ggtitle("2 mm POM effect on abundance-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var2))+
  geom_line(aes(y =predict(var.lmetr2),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("2 mm POM") +
  ylab(" taxa richness") +
  ggtitle("2 mm POM effect on taxa richness-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))





#-4 mm POM 


ab3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var1))+
  geom_line(aes(y = predict(var.lmea3),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("4 mm POM") +
  ylab(" abundance") +
  ggtitle("4 mm POM effect on abundance -month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var2))+
  geom_line(aes(y =predict(var.lmetr3),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("4 mm POM") +
  ylab(" taxa richness") +
  ggtitle("4 mm POM effect on taxa richness-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




#-8 mm POM 


ab4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var1))+
  geom_line(aes(y = predict(var.lmea4),group = time ,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("8 mm POM") +
  ylab(" abundance") +
  ggtitle("8 mm POM effect on abundance-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var2))+
  geom_line(aes(y =predict(var.lmetr4),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("8 mm POM") +
  ylab(" taxa richness") +
  ggtitle("8 mm POM effect on taxa richness-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




#-total POM 


ab5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var1))+
  geom_line(aes(y = predict(var.lmea5),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("total POM") +
  ylab(" abundance") +
  ggtitle("total POM effect on abundance-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))




tr5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var2))+
  geom_line(aes(y =predict(var.lmetr5),group= time,color = time ),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("total POM") +
  ylab(" taxa richness") +
  ggtitle("total POM effect on taxa richness-month wise")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+scale_x_discrete(expand = expansion(add = .7))+
  theme(plot.title = element_text(hjust = 0.5))






legend_patch <- plot_layout(guides = "collect")


random1 <- (ab1+tr1)/(ab2+tr2)+legend_patch+plot_annotation(tag_levels = 'A')
random2 <- (ab3+tr3)/(ab4+tr4)+legend_patch+plot_annotation(tag_levels = 'A')
random3 <- (ab5+tr5)+legend_patch+plot_annotation(tag_levels = 'A')

random1
random2
random3

dev.off()
while (!is.null(dev.list()))  dev.off()



#-----------------END OF PROJECT SCRIPT----------------------------
#-----------------------end----------------------------------------
#------------------------end----------------------------------------




# scipt bellow just for reference purpose only
# model use with CH in random structure
#BELLOW SCRIPT IS ONLY FOR REFERANCE PURPOSE TO SEE HOW RANDOM EFFECT  EXIST IN PRESENT DATA SET IF WE FIT WITH CHANNEL




var.lmea1 <- lmer(log(1+var1) ~ expl1 + year
                  +(1|CH:position)+(1|time:position) , data = RPC1)




var.lmea2 <- lmer(log(1+var1) ~ expl2 + year
                  +(1|CH:position) +(1|time:position), data = RPC1)



var.lmea3 <- lmer(log(1+var1) ~ expl3 + year
                  +(1|CH:position) +(1|time:position), data = RPC1)



var.lmea4 <- lmer(log(1+var1) ~ expl4 + year
                  +(1|CH:position) +(1|time:position), data = RPC1)

var.lmea5 <- lmer(log(1+var1) ~ expl5 + year
                  +(1|CH:position) +(1|time:position), data = RPC1)
				  


#---lmer taxa.richness ,tr1 to tr5 for taxa richness-----

var.lmetr1 <- lmer(log(1+var2) ~ expl1 + year
                   +(1|CH:position) +(1|time:position), data = RPC1)

var.lmetr2 <- lmer(log(1+var2) ~ expl2 + year
                   +(1|CH:position) +(1|time:position), data = RPC1)

var.lmetr3 <- lmer(log(1+var2) ~ expl3 + year
                   +(1|CH:position) +(1|time:position), data = RPC1)

var.lmetr4 <- lmer(log(1+var2) ~ expl4 + year
                   +(1|CH:position) +(1|time:position), data = RPC1)

var.lmetr5 <- lmer(log(1+var2) ~ expl5 + year
                   +(1|CH:position) +(1|time:position), data = RPC1)


# model checks
plot(var.lmea1,type=c("p","smooth")) # default residuals-vs.-fitted plot

plot(var.lmea1,sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))) # scale-location plot 
text(qqnorm(resid(var.lmea1))) # Q-Q plot
qqline(resid(var.lmea1))

dev.off()
#
 


# R2
MuMIn::r.squaredGLMM(var.lmea1) # marginal R2 & conditional R2
MuMIn::r.squaredGLMM(var.lmea2)
MuMIn::r.squaredGLMM(var.lmea3)
MuMIn::r.squaredGLMM(var.lmea4)
MuMIn::r.squaredGLMM(var.lmea5)

MuMIn::r.squaredGLMM(var.lmetr1)
MuMIn::r.squaredGLMM(var.lmetr2)
MuMIn::r.squaredGLMM(var.lmetr3)
MuMIn::r.squaredGLMM(var.lmetr4)
MuMIn::r.squaredGLMM(var.lmetr5)



Anova(var.lmea1) # unbalanced design (more controls than other treatments)
Anova(var.lmea2)
Anova(var.lmea3)
Anova(var.lmea4)
Anova(var.lmea5)

#
Anova(var.lmetr1)
Anova(var.lmetr2)
Anova(var.lmetr3)
Anova(var.lmetr4)
Anova(var.lmetr5)

##AIC
AIC(var.lmea1)
AIC(var.lmea2)
AIC(var.lmea3)
AIC(var.lmea4)
AIC(var.lmea5)

AIC(var.lmetr1)
AIC(var.lmetr2)
AIC(var.lmetr3)
AIC(var.lmetr4)
AIC(var.lmetr5)



#----effect visulization at channel or month level
#--replace time with CH to see effect at channel level


My_Theme = theme(
  axis.title.x = element_text(size = 9),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 8))




#--0.5mm-----------


ab1<- ggplot2::ggplot(RPC1, aes(x=expl1, y=var1))+
      geom_line(aes(y = predict(var.lmea1),group = time,color = time),size = 0.4)+
      facet_wrap(~position)+
      scale_linewidth(range = c(0, 4))+
      xlab("0.5 mm POM") +
      ylab(" abundance") +
       ggtitle("month wise 0.5 mm POM effect on abundance-RE")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        My_Theme+
  theme(plot.title = element_text(hjust = 0.5))





tr1<- ggplot2::ggplot(RPC1, aes(x=half, y=var2))+
      geom_line(aes(y =predict(var.lmetr1),group= time,color = time),size =0.4)+
      scale_linewidth(range = c(0.5,3))+
      facet_wrap(~position)+
      xlab("0.5 mm POM") +
      ylab(" taxa richness") +
      ggtitle("month wise 0.5 mm POM effect on taxa richness-RE")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      My_Theme+
  theme(plot.title = element_text(hjust = 0.5))





#----------------2mm POM ---------------


ab2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var1))+
  geom_line(aes(y = predict(var.lmea2),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("2 mm POM") +
  ylab(" abundance") +
  ggtitle("month wise 2 mm POM effect on abundance-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




tr2<- ggplot2::ggplot(RPC1, aes(x=expl2, y=var2))+
  geom_line(aes(y =predict(var.lmetr2),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("2 mm POM") +
  ylab(" taxa richness") +
  ggtitle("month wise 2 mm POM effect on taxa richness-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))





#----------------4 mm POM ---------------


ab3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var1))+
  geom_line(aes(y = predict(var.lmea3),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("4 mm POM") +
  ylab(" abundance") +
  ggtitle("month wise 4 mm POM effect on abundance-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




tr3<- ggplot2::ggplot(RPC1, aes(x=expl3, y=var2))+
  geom_line(aes(y =predict(var.lmetr3),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("4 mm POM") +
  ylab(" taxa richness") +
  ggtitle("month wise 4 mm POM effect on taxa richness-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




#----------------8 mm POM ---------------


ab4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var1))+
  geom_line(aes(y = predict(var.lmea4),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("8 mm POM") +
  ylab(" abundance") +
  ggtitle("month wise 8mm POM effect on abundance-RE  ")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




tr4<- ggplot2::ggplot(RPC1, aes(x=expl4, y=var2))+
  geom_line(aes(y =predict(var.lmetr4),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("8 mm POM") +
  ylab(" taxa richness") +
  ggtitle("month wise 8 mm POM effect on taxa richness-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




#----------------total POM ---------------


ab5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var1))+
  geom_line(aes(y = predict(var.lmea5),group = time,color = time),size = 0.4)+
  facet_wrap(~position)+
  scale_linewidth(range = c(0, 4))+
  xlab("total POM") +
  ylab(" abundance") +
  ggtitle("month wise total POM effect on abundance-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))




tr5<- ggplot2::ggplot(RPC1, aes(x=expl5, y=var2))+
  geom_line(aes(y =predict(var.lmetr5),group= time,color = time),size =0.4)+
  scale_linewidth(range = c(0.5,3))+
  facet_wrap(~position)+
  xlab("total POM") +
  ylab(" taxa richness") +
  ggtitle("month wise total POM effect on taxa richness-RE")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  My_Theme+
  theme(plot.title = element_text(hjust = 0.5))






legend_patch <- plot_layout(guides = "collect")


random1 <- (ab1+tr1)/(ab2+tr2)+legend_patch+plot_annotation(tag_levels = 'A')
random2 <- (ab3+tr3)/(ab4+tr4)+legend_patch+plot_annotation(tag_levels = 'A')
random3 <- (ab5+tr5)+legend_patch+plot_annotation(tag_levels = 'A')



random1
random2
random3







sessionInfo()

citation()







