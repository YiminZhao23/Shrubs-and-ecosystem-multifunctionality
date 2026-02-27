library(openxlsx);library(nlme);library(tidyverse);library(MuMIn)
library(ggpubr);library(lme4);library(lmerTest);library(piecewiseSEM)


####Load the complete data####
Community_data <- read.xlsx("Data_for_article.xlsx",sheet = 1)

####//////GROUP1  Natural grassland, shrub encroachment, artificial shrub treatments were analyzed as a single group####
Community_G3 <- Community_data%>%
  filter(Treatment!="Shrub removal")%>%
  mutate(Treatment=factor(Treatment,levels=
                            c("Natural grassland",
                              "Shrub encroachment",
                              "Artificial shrub"
                            )))

####//Figure1A  Ecosystem Multifuntionality####
summary(lmer(EcosystemMultifuntionality~Treatment+(1|Block),
             data=Community_G3)) 

summary(lmer(EcosystemMultifuntionality~ArtificialShrub+ShrubEncroachment
             +(1|Block), data=Community_G3)) 
   
########Figure1A
Multifuntionality_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=EcosystemMultifuntionality,
                          color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1),width = 0.2,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),
        legend.position = "none")+
  labs(x="",y = 'Multifuntionality')+
  scale_colour_brewer(palette="Dark2")+
  ylim(0.1,1.05)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub"))

####//Figure2  biotic and abiotic environments factors####
#### abiotic environments factors

####  (A) light penetration
summary(lmer(LightPenetration~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

LightPenetration_G3 <-  
  ggplot(Community_G3,aes(x=Treatment,y=LightPenetration,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Light penetration')+
  scale_colour_brewer(palette="Dark2")+
  ylim(0,0.4)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub")) 

#### (B) temperature
summary(lmer(Temperature ~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

Temperature_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=Temperature,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar',
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Temperature(℃)')+
  scale_colour_brewer(palette="Dark2")+
  ylim(11,24.5)+
  scale_x_discrete(limits = c("Natural grassland",
                              "Shrub encroachment", 
                              "Artificial shrub"))


#### (C) humidity
summary(lmer(Humidity~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

Humidity_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=Humidity,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+

    theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Moisture(%)')+
  scale_colour_brewer(palette="Dark2")+
  ylim(79,107)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub")) 

####(F) soil water content
summary(lmer(SoilWaterContent~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

SoilWater_G3 <-   
  ggplot(Community_G3,aes(x=Treatment,y=SoilWaterContent,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+

    theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Soil water content(%)')+
  scale_colour_brewer(palette="Dark2")+
  ylim(30,172)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment",
                              "Artificial shrub")) 

####(G) soil pH
summary(lmer(SoilpH~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

SoilpH_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=SoilpH,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+
    theme(axis.text.x = element_text(angle=45,hjust =T),
          legend.position = "none")+
  labs(x="",y = 'Soil pH')+
  scale_colour_brewer(palette="Dark2")+
  ylim(5.9,7.33)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment",
                              "Artificial shrub"))

######## biotic environments factors

####  (D) species richness
summary(lmer(SpeciesRichness ~ArtificialShrub+ShrubEncroachment 
             +(1|Block),data=Community_G3)) 

SR_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=SpeciesRichness ,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,#
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Species richness')+
  scale_colour_brewer(palette="Dark2")+
  ylim(11,38)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub")) 

#### (E) evenness
summary(lmer(Evenness~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

Evenness_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=Evenness,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar',
               position = position_dodge(width = 1),  
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Evenness')+
  scale_colour_brewer(palette="Dark2")+
  ylim(0.35,0.965)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub")) 

####(H) AMF 
summary(lmer(AMF ~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

AMF_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=AMF,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar',
               position = position_dodge(width = 1),  
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),
        legend.position = "none")+
  labs(x="",y = 'AMF(%)')+
  scale_colour_brewer(palette="Dark2")+
  ylim(1.5,11)+
  scale_x_discrete(limits = c("Natural grassland", 
                              "Shrub encroachment", 
                              "Artificial shrub")) 

#### (I) soil multidiversity.
summary(lmer(SoilMultidiversity ~ArtificialShrub+ShrubEncroachment
             +(1|Block),data=Community_G3)) 

soilDiversity_G3 <- 
  ggplot(Community_G3,aes(x=Treatment,y=SoilMultidiversity,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,#
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.2,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),
        legend.position = "none")+
  labs(x="",y = 'Soil multidiversity')+
  scale_colour_brewer(palette="Dark2")+
  ylim(0.18,1.01)+
  scale_x_discrete(limits = c("Natural grassland",
                              "Shrub encroachment",
                              "Artificial shrub")) 


####Model selection of Ecosystem Multifuntionality####
###Full model
model1 <- lmer(EcosystemMultifuntionality~ArtificialShrub+ShrubEncroachment+
                 Temperature +LightPenetration+Humidity+SpeciesRichness +
                 SoilWaterContent+SoilpH+SoilMultidiversity+Evenness+AMF+(1|Block),
               data = Community_G3) 

summary(model1)
car::vif(model1)


####Due to the multicollinearity, shrub encroachment and light penetration are excluded. 
model2 <- lmer(EcosystemMultifuntionality~ArtificialShrub+Temperature+
                 Humidity+SpeciesRichness +Evenness+SoilWaterContent+
                 SoilpH+SoilMultidiversity+AMF+(1|Block),
               data = Community_G3) 

model2 <- arm::standardize(model2,standardize.y = TRUE)

####Model selection
options(na.action = na.fail)
selection_model3 <- dredge(model3,evaluate = T,rank = "AICc",
                           REML=F,
                           extra = c("R^2", F = function(x)
                            summary(x)$fstatistic[[1]])) 
summary(selection_model3)

subset_selection_model3 <- subset(selection_model3,delta<2)

avemodel_slection_model3 <- model.avg(subset_selection_model3)
summary(avemodel_slection_model3)

####//Figure4A SEM####
SEM1<- psem(
  lme(SpeciesRichness ~ArtificialShrub+ShrubEncroachment,
      random = ~1|Block,Community_G3),
  lme(SoilWaterContent~ArtificialShrub+ShrubEncroachment,
      random = ~1|Block,Community_G3),          
  lme(SoilMultidiversity~ArtificialShrub+ShrubEncroachment,
      random = ~1|Block,Community_G3),
  lme(Evenness~ArtificialShrub+ShrubEncroachment,
      random = ~1|Block,Community_G3),
  lme(SoilpH~ArtificialShrub+ShrubEncroachment,
      random = ~1|Block,Community_G3),
  lme(EcosystemMultifuntionality~SoilMultidiversity+SoilWaterContent
      +Evenness+SpeciesRichness +SoilpH,random = ~1|Block,Community_G3))
summary(SEM1)


#### //////GROUP2  Shrub encroachment, shrub removal, and natural grassland treatments were analyzed as a single group####
Community_R3 <- Community_data%>%
  filter(Treatment!="Artificial shrub")

####//Figure1B  Ecosystem Multifuntionality####
summary(lmer(EcosystemMultifuntionality~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(EcosystemMultifuntionality~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 

########Figure1B
Multifuntionality_R3 <- 
  ggplot(Community_R3,aes(x=Treatment,y=EcosystemMultifuntionality,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1),  
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),legend.position = "none")+
  labs(x="",y = 'Multifuntionality')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland")) 


####//Figure3  biotic and abiotic environments factors####
#### abiotic environments factors

####  (A) light penetration
summary(lmer(LightPenetration~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(LightPenetration~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 
####Figure3A
LightPenetration_R3 <- 
  ggplot(Community_R3,aes(x=Treatment,y=LightPenetration,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1),  
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Light penetration')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(0,0.35)+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland")) 

#### (B) temperature
summary(lmer(Temperature~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(Temperature~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 
####Figure3B
Temperature_R3 <- 
ggplot(Community_R3,aes(x=Treatment,y=Temperature,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar',
               position = position_dodge(width = 1),  
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Temperature(℃)')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(10,25)+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland")) 


#### (C) humidity
summary(lmer(Humidity~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(Humidity~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 
####Figure3C
Humidity_R3 <-   
  ggplot(Community_R3,aes(x=Treatment,y=Humidity,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar',
               position = position_dodge(width = 1), 
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Moisture(%)')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(74,107)+
  scale_x_discrete(limits = c("Shrub encroachment",
                              "Shrub removal",
                              "Natural grassland"))  


####(F) soil water content
summary(lmer(SoilWaterContent~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(SoilWaterContent~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 
####Figure3F
SoilWater_R3 <- 
  ggplot(Community_R3,aes(x=Treatment,y=SoilWaterContent,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.15,size=1.2)+
  
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Soil water content(%)')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(53,155)+
  scale_x_discrete(limits = c("Shrub encroachment",
                              "Shrub removal",
                              "Natural grassland"))


####(G) soil pH
summary(lmer(SoilpH~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(SoilpH~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 

####Figure3G
SoilpH_R3 <-   
  ggplot(Community_R3,aes(x=Treatment,y=SoilpH,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),legend.position = "none")+
  labs(x="",y = 'Soil pH')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(5.9,7.3)+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland"))



######## biotic environments factors

####  (D) species richness
summary(lmer(SpeciesRichness~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(SpeciesRichness~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 

####Figure3D
SR_R3 <-    
  ggplot(Community_R3,aes(x=Treatment,y=SpeciesRichness ,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se,
               geom = 'errorbar', 
               position = position_dodge(width = 1),
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Species richness')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(10,36)+
  scale_x_discrete(limits = c("Shrub encroachment",
                              "Shrub removal",
                              "Natural grassland"))

#### (E) evenness
summary(lmer(Evenness~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(Evenness~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 

####Figure3E
Evenness_R3 <-   
  ggplot(Community_R3,aes(x=Treatment,y=Evenness,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_blank(),legend.position = "none")+
  labs(x="",y = 'Evenness')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(0.3,0.93)+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland")) 


####(H) AMF
summary(lmer(AMF~ShrubRemoval+(1|Block),
              data=Community_R3%>%
                 filter(Treatment%in%c("Shrub encroachment",
                                       "Shrub removal")))) 
  
summary(lmer(AMF~Treatment
               +(1|Block), data=Community_R3%>%
                 filter(Treatment%in%c("Natural grassland",
                                       "Shrub removal"))%>%
                 mutate(Treatment=factor(Treatment,levels=
                                           c("Shrub removal",
                                             "Natural grassland"
                                           ))))) 
####Figure3H
AMF_R3 <-  
  ggplot(Community_R3,aes(x=Treatment,y=AMF,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1), 
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),legend.position = "none")+
  labs(x="",y = 'AMF(%)')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(1.5,14)+
  scale_x_discrete(limits = c("Shrub encroachment",
                              "Shrub removal",
                              "Natural grassland")) 


#### (I) soil multidiversity.
summary(lmer(SoilMultidiversity~ShrubRemoval+(1|Block),
             data=Community_R3%>%
               filter(Treatment%in%c("Shrub encroachment",
                                     "Shrub removal")))) 

summary(lmer(SoilMultidiversity~Treatment
             +(1|Block), data=Community_R3%>%
               filter(Treatment%in%c("Natural grassland",
                                     "Shrub removal"))%>%
               mutate(Treatment=factor(Treatment,levels=
                                         c("Shrub removal",
                                           "Natural grassland"
                                         ))))) 

####Figure3I
soilDiversity_R3 <-  
  ggplot(Community_R3,aes(x=Treatment,y=SoilMultidiversity,color=Treatment)) +
  geom_point(size=3.5, shape=19)+
  stat_summary(fun= mean, geom = 'point', shape = 19, 
               size = 6,  position = position_dodge(width = 1) )+
  stat_summary(fun.data = mean_se, 
               geom = 'errorbar', 
               position = position_dodge(width = 1),
               width = 0.15,size=1.2)+
  theme(axis.text.x = element_text(angle=45,hjust =T),legend.position = "none")+
  labs(x="",y = 'Soil multidiversity')+
  scale_color_manual(values=c("#D95F02","#E6AB02","#1B9E77"))+
  ylim(0.26,1)+
  scale_x_discrete(limits = c("Shrub encroachment", 
                              "Shrub removal",
                              "Natural grassland")) 


####//Model selection of Ecosystem Multifuntionality####
###Full model
model3 <- lmer(EcosystemMultifuntionality~ShrubRemoval+
                 Temperature +LightPenetration+Humidity+SpeciesRichness +
                 SoilWaterContent+SoilpH+SoilMultidiversity+Evenness+AMF+(1|Block),
                 data = Community_R3) 

summary(model3)
car::vif(model3)


####Shrub removal and light penetration are excluded.
model4 <- lmer(EcosystemMultifuntionality~Temperature+
                 Humidity+SpeciesRichness +Evenness+SoilWaterContent+
                 SoilpH+SoilMultidiversity+AMF+(1|Block),
               data = Community_R2) 

model4 <- arm::standardize(model4,standardize.y = TRUE)

####Model selection
options(na.action = na.fail)
selection_model4<- dredge(model4,evaluate = T,rank = "AICc",
                           REML=F,
                           extra = c("R^2", F = function(x)
                             summary(x)$fstatistic[[1]])) 
summary(selection_model4)

subset_selection_model4 <- subset(selection_model4,delta<2)

avemodel_slection_model4 <- model.avg(subset_selection_model4)
summary(avemodel_slection_model4)

####//Figure4B SEM####
Community_R2 <- Community_data%>%
  filter(Treatment%in%c("Shrub encroachment", 
                        "Shrub removal"))
SEM2<- psem(
  lme(SoilWaterContent~ShrubRemoval,random = ~1|Block,Community_R2),
  lme(log(Evenness)~ShrubRemoval,random = ~1|Block,Community_R2),
  lme(SpeciesRichness ~ShrubRemoval,random = ~1|Block,Community_R2),
  lme(SoilpH~ShrubRemoval,random = ~1|Block,Community_R2),
  lme(EcosystemMultifuntionality~SoilWaterContent+log(Evenness)+
        SpeciesRichness +SoilpH,random = ~1|Block,Community_R2))
summary(SEM2)
