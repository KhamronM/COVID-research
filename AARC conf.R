####
library(tidyverse)

# Used for skewness and kurtosis
library(e1071)

# Used for adding info about diagnostics to a model
library(broom)
library(MASS) # For the studres function

# Used for VIF
library(car)

# Used for partial correlation
library(ppcor)


library("gmodels")
library("car")
library("DescTools")
library("ggplot2")
library("qqplotr")
library("dplyr")
library(stats)
library(readxl)
library(tableone)
library(car)

aarcvent.df<- read_excel("Desktop/AARC Winter/survived vent data.xlsx")

aarcvent.df <- subset(aarcvent.df, select = -c(MRN, ETHNICITY,RACE, day1_dt, day5_dt))

aarcvent.df$Survived <- as.factor(aarcvent.df$Survived)

levels(aarcvent.df$medic) <- c("no medicine", "dexamethasone", "remdesivir", "both")
levels(aarcvent.df$Survived) <- c("did not survive", "survived")


aarcventDesc <- CreateTableOne(data = aarcvent.df)
summary(aarcventDesc)


SurvivedYes <- subset(aarcvent.df, Survived == "survived")
SurvivedNo <- subset(aarcvent.df, Survived == "did not survive")

SurvivedYesDesc <- CreateTableOne(data = SurvivedYes)
summary(SurvivedYesDesc)

SurvivedNoDesc <- CreateTableOne(data = SurvivedNo)
summary(SurvivedNoDesc)

aarcvent.df$AGE <- cut(aarcvent.df$AGE, breaks = c(-Inf, 45, 60,75, Inf),
                       labels = c("<=45","46-60","61-75",">=76"))

aarcventDesc <- CreateTableOne(data = aarcvent.df)
summary(aarcventDesc)


hist(aarcvent.df$dc_d1)
hist(aarcvent.df$dc_d5)
hist(aarcvent.df$bg_d1)
hist(aarcvent.df$bg_d5)
hist(aarcvent.df$ar_d1)
hist(aarcvent.df$ar_d5)
hist(aarcvent.df$sc_d1)
hist(aarcvent.df$sc_d5)


#shapiro
aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(dc_d1)$statistic,
            p.value = shapiro.test(dc_d1)$p.value)

aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(dc_d5)$statistic,
            p.value = shapiro.test(dc_d5)$p.value)

aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(bg_d1)$statistic,
            p.value = shapiro.test(bg_d1)$p.value)

aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(bg_d5)$statistic,
            p.value = shapiro.test(bg_d5)$p.value)

aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(ar_d1)$statistic,
            p.value = shapiro.test(ar_d1)$p.value)

aarcvent.df %>%
  group_by(Survived) %>%
  summarise(`W Stat` = shapiro.test(ar_d5)$statistic,
            p.value = shapiro.test(ar_d5)$p.value)


dcd1w <-wilcox.test(dc_d1 ~ Survived, data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(dcd1w)

dcd5w<-wilcox.test(dc_d5 ~ Survived, data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(dcd5w)

bgd1w<-wilcox.test(bg_d1 ~ Survived , data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(bgd1w)

bgd5w<-wilcox.test(bg_d5 ~ Survived, data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(bgd5w)

ard1w<-wilcox.test(ar_d1 ~ Survived, data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(ard1w)

ard5w<-wilcox.test(ar_d5 ~ Survived, data=aarcvent.df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(ard5w)


AGEchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$AGE, correct=FALSE)
AGEchi

genderchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$GENDER, correct=FALSE)
genderchi

diabchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$DIABETES, correct=FALSE)
diabchi

hyperchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$HYPERTENSION, correct=FALSE)
hyperchi

asthmachi <- chisq.test(aarcvent.df$Survived, aarcvent.df$ASTHMA, correct=FALSE)
asthmachi

fisher.test(aarcvent.df$ASTHMA,aarcvent.df$Survived)

STROKEchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$STROKE, correct=FALSE)
STROKEchi

LIVER_DISEASEchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$LIVER_DISEASE, correct=FALSE)
LIVER_DISEASEchi

CADchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$CAD, correct=FALSE)
CADchi

CHFchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$CHF, correct=FALSE)
CHFchi

COPDchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$COPD, correct=TRUE)
COPDchi

fisher.test(aarcvent.df$COPD,aarcvent.df$Survived)

OBESITYchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$OBESITY, correct=FALSE)
OBESITYchi

ESRDchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$ESRD, correct=FALSE)
ESRDchi

CKDchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$CKD, correct=FALSE)
CKDchi

CANCERchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$CANCER, correct=FALSE)
CANCERchi

medicchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$medic, correct=FALSE)
medicchi

comor_grpchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$comor_grp, correct=FALSE)
comor_grpchi

c_ar_d1chi <- chisq.test(aarcvent.df$Survived, aarcvent.df$c_ar_d1, correct=FALSE)
c_ar_d1chi

c_ar_d5chi <- chisq.test(aarcvent.df$Survived, aarcvent.df$c_ar_d5, correct=FALSE)
c_ar_d5chi

ARDS_d1chi <- chisq.test(aarcvent.df$Survived, aarcvent.df$ARDS_d1, correct=FALSE)
ARDS_d1chi

ARDS_d5chi <- chisq.test(aarcvent.df$Survived, aarcvent.df$ARDS_d5, correct=FALSE)
ARDS_d5chi

race_ethchi <- chisq.test(aarcvent.df$Survived, aarcvent.df$race_eth, correct=FALSE)
race_ethchi

library("geepack")

GEEmodel<- geeglm(factor(Survived)~dynamic_compliance+ARDS+  blood_gas+airway_res +
                 medic_gm+GENDER +AGE+race_eth+comor_grp+
                 static_compliance, id=MRN,
               data=aarcvent.df, subset=NULL, na.action=na.omit,
               family = binomial(link = "logit"), corstr = "independence")
               
  

GEEmodel<- geeglm(factor(Survived)~dynamic_compliance+ARDS+  blood_gas+airway_res +
          medic_gm+GENDER +AGE+race_eth+comor_grp+
          static_compliance, family = binomial(link = "logit"),
        data = aarcvent.df,
        na.action=na.omit)

GEEmodel<-geeglm(factor(Survived) ~ dynamic_compliance+ARDS+ blood_gas+
                   airway_res+
                   medic_gm+GENDER +AGE+race_eth+comor_grp+
                   static_compliance, data=aarcvent.df, id=MRN, family=binomial(link = "logit"),
   corstr="ar1")
        anova(gee2)

FINALmodel.glm<- glm(factor(Survived)~dc_d1+dc_d5+ARDS_d1 +  
                       ARDS_d5 +bg_d1+bg_d5+
                       medic+GENDER +AGE+race_eth+comor_grp,
                     data=aarcvent.df, family = binomial(link = "logit"))



m <- glmer(factor(Survived) ~ dynamic_compliance+ARDS+ blood_gas+
             airway_res+
             medic_gm+GENDER +AGE+race_eth+comor_grp+
             static_compliance +
             (1 | MRN), data = aarcvent.df, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)

summary(m)

GEEmodel<-gee(f1,id=MRN,family=binomial(),
              data=aarcvent.df,corstr="independence")

summary(GEEmodel)

exp(1.41)

counts <- table(aarcvent.df$Survived,aarcvent.df$AGE)
barplot(counts, main="Survival by Age", ylab = "# of patient (n)",
        xlab="Age Group", col=c("BLACK","WHITE"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,150))

counts <- table(aarcvent.df$Mortality,aarcvent.df$race_eth)
barplot(counts, main="Survival by Race", ylab = "# of patient (n)",
        xlab="", col=c("yellow","purple"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,150))

counts <- table(aarcvent.df$Survived,aarcvent.df$GENDER)
barplot(counts, main="Survival by Gender", ylab = "# of patient (n)",
        xlab="", col=c("darkblue","red"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,250))

counts <- table(aarcvent.df$Survived,aarcvent.df$ARDS_d1)
barplot(counts, main="Survival by Oxygenation impaitment", ylab = "# of patient (n)",
        xlab="", col=c("darkblue","red"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,200))

levels(aarcvent.df$ARDS_d5) <- c("PaO2/FiO2 > 300", "Mild", "Moderate", "Severe")

counts <- table(aarcvent.df$Mortality,aarcvent.df$ARDS_d5)
barplot(counts, main="Mortality by Oxygen Impairment", ylab = "# of patient (n)",
        xlab="", col=c("darkblue","red"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,250))

counts <- table(aarcvent.df$Survived,aarcvent.df$c_ar_d1)
barplot(counts, main="Survival by ARDS", ylab = "# of patient (n)",
        xlab="", col=c("darkblue","red"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,250))

counts <- table(aarcvent.df$Survived,aarcvent.df$c_ar_d5)
barplot(counts, main="Survival by ARDS", ylab = "# of patient (n)",
        xlab="", col=c("darkblue","red"),
        beside=TRUE, legend = rownames(counts), ylim = c(0,250))
























