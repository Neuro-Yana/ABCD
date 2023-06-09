##Set the working directory to the deap data folder through the session tab


R.version

library("gamm4")
library("ggplot2")
library("matching")
library("mgcv") - #don't have it in my packages?! how?'
library("tidyverse")

#reading data
data <- read.csv("3_fullest.csv")



#renaming the columns
colnames(data)
names(data)[names(data) == 'tfmri_sst_all_beh_total_mean.rt'] <- 'ssrt'
names(data)[names(data) == 'anthro_bmi_calc'] <- 'bmi'

##dropping nas from specific columns (ssrt and bmi)
data <- data[!is.na(data$ssrt),]
data <- data[!is.na(data$bmi),]
summary(data$bmi)


#choosing baseline

summary(as.factor(data$event_name)) #as factor bcs these variables are categorical
data_base <- subset(data, data$event_name == "baseline_year_1_arm_1")
summary(as.factor(data_base$event_name))


#choosing follow-up
year2 <- subset(data, data$event_name == "2_year_follow_up_y_arm_1")
summary(as.factor(year2$event_name))

##filter out underweights
year2 <- year2 %>% 
  filter(bmi > 18.5)
summary(year2$bmi)


#add new column of bmi categories
year2$bmigroup <- cut(year2$bmi,
                          breaks=c(18.5, 25, 30, 36),
                          labels=c('Recommended', 'Overweight', 'Obese'))
summary(year2$bmigroup)

#data cleaned
#we have 2-nd year follow-up data in year2 
#with BMI groups and filtered underweights 
------------------------------------------------------------------------
#plotting
ggplot(year2, aes(x=bmi, y=ssrt, color = bmigroup)) +
geom_point(shape=1)  



#first, we need to tell r that bmi groups are categorical variables
#for some modeling/plotting reasons further on which I donremember

#to do this, I assign the bmigroups to a variable that stores data as factor
factorbmigroup <- factor(year2$bmigroup)

#MORE DATA PREPARATION BCS MODEL/PLOTTING KEEPS SAYING THERE'S TOO MUCH NANs 
#TO DO SMTH MEANINIGFUL

#mean centering age and ssrt
year2$mcage <- scale(year2$age, scale = FALSE)
year2$mcssrt <- scale(year2$ssrt, scale = FALSE)

summary(is.na(year2$mcssrt)) #- no NAs in either MC age or ssrt

#drop all NAs for running the analysis
##dropping nas from specific columns (ssrt and bmi)

Y2_dna <- year2
Y2_dna <- data[!is.na(Y2_dna$sex_at_birth),]
Y2_dna <- data[!is.na(Y2_dna$household.income),]
Y2_dna <- data[!is.na(Y2_dna$race_ethnicity),]

#for some reason, there are no MC, bmigroup columns after dropping NAs
#so I am adding them back
#mean centering age and ssrt
#First added BMI groups
##BMI filtering AGAIN :/

# didn't work Y2_dna <- data[!is.na(Y2_dna$bmigroup),]
#filter out underweights
Y2_dna <- Y2_dna %>% 
  filter(bmi > 18.5)
summary(Y2_dna$bmi)

#add new column of bmi categories
Y2_dna$bmigroup <- cut(Y2_dna$bmi,
                      breaks=c(18.5, 25, 30, 36),
                      labels=c('Recommended', 'Overweight', 'Obese'))
summary(Y2_dna$bmigroup)
Y2_dna$bmigroup <- factor(Y2_dna$bmigroup)

#then MC columns
Y2_dna$mcage <- scale(Y2_dna$age, scale = FALSE)
Y2_dna$mcssrt <- scale(Y2_dna$ssrt, scale = FALSE)
Y2_dna$mcage


#NOW WE ARE READY TO RUN THE MODEL
#FOLLOW-UP SSRT - BMI GROUPS GAM
#then, run the model for meancentered age and ssrt #with no nas

Y2_bmiGssrt <- gam(mcssrt ~ s(mcage, by = bmigroup) + 
    sex_at_birth + household.income + race_ethnicity, 
    data = Y2_dna, method = "REML")
summary(Y2_bmiGssrt)
#I had to drop out household income and race-ethnicity because
#the model says there's too many NAs to run the model


#now plotting the model 
library(visreg)
plot(visreg(Y2_bmiGssrt, xvar = "mcage",
  by = "bmigroup", data = Y2_dna, method = "REML"), 
  legend=TRUE, ylab = "Mean stop signal reaction time ", 
  xlab = "Age (mean-centered)",)

#Error in y + rr : non-conformable arrays

#running the model for non-mc ssrt and mc age bcs plotting doesn't like the MCs

Y2_bmiGssrt_nmc <- gam(ssrt ~ s(mcage, by = bmigroup) + bmigroup +
                     sex_at_birth + household.income + race_ethnicity, 
                   data = Y2_dna, method = "REML")
summary(Y2_bmiGssrt_nmc)

plot(visreg(Y2_bmiGssrt_nmc, xvar = "mcage",
            by = "bmigroup", data = Y2_dna, method = "REML"), 
     legend=TRUE, ylab = "Mean stop signal reaction time ", 
     xlab = "Age (mean-centered)",)

## now trying to plot violin plots

library(tidymv)

model_p <- predict_gam(Y2_bmiGssrt_nmc)
#model_p

model_p %>%
  ggplot(aes(mcage, fit)) +
  geom_smooth_ci(bmigroup) + geom_violin()

predict_gam(Y2_bmiGssrt_nmc) %>%
  ggplot(aes(ssrt, fit)) +
  geom_smooth_ci(bmigroup)
ggplot(Y2_bmiGssrt_nmc)
+ 

ggplot(Y2_bmiGssrt_nmc, aes(x=bmi, y=ssrt, color = bmigroup)) +
  geom_point(shape=1)

ggplot(year2, aes(x=bmi, y=ssrt, color = bmigroup)) +
  geom_point(shape=1)  


##FOLLOW-UP SSRT - BMI IN GENERAL GAM

#then, run the model (for non-meancentered ssrt and MC age)
#names(data_base)
Y2_bmissrt <- gam(ssrt ~ s(bmi) + as.matrix(mcage) +
        race_ethnicity + sex_at_birth + household.income, data = Y2_dna, method = "REML")
summary(Y2_bmissrt)

#added as matrix to mc age because the plotting did not like it


plot(visreg(Y2_bmissrt, xvar = "bmi", data = Y2_dna), 
     legend=TRUE, ylab = "Mean stop signal reaction time ", 
     xlab = "BMI",)

#ASK GRACE WHY IT DOES NOT GIVE RESULTS FOR BMI

------------------------------------------------------------
#started BOLD analysis but then decided to do violin plots
  
library(tidyverse)
library("dplyr")
  
##BOLD data analysis
#We have data in Y2_dna
#I don't want to mess it up so I will use a diff var
Y2B_dna <- Y2_dna

#Now we can renew column names for ROIs

colnames(Y2B_dna) <- gsub("tfmri_sst_all_correct.stop.vs.incorrect.stop_beta_cort.destrieux_", "", colnames(Y2B_dna))
#PREPARING DATA
  

------------------------------------------------------------
#trying to match by sex
matched_data_base <-matrix(data_base)
class(matched_data_base)
class(x)
x <- unlist(c(data_base$bmigroup))
class(as.matrix(data_base))
      
class(data_base$bmigroup)
matched_data_base <- Matchby(Y=unlist(matched_data_base), Tr=unlist(data_base), X=unlist(data_base), by=unlist(data_base$sex_at_birth), M=1);
summary(matched_data_base)

summary(as.factor(is.na(matched_data_base)))
