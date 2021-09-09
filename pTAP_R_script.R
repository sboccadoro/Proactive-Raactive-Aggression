#Libraries needed
library(lme4) ## Linear Mixed-Effects Models using 'Eigen' and S4
library(interactions)
library(haven)
library (emmeans)#marginal means for factors; emtrends for interactions with CVs (i.e.,continuous predictors)
library(lmerTest)#provides p-values for lmer models using Satterthwaite's degrees of freedom method
library(texreg) # Mit dem package texreg lassen sich Ergebnisse sehr schnell und leicht in eine Tabelle packe; mit Befehl file="filename.html" lässt sich die html File auch abspeichern und in Word einladen.
library(sjstats) #
library(car)#needed for Levene's Test
library(readxl)
library(effects)
#_____________________________________________________________________________________________________________________________________________________________________  

#________________________Import data________________________________________________________________________________________________________________________
library(readxl)
pTAP <- read_excel("C:\Folder_name\pTAP_R_data.xlsx") #directory of the data and name of the file with the data
View(pTAP)

#________________________Check structure of imported data________________________________________________________________________________________________________________________
str(pTAP)

#________________________Recode variables________________________________________________________________________________________________________________________
#Recode variables into factors, numeric, integer and rename new variables-> important for mixed model!

pTAP$Trial <- as.numeric(pTAP$Trial) 
pTAP$Trial.z <- (pTAP$Trial - mean(pTAP$Trial))/sd(pTAP$Trial)   # z transformation
pTAP$Trial_recode <- as.numeric(pTAP$Trial_recode)
pTAP$Trial_recode.z <- (pTAP$Trial_recode - mean(pTAP$Trial_recode))/sd(pTAP$Trial_recode)   # z transformation

pTAP$RPQpro <- as.numeric (pTAP$RPQpro)
pTAP$BPAQ.z <- as.numeric (pTAP$BPAQ.z) #BPAQ data have already been z transformed in a previous step
pTAP$SQ <- as.numeric (pTAP$SQ)
pTAP$SP <- as.numeric (pTAP$SP)
pTAP$SR <- as.numeric (pTAP$SR)

pTAP$RPQpro.z <- (pTAP$RPQpro - mean(pTAP$RPQpro))/sd(pTAP$RPQpro)  # z transformation
pTAP$SQ.z <- (pTAP$SQ - mean(pTAP$SQ))/sd(pTAP$SQ)                  # z transformation
pTAP$SR.z <- (pTAP$SR - mean(pTAP$SR))/sd(pTAP$SR)                  # z transformation
pTAP$SP.z <- (pTAP$SP - mean(pTAP$SP))/sd(pTAP$SP)                  # z transformation

pTAP$SCRs <- as.numeric (pTAP$SCRs)
pTAP$SCRsshifted <- pTAP$SCRs +1

pTAP$Code <- factor(pTAP$Code) #factor
pTAP$aggression_choice <- as.numeric (pTAP$aggression_choice)
pTAP$game_outcome <- factor(pTAP$game_outcome)
pTAP$belief_cover_story <- factor(pTAP$belief_cover_story)
pTAP$gender <- factor(pTAP$gender)


#________________________Final aggression model________________________________________________________________________________________________________________________

pTAP_model <- glmer(aggression_choice ~ game_outcome + belief_cover_story +  gender + BPAQ.z + SQ.z + RPQpro.z + SR.z + SP.z + (1+Trial.z|Code) 
                    + (1+game_outcome|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_model)

#________________________Plot effects________________________________________________________________________________________________________________________

e <- allEffects(pTAP_model)
print(e)
plot(e)

#________________________Test Models________________________________________________________________________________________________________________________

#Model included in the first submission
pTAP_model1 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + BPAQ.z + SQ.z + RPQpro.z + SR.z + SP.z
                     + (Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(pTAP_model1)

#model2
pTAP_model2 <- glmer(aggression_choice ~ game_outcome + belief_cover_story +  gender + BPAQ.z + SQ.z + RPQpro.z + SR.z + SP.z
                     + (1+Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_model2)

#model3
pTAP_model3 <- glmer(aggression_choice ~ game_outcome + belief_cover_story +  gender + BPAQ.z + SQ.z + RPQpro.z + SR.z + SP.z + (1+Trial.z|Code) 
                     + (1|game_outcome), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_model3)

#model4
pTAP_model4 <- glmer(aggression_choice ~ game_outcome + belief_cover_story +  gender + BPAQ.z + SQ.z + RPQpro.z + SR.z + SP.z
                     + (1+Trial.z|Code) + (1+game_outcome|Code)
                     + (1|game_outcome), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_model4)
#boundary (singular) fit: see ?isSingular

anova(pTAP_model, pTAP_model1, pTAP_model2, pTAP_model3)

#________________________Final SCRs Model________________________________________________________________________________________________________________________

pTAP_SCRs <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender +
                    + aggression_choice:belief_cover_story + gender:aggression_choice + aggression_choice:game_outcome
                    + (1+Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))
summary(pTAP_SCRs)

#________________________Post Hoc Test________________________________________________________________________________________________________________________

emtrends(pTAP_SCRs, pairwise ~ gender, var = "aggression_choice") #interaction between aggression choice and gender in predicting SCRs

emtrends(pTAP_SCRs, pairwise ~ belief_cover_story, var = "aggression_choice") #interaction between aggression choice and belief in cover story in predicting SCRs

#________________________Plot effects________________________________________________________________________________________________________________________

f <- allEffects(pTAP_SCRs)
print(f)
plot(f)

#________________________Test Models________________________________________________________________________________________________________________________

#Model included in the first submission
pTAP_SCRs1 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender +
                      + aggression_choice:belief_cover_story + gender:aggression_choice + aggression_choice:game_outcome
                    + (Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(pTAP_SCRs1)

#model2
pTAP_SCRs2 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender +
                      + aggression_choice:belief_cover_story + gender:aggression_choice + aggression_choice:game_outcome + (1+game_outcome|Code)
                    + (1+Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_SCRs2)
#Model is nearly unidentifiable: large eigenvalue ratio

#model3
pTAP_SCRs3 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender +
                      + aggression_choice:belief_cover_story + gender:aggression_choice + aggression_choice:game_outcome + (1|game_outcome)
                    + (1+Trial.z|Code), data=pTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(pTAP_SCRs3)

anova(pTAP_SCRs, pTAP_SCRs1, pTAP_SCRs3)
