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
rTAP <- read_excel("C:\Folder_name\rTAP_R_data.xlsx") #directory of the data and name of the file with the data
View(rTAP)

#________________________Check structure of imported data________________________________________________________________________________________________________________________
str(rTAP)


#________________________Recode variables________________________________________________________________________________________________________________________
#Recode variables into factors, numeric, integer and rename new variables-> important for mixed model!

rTAP$Trial <- as.integer(rTAP$Trial) 
rTAP$Trial.z <- (rTAP$Trial - mean(rTAP$Trial))/sd(rTAP$Trial)   # z transformation
rTAP$Trial_recode <- as.integer(rTAP$Trial_recode)
rTAP$Trial_recode.z <- (rTAP$Trial_recode - mean(rTAP$Trial_recode))/sd(rTAP$Trial_recode)   # z transformation

rTAP$RPQre <- as.numeric (rTAP$RPQre)
rTAP$BPAQ.z <- as.numeric (rTAP$BPAQ.z)    #BPAQ data have already been z transformed in a previous step
rTAP$SQ <- as.numeric (rTAP$SQ)
rTAP$SP <- as.numeric (rTAP$SP)
rTAP$SR <- as.numeric (rTAP$SR)

rTAP$RPQre.z <- (rTAP$RPQre - mean(rTAP$RPQre))/sd(rTAP$RPQre) # z transformation
rTAP$SQ.z <- (rTAP$SQ - mean(rTAP$SQ))/sd(rTAP$SQ)             # z transformation
rTAP$SR.z <- (rTAP$SR - mean(rTAP$SR))/sd(rTAP$SR)             # z transformation
rTAP$SP.z <- (rTAP$SP - mean(rTAP$SP))/sd(rTAP$SP)             # z transformation

rTAP$SCRs <- as.numeric (rTAP$SCRs)
rTAP$SCRsshifted <- rTAP$SCRs +1

rTAP$Code <- factor(rTAP$Code) #factor
rTAP$aggression_choice <- as.numeric (rTAP$aggression_choice)
rTAP$game_outcome <- factor(rTAP$game_outcome)
rTAP$provocation <- factor(rTAP$provocation)
rTAP$belief_cover_story <- factor(rTAP$belief_cover_story)
rTAP$gender <- factor(rTAP$gender)

#________________________Final Aggression model________________________________________________________________________________________________________________________

rTAP_model <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                     + provocation:gender + game_outcome:provocation
                     + (1+Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))
summary(rTAP_model)

#________________________Post Hoc Test________________________________________________________________________________________________________________________

emmeans(rTAP_model, pairwise ~ gender|provocation,  adjust = "tukey")      #interaction between gender and provocation in predicting aggression
emmeans(rTAP_model, pairwise ~ provocation|gender,  adjust = "tukey")

emmeans(rTAP_model, pairwise ~ game_outcome|provocation,  adjust = "tukey") #interaction between game outcome and provocation in predicting aggression
emmeans(rTAP_model, pairwise ~ provocation|game_outcome,  adjust = "tukey")

#________________________Plot effects________________________________________________________________________________________________________________________

e <- allEffects(rTAP_model)
print(e)
plot(e)

#________________________Test Models________________________________________________________________________________________________________________________

#Model included in the first submission
rTAP_model1 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                     + provocation:gender + game_outcome:provocation
                     + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(rTAP_model1)

#model 2
rTAP_model2 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                     + provocation:gender + game_outcome:provocation + (1|game_outcome) + (1+game_outcome|Code)
                     + (1+Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_model2)
#unable to evaluate scaled gradient
#Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#model3
rTAP_model3 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                     + provocation:gender + game_outcome:provocation + (1|Trial.z) + (1|game_outcome)
                     + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_model3)


#model4
rTAP_model4 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                     + provocation:gender + game_outcome:provocation + (1+game_outcome|Code)
                     + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))
summary(rTAP_model4)
#boundary (singular) fit: see ?isSingular

#model5
rTAP_model5 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                      + provocation:gender + game_outcome:provocation + (1+Trial.z|Code) 
                      + (1|provocation), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_model5)

#model6
rTAP_model6 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                      + provocation:gender + game_outcome:provocation + (1+Trial.z|Code)
                      + (1+provocation|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_model6)
#unable to evaluate scaled gradient
#Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#model11
rTAP_model11 <- glmer(aggression_choice ~ game_outcome + belief_cover_story + gender + provocation + BPAQ.z + SQ.z + RPQre.z + SR.z + SP.z
                      + provocation:gender + game_outcome:provocation + (1+game_outcome|Code) + (1|provocation)
                      + (1+Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))
summary(rTAP_model11)
#Model failed to converge with max|grad| = 0.237966 (tol = 0.2, component 1)

anova(rTAP_model, rTAP_model1, rTAP_model3, rTAP_model5)

#________________________Final SCRs Model________________________________________________________________________________________________________________________

rTAP_SCRs <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender + provocation  + aggression_choice:belief_cover_story 
                    + gender:aggression_choice + aggression_choice:game_outcome + aggression_choice:provocation 
                    + provocation:gender + game_outcome:provocation + (1+game_outcome|Code)
                    + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_SCRs)

#________________________Post Hoc Test________________________________________________________________________________________________________________________

emmeans(rTAP_SCRs, pairwise ~ provocation|game_outcome, adjust = "tukey")    #interaction between provocation and game outcome in predicting SCRs
emmeans(rTAP_SCRs, pairwise ~ game_outcome|provocation,  adjust = "tukey")   

#________________________Plot effects________________________________________________________________________________________________________________________

f <- allEffects(rTAP_SCRs)
print(f)
plot(f)

#________________________Test Models________________________________________________________________________________________________________________________

#Model included in the first submission
rTAP_SCRs1 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender + provocation  + aggression_choice:belief_cover_story 
                    + gender:aggression_choice + aggression_choice:game_outcome + aggression_choice:provocation 
                    + provocation:gender + game_outcome:provocation
                    + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(rTAP_SCRs1)

#model2
rTAP_SCRs2 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender + provocation  + aggression_choice:belief_cover_story 
                    + gender:aggression_choice + aggression_choice:game_outcome + aggression_choice:provocation 
                    + provocation:gender + game_outcome:provocation
                    + (1+Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(rTAP_SCRs2)

#model3
rTAP_SCRs3 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender + provocation  + aggression_choice:belief_cover_story 
                    + gender:aggression_choice + aggression_choice:game_outcome + aggression_choice:provocation 
                    + provocation:gender + game_outcome:provocation + (1+game_outcome|Code) + (1|game_outcome)
                    + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_SCRs3)
#Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#model4
rTAP_SCRs4 <- glmer(SCRsshifted ~ aggression_choice + game_outcome + belief_cover_story + gender + provocation  + aggression_choice:belief_cover_story 
                    + gender:aggression_choice + aggression_choice:game_outcome + aggression_choice:provocation 
                    + provocation:gender + game_outcome:provocation + (1+game_outcome|Code) + (1|provocation)
                    + (Trial.z|Code), data=rTAP, family = Gamma (link=log), control = glmerControl(optimizer = "Nelder_Mead", check.conv.grad = .makeCC("warning", tol = 2e-1, relTol = NULL),optCtrl=list(maxfun=2e5)))

summary(rTAP_SCRs4)
#boundary (singular) fit: see ?isSingular

anova(rTAP_SCRs, rTAP_SCRs1, rTAP_SCRs2)