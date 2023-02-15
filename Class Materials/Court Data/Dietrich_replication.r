#################################################################
# Description: Replicates Table 1 in Dietrich, Enos, Sen (2018) #
# Author: Bryce J. Dietrich                                     #
# Affiliation: University of Iowa                               #
# Date: 6/7/2018                                                #
# Email: brycedietrich@gmail.com                                #
# R Version: 3.5.0 (2018-04-23) -- "Joy in Playing"             #
# Platform: x86_64-apple-darwin15.6.0 (64-bit)                  #
# Computer: MacBook Pro (Retina, 13-inch, Early 2013)           #
# Processor: 3 GHz Intel Core i7                                #
# OS: macOS Sierra 10.12.6                                      #
# Data: justice_results.tab                                     #
# Packages: lme4_1.1-17                                         #
#           Matrix_1.2-14                                       #
#           stargazer_5.2.2                                     #
# Output: table_1.html                                          #
# Run Time: 26.18765 secs                                       #
#################################################################

require(lme4)
require(stargazer)
library(caTools)
library(dplyr)
library(caret)

#setwd('../')

# read in data
sc<-read.table("justice_results.tab",header=TRUE,as.is=TRUE,sep="\t")

# labels for stargazer
labels = c("Constant",
           "Pitch difference",
           "% more unpleasant words directed at petitioner",
          "% more pleasant words directed at petitioner",
          "# more questions directed at petitioner",
          "Lagged ideology",
          "LC decision was conservative",
          "Lagged ideology * LC decision was conservative",
          "Solicitor general as amicus supporting petitioner",
          "Solicitor general as amicus supporting respondent",
          "# of amicus briefs supporting petitioner",
          "# of amicus brief supporting respondent",
          "Petitioner’s level ofresources",
          "Respondent’s level ofresources",
          "SG amicus * total amicus petitioner",
          "SG amicus * total amicus respondent")
######################
# Table 1 Replication
######################

#intercept only (Table 1, Model 1)
mod0<-glmer(petitioner_vote~1+(1|justiceName),data=sc,family=binomial)
pred_mod0<-sum(diag(table(ifelse(predict(mod0,type="response")>.50,1,0),sc[names(residuals(mod0)),"petitioner_vote"])))/length(residuals(mod0))

#pitch only (Table 1, Model 2)
mod1<-glmer(petitioner_vote~pitch_diff+(1|justiceName),data=sc,family=binomial)
pred_mod1<-sum(diag(table(ifelse(predict(mod1,type="response")>.50,1,0),sc[names(residuals(mod1)),"petitioner_vote"])))/length(residuals(mod1))

#dal model (Table 1, Model 3)
sc$petitioner_pos_words<-sc$petitioner_dal_pos
sc$petitioner_neg_words<-sc$petitioner_dal_neg
sc$respondent_pos_words<-sc$respondent_dal_pos
sc$respondent_neg_words<-sc$respondent_dal_neg

mod2<-glmer(petitioner_vote~pitch_diff+
            I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
            I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
            I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
            lagged_ideology+ 
            conservative_lc+ # lower court decision was conservative
            I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
            sgpetac+ # solicitor general supporting petitioner
            sgrespac+ # solicitor general supporting respondent
            petac+ # num amicus supporting petitioner
            respac+ # num amicus supporing repondent
            petNumStat+ # # petitioner resources
            respNumStat+ # respondent resources 
            (1|justiceName),
            data=sc,
            family=binomial,
            nAGQ=2)


pred_mod2<-sum(diag(table(ifelse(predict(mod2,type="response")>.50,1,0),sc[names(residuals(mod2)),"petitioner_vote"])))/length(residuals(mod2))
#the model does not converge unless the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood is increased from 0. Coefficients and prediction rate are essentiall the same regardless of nAGQ used. More specifically, max coefficient change is around 10^-04

#harvard model (Table 1, Model 4)
sc$petitioner_pos_words<-sc$petitioner_harvard_pos
sc$petitioner_neg_words<-sc$petitioner_harvard_neg
sc$respondent_pos_words<-sc$respondent_harvard_pos
sc$respondent_neg_words<-sc$respondent_harvard_neg

mod3<-glmer(petitioner_vote~pitch_diff+I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+I(petitioner_count-respondent_count)+lagged_ideology+conservative_lc+I(lagged_ideology*conservative_lc)+sgpetac+sgrespac+petac+respac+petNumStat+respNumStat+(1|justiceName),data=sc,family=binomial)
pred_mod3<-sum(diag(table(ifelse(predict(mod3,type="response")>.50,1,0),sc[names(residuals(mod3)),"petitioner_vote"])))/length(residuals(mod3))

#liwc model (Table 1, Model 5)
sc$petitioner_pos_words<-sc$petitioner_liwc_pos
sc$petitioner_neg_words<-sc$petitioner_liwc_neg
sc$respondent_pos_words<-sc$respondent_liwc_pos
sc$respondent_neg_words<-sc$respondent_liwc_neg

mod4<-glmer(petitioner_vote~pitch_diff+I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+I(petitioner_count-respondent_count)+lagged_ideology+conservative_lc+I(lagged_ideology*conservative_lc)+sgpetac+sgrespac+petac+respac+petNumStat+respNumStat+(1|justiceName),data=sc,family=binomial,nAGQ=2)
pred_mod4<-sum(diag(table(ifelse(predict(mod4,type="response")>.50,1,0),sc[names(residuals(mod4)),"petitioner_vote"])))/length(residuals(mod4))
#the model does not converge unless the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood is increased from 0. Coefficients and prediction rate are essentiall the same regardless of nAGQ used. More specifically, max coefficent change is around 10^-04

stargazer(mod0,mod1,mod2,mod3,mod4,type='latex',out='output/table_1.html',intercept.bottom = FALSE, intercept.top = TRUE, omit.stat = c('bic'), dep.var.labels.include = FALSE, dep.var.caption = "", column.labels = c('intercept only','no controls','dal','harvard','liwc'))

####################
## Cross Validation
####################

sc$petitioner_pos_words<-sc$petitioner_dal_pos
sc$petitioner_neg_words<-sc$petitioner_dal_neg
sc$respondent_pos_words<-sc$respondent_dal_pos
sc$respondent_neg_words<-sc$respondent_dal_neg

# Convert petitioner_vote to a factor
sc$petitioner_vote <- as.factor(sc$petitioner_vote)
# lots of missingness with recent data is causing problems with OOS predictions, so drop these cases
sc <- sc[complete.cases(sc),]
# train, test split
set.seed(123)
split <- sample.split(sc, SplitRatio = 0.7)
train = subset(sc, split == T)
test = subset(sc, split == F)


### Original model with Cross validation
set.seed(123)
mod2 <- train(form = petitioner_vote~pitch_diff+
                    I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
                    I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
                    I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
                    lagged_ideology+ 
                    conservative_lc+ # lower court decision was conservative
                    I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
                    sgpetac+ # solicitor general supporting petitioner
                    sgrespac+ # solicitor general supporting respondent
                    petac+ # num amicus supporting petitioner
                    respac+ # num amicus supporing repondent
                    petNumStat+ # # petitioner resources
                    respNumStat, # respondent resources 
             
             data = train[complete.cases(train),],
             trControl = trainControl(method = "cv", number = 5),
             method = 'glm',
             family = binomial)

# model results
mod2$results
mod2$finalModel

# prepping the CV models for stargazer output
mod2glm <- glm(petitioner_vote~pitch_diff+
                    I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
                    I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
                    I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
                    lagged_ideology+ 
                    conservative_lc+ # lower court decision was conservative
                    I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
                    sgpetac+ # solicitor general supporting petitioner
                    sgrespac+ # solicitor general supporting respondent
                    petac+ # num amicus supporting petitioner
                    respac+ # num amicus supporing repondent
                    petNumStat+ # # petitioner resources
                    respNumStat, # respondent resources 
             
             data = train[complete.cases(train),],
             family = "binomial")
mod2$finalModel$call <- mod2glm$call

mod2.1glm <- glm(petitioner_vote~pitch_diff+
                    I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
                    I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
                    I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
                    lagged_ideology+ 
                    conservative_lc+ # lower court decision was conservative
                    I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
                    sgpetac+ # solicitor general supporting petitioner
                    sgrespac+ # solicitor general supporting respondent
                    petac+ # num amicus supporting petitioner
                    respac+ # num amicus supporing repondent
                    petNumStat+ # # petitioner resources
                    respNumStat + # respondent resources 
                    petac * sgpetac +
                    respac * sgrespac,
             
             data = train[complete.cases(train),],
             family = "binomial")
mod2.1$finalModel$call <- mod2.1glm$call

# output the models
stargazer(mod2$finalModel, mod2.1$finalModel,
         type='latex',
          out='output/table_2.html',
          intercept.bottom = FALSE, 
          intercept.top = TRUE, 
          omit.stat = c('bic'), 
          dep.var.labels.include = FALSE, 
          dep.var.caption = "", 
          column.labels = c('DAL (CV)','DAL V2 (CV)'),
         covariate.labels = labels,
         no.space = T)


# refit original model to the entire train set, and get out of sample accuracy
set.seed(123)
mod2OOS<-glmer(petitioner_vote~pitch_diff+
            I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
            I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
            I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
            lagged_ideology+ 
            conservative_lc+ # lower court decision was conservative
            I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
            sgpetac+ # solicitor general supporting petitioner
            sgrespac+ # solicitor general supporting respondent
            petac+ # num amicus supporting petitioner
            respac+ # num amicus supporing repondent
            petNumStat+ # # petitioner resources
            respNumStat+ # respondent resources 
            (1|justiceName),
            data=train,
            family=binomial,
            nAGQ=2)

# get OOS predictions for original model
mod2_pred <- ifelse(predict(mod2OOS, newdata=test, type = "response") > 0.5, 1,0)
# actual votes
actual <- test$petitioner_vote
# flag if prediction was correct
mod2_correct <- ifelse(mod2_pred == actual,1,0)
# drop NA values
mod2_correct <- mod2_correct[!is.na(mod2_correct)]
# calculate correct percent
sum(mod2_correct)/length(mod2_correct)


#### Revised Model with cross validation
set.seed(123)
mod2.1 <- train(form = petitioner_vote~pitch_diff+
                    I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
                    I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words
                    I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
                    lagged_ideology+ 
                    conservative_lc+ # lower court decision was conservative
                    I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
                    sgpetac+ # solicitor general supporting petitioner
                    sgrespac+ # solicitor general supporting respondent
                    petac+ # num amicus supporting petitioner
                    respac+ # num amicus supporing repondent
                    petNumStat+ # # petitioner resources
                    respNumStat + # respondent resources
                    
                    petac * sgpetac +
                    respac * sgrespac,
             
             data = train[complete.cases(train),],
             trControl = trainControl(method = "cv", number = 5),
             method = 'glm',
             family = binomial)

# model results
mod2.1$results
mod2.1$finalModel

# Fit revised model to entire trainig set and test OOS predictions
mod2.1OOS<-glmer(petitioner_vote~pitch_diff+
            I((petitioner_neg_words/petitioner_wc)-(respondent_neg_words/respondent_wc))+ # % more unpleasant words
            I((petitioner_pos_words/petitioner_wc)-(respondent_pos_words/respondent_wc))+# % more pleasant words  
            I(petitioner_count-respondent_count)+ # #more questions directed and petitioner
            lagged_ideology+ 
            conservative_lc+ # lower court decision was conservative
            I(lagged_ideology*conservative_lc)+ # Ideology times lower court decision
            sgpetac+ # solicitor general supporting petitioner
            sgrespac+ # solicitor general supporting respondent
            petac+ # num amicus supporting petitioner
            respac+ # num amicus supporing repondent
            petNumStat+ # # petitioner resources
            respNumStat+ # respondent resources 
            
            petac * sgpetac +
            respac * sgrespac +
            
            (1|justiceName),
            data=train,
            family=binomial,
            nAGQ=2)

# get OOS predictions for alternative model
mod2.1_pred <- ifelse(predict(mod2.1OOS, newdata=test, type = "response") > 0.5, 1,0)
# actual votes
actual <- test$petitioner_vote
# flag if prediction was correct
mod2.1_correct <- ifelse(mod2.1_pred == actual,1,0)
# drop NA values
mod2.1_correct <- mod2.1_correct[!is.na(mod2.1_correct)]
# calculate correct percent
sum(mod2.1_correct)/length(mod2.1_correct)

# OOS prediction table 
stargazer(mod2OOS, mod2.1OOS,
          type='latex',
          out='output/table_1.html',
          intercept.bottom = FALSE, 
          intercept.top = TRUE, 
          omit.stat = c('bic'), 
          dep.var.labels.include = FALSE, 
          dep.var.caption = "", 
          column.labels = c('DAL (Out of Sample)','DAL V2 (Out of Sample)'))
