getwd()
bank <- read.csv("C:/Users/us/Desktop/Projects/Intermediate Analytics/Assignment 4/banking_data.csv")
str(bank)
##################
summary(bank)
is.factor(bank$marital)
head(bank)
###################
install.packages("Amelia")
library(Amelia)
missmap(bank,main="Missing Values vs Observed", col = c("BLACK","LIGHT BLUE" ), legend= FALSE)

sapply(bank,function(x) sum(is.na(x)))
sapply(bank, function(x) length(unique(x)))

contrasts(bank$marital)
contrasts(bank$default)
contrasts(bank$housing)
contrasts(bank$contact)
contrasts(bank$poutcome)
contrasts(bank$y)
#######################

###Intercept only model
c_only <- glm(y~1,family = binomial, data = bank)
c_only

coef(c_only)## -2.063912

##funcion to convert logit to probability
logit2prob <- function(logit){
  
  odds<- exp(logit)
  prob<- odds/(1+odds)
  return(prob)
}
##function call
prob<- logit2prob(coef(c_only))
prob ## 0.1126542

install.packages("gmodels")
library("gmodels")

##CrossTable
CrossTable(bank$y)

##Manual Probability Calculation for Yes and No of Term Deposit
yes_logit<- coef((c_only)[1])
y_prob<- logit2prob(yes_logit)
n_prob<- 1- y_prob

y_prob ##0.1126542
n_prob ##0.8873458

###############################################################################################
##Handling NA values
##Instead of omiting NA values or using KNN method, 
##we have utilised the categorization method by introducing a category : "UNKNOWN"

str(bank$education)
bank$education[is.na(bank$education)]<- 999 ##Replace NA by introducing new integer value: 999

str(bank$occupation)
bank$occupation[is.na(bank$occupation)]<- 999 ##Replace NA by introducing new integer value: 999

bank$education <- factor(bank$education,labels=c("LOW","INTERMEDIATE","HIGH","UNKNOWN"))
str(bank$education)
levels(bank$education)

target1<- glm(y~education, data= bank, family='binomial')
summary(target1)

##y = intercept + b1x1+b2x2+b3x3
y1<- -2.34201+0.44785*1 ##high
y2<- -2.34201 ##low

##OddsRatio
#Manual
##y= log(p/1-p) ; p/1-p = exp(y); 
h_odd<- exp(y1)
l_odd<- exp(y2)
odds_h_l <- h_odd/l_odd
odds_h_l##High_education vs Low_education OddsRatio = 1.564944

tab<- table(bank$y,bank$education, exclude= c("INTERMEDIATE", "UNKNOWN"))
tab

##OddsRatio
##using package epiR
install.packages("epiR")
library(epiR)
epi.2by2(tab,method = "cohort.count",conf.level = 0.95)

##Probability of Highly educated clients taking Term Deposit
#method 1
h_prob<- logit2prob(y1)
h_prob ##0.1307709
#method 2
h1<-exp(-2.34801+0.44785)/(1+exp(-2.34801+0.44785))
h1  ##0.1300904

##Probability of Lowly educated clients taking Term Deposit
#method 1
l_prob<- logit2prob(y2)
l_prob
#method 2
l2<- exp(-2.34801)/(1+exp(-2.34801))
l2

CrossTable(bank$education,bank$y)  
  

##
#bank2<- subset(bank,bank$education =="LOW" | bank$education=="HIGH",select = X:y)
#bank2
#target<- glm(y~education, data= bank2, family='binomial')
#target
#coef(target)
#summary(target)
#logit2prob(coef(target))
##

##
#exp(0.45201)
#exp(-1.88896-0.45201)/(1+exp(-1.88896-0.45201))
#exp(-1.88896)/(1+exp(-1.88896))
##

##################################################################################################
#Highest day of the weeek for Term Deposit Sign Up

bank[bank$day==1,]$day<- "MONDAY"
bank[bank$day==2,]$day<- "TUESDAY"
bank[bank$day==3,]$day<- "WEDNESDAY"
bank[bank$day==4,]$day<- "THURSDAY"
bank[bank$day==5,]$day<- "FRIDAY"

bank$day<- as.factor(bank$day)
is.factor(bank$day)

str(bank$day)

day_model<- glm(y~day, data=bank, family="binomial")
summary(day_model)

coef(day_model)

fri_p<- logit2prob(coef(day_model)[1])
fri_p ##0.1080874

mon_p<- exp( -2.11042809-0.09255)/(1+exp( -2.11042809-0.09255))
mon_p ##0.09948337

thurs_p<-exp( -2.11042809+0.12919566)/(1+exp( -2.11042809+0.12919566))
thurs_p ##0.1211875

tue_p<- exp( -2.11042809+0.09699519)/(1+exp( -2.11042809+0.09699519))
tue_p ##0.1177998

wed_p<- exp( -2.11042809+0.08608609)/(1+exp( -2.11042809+0.08608609))
wed_p ##0.1166708

highest_day<- thurs_p
highest_day ##0.1211875

######################################################################################
#Logistic Regression for Predicting Term Deposit sign up

set.seed(12345)
split<- sample(seq_len(nrow(bank)),size= floor(0.80*nrow(bank)))
train_data11<- bank[split, ]
test_data11<- bank[-split, ]
head(train_data11)
head(test_data11)

summary(glm(y~., data= train_data11,family=binomial(link='logit')))
sat_mod<- glm(y~., data= train_data11,family=binomial(link='logit'))

library("MASS")
stepAIC(sat_mod, direction="backward")

fin_stepAIC_mod<- glm(formula = y ~ X + age + marital + education + default + housing + 
              contact + day + duration + campaign + pdays + poutcome, 
              family = binomial(link = "logit"), 
              data = train_data11)

summary(fin_stepAIC_mod)
## we can observe that Marital status and Housing still do  not add much value to the prediction
# we can eliminate them by observation from our model.
anova(sat_mod, glm(y~.-contact,data = train_data11, family = binomial (link= "logit")),test = "Chisq")
anova(sat_mod,test="Chisq")


fin_mod <- glm(y~ X+age+education+default+contact+day+duration+campaign+pdays+poutcome, 
               family = binomial(link="logit"),
               data=train_data11)


summary(fin_mod)
fin_mod


install.packages("ROCR")
library("ROCR")

test_data11$pred_data<- predict(fin_mod,test_data11,type='response')

test_data11$predictedY<- ifelse(test_data11$pred_data>0.5, 1,0)

test_data11$pred_num<- ifelse(test_data11$pred_data>0.5, 1,0)
test_data11$predictedY<- factor(test_data11$predictedY, levels=c("0","1"),labels=c("no","yes"))  
str(test_data11$y)
str(test_data11$predictedY)

##confusion_matrix<- confusionMatrix(test_data11$y, test_data11$predictedY, threshold= 0.5)

c_tab<- table(test_data11$predictedY, test_data11$y)
c_tab

(7085+381)/sum(c_tab)


install.packages('InformationValue')
library(InformationValue)

misClasificError<- mean(test_data11$predictedY != test_data11$y)
Accuracy<- 1-misClasificError

mis<- misClassError(as.integer(test_data11$y), as.integer(test_data11$predictedY), threshold = 0.5)
accur<- 1-mis
accur

##plot TPR vs FPR , 
p<- predict(fin_mod, test_data11,type="response")
pr <- prediction(predictions = p,test_data11$y)
perf<- performance(pr,measure = "tpr",x.measure="fpr")
perf
plot(perf)
## AREA UNDER CURVE
auc<- performance(pr, measure = "auc")
auc<- auc@y.values[[1]]
auc


##ROC
plotROC(as.integer(test_data11$y),test_data11$pred_data)

sensitivity(as.numeric(test_data11$y),as.numeric(test_data11$predictedY), threshold = 0.5)

install.packages("pROC")
library("pROC")

x<- rnorm(1000)
pre<- exp(5*x)/(1+exp(5*x))

y<- 1 *(runif(1000)< pre)
modd<- glm(y~x, family="binomial")
predpr<- predict(modd, type= "response")
rocc<- roc(y~predpr)
plot(rocc)

pre2<- exp(0*x)/(1+exp(0*x))
y2<- 1 *(runif(1000)< pre2)
modd2<- glm(y2~x, family="binomial")
predpr2<- predict(modd2, type= "response")
rocc2<- roc(y2~predpr2)
plot(rocc2)

###################################################################################
optimalCutoff(as.numeric(test_data11$y),as.numeric(test_data11$predictedY),optimiseFor = "misclasserror", returnDiagnostics = TRUE)
###################################
#tree model trial.
install.packages("tree")
library(tree)
y_pred_num<-  ifelse(p>0.5,"yes","no")
y_predicted<- factor(y_pred_num,levels=c("no","yes"))
y_observed<- test_data11$y

mean(y_predicted == y_observed, na.rm = TRUE)

plot(tree.model)
text(tree.model)



