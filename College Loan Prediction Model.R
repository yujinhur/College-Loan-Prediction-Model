
#new data set
install.packages(readr)
library(readr)
College <- read_csv("~/Documents/R/Most-Recent-Cohorts-All-Data-Elements.csv")
#View(College)

College1 <- read.csv("~/Most-Recent-Cohorts-All-Data-Elements.csv")

#Removes PrivacySuppressed Rows and NA rows
College2 <- subset(College, College$DEBT_MDN != "PrivacySuppressed")
College2$DEBT_MDN <- as.numeric(College2$DEBT_MDN)
College2 <- subset(College2, !is.na(College2$DEBT_MDN))
College2 <- College2[complete.cases(College2),] #6275

#for(col in names(College2)){
#  College2 <- subset(College2, sum(College2$col=="NULL")=0)
#}

#for(col in names(College2)){
#  College2 <- subset(College2, col != "PrivacySuppressed")
#}
for(c in names(College2)){
 College2[!grepl("PrivacySuppressed", col),]
}


College2_1 <- College2[!grepl("PrivacySuppressed", College2$FIRSTGEN_UNKN_2YR_TRANS_YR4_RT),]
#write.table(College2_1, "college2_1.csv")

College3 <- College2[, colSums(College2=="NULL")==0]
College3 <- College3[-colSums(College3=="PrivacySuppressed")!=0]
View(College3)

College5<- College
College5 <- College5[,colSums(College5=="PrivacySuppressed")>(7703-500)]




#training data set
train_index = sample(nrow(College3), nrow(College3)/2)
college_train <-College3[train_index, ]
college_test <- College3[-train_index, ]


lm1<- lm(DEBT_MDN~MENONLY+WOMENONLY, data= College3)
summary(lm1)
lm1_train<- lm(DEBT_MDN~MENONLY+WOMENONLY, data= college_train)
summary(lm1_train)
lm2_train<- lm(DEBT_MDN~MENONLY+WOMENONLY+GRAD_DEBT_MDN+WDRAW_DEBT_MDN+ICLEVEL+LONGITUDE, data= college_train)
summary(lm2_train)

#these linear models are too large
#lm2<- lm(DEBT_MDN~MENONLY+WOMENONLY+GRAD_DEBT_MDN+WDRAW_DEBT_MDN+ICLEVEL+LONGITUDE, data= College3)
#summary(lm2)
#lm3<- lm(DEBT_MDN~MENONLY+WOMENONLY+WDRAW_DEBT_MDN+ICLEVEL+LONGITUDE, data= College3)
#summary(lm3)

#try 500 samples
train_index_500 = sample(nrow(College3), 500)
college_train_500 <-College3[train_index_500, ]
lm1_train_500<- lm(DEBT_MDN~MENONLY+WOMENONLY, data= college_train_500)
summary(lm1_train_500)
lm2_train_500<- lm(DEBT_MDN~MENONLY+WOMENONLY+GRAD_DEBT_MDN+WDRAW_DEBT_MDN+ICLEVEL+LONGITUDE, data= college_train_500)
summary(lm2_train_500)

lm3_train_500 <- lm(DEBT_MDN~., data=college_train_500)

#try forward subset selection
null = lm(DEBT_MDN~1, data = college_train_500)
full = lm(DEBT_MDN~., data = college_train_500)
step(null, scope=list(lower=null, upper=full), direction="forward")




debt <- na.omit(as.numeric(College1$DEBT_MDN))
debt
mean(debt)
median(debt)
sd(debt)

medianwage <- na.omit(as.numeric(College$MD_EARN_WNE_P10))
medianwage
mean(medianwage)
median(medianwage)
sd(medianwage)


#testing 100 data point script
College_100 <- read_excel("~/Documents/R/College_Data_100_Points.xlsx")
View(College_100)
for (i in 1:length(names)) {
#College_100 <- College_100[!grepl("PrivacySuppressed", names(College_100)[i])]
College_100 <- subset(College_100, names(College_100)[1] != "PrivacySuppressed")
}

College7 <- read.csv("~/R/4740file.csv")

counter1 = 0
counter2 = 0
for (i in 1:nrow(College7)){
  if (sum(College7[i,]=="NULL") >500){
    College7 <- College7[-i,]
  }
  else if (sum(College7[i,]=="PrivacySuppressed") >500){
    College7 <- College7[-i,]
  }
}
"done"
counter1
counter2

#cleaning up the most Relevant Var Files
Vars28 <- read.csv("~/R/mostrelevantvar.csv")
#Removes PrivacySuppressed Rows and NA rows
Vars28<- subset(Vars28, Vars28$DEBT_MDN != "PrivacySuppressed")
Vars28$DEBT_MDN <- as.numeric(Vars28$DEBT_MDN)
Vars28 <- subset(Vars28, !is.na(Vars28$DEBT_MDN))
Vars28 <- Vars28[complete.cases(Vars28),] #6275

varlist = names(Vars28)[1:length(names(Vars28))-1]
varlist
for (i in names(Vars28)[1:length(names(Vars28))-1]){
  Vars28[,i] <- as.numeric(Vars28[,i])
  Vars28[,i][is.na(Vars28[,i])]=median(Vars28[,i], na.rm=TRUE)
  
}

lm <- lm(DEBT_MDN~., data = Vars28)
summary(lm)
  
#forward subset selection
null = lm(DEBT_MDN~1, data = Vars28)
full = lm(DEBT_MDN~., data = Vars28)
step(null, scope=list(lower=null, upper=full), direction="both")
a <- regsubsets(DEBT_MDN~., data=Vars28, method="backward")
summary(a)

lm_college_101 <- lm(DEBT_MDN~COSTT4_A+ADM_RATE+UG+MD_FAMINC+CONTROL+HIGHDEG+LATITUDE+LONGITUDE+MENONLY+WOMENONLY, data= Vars28)
summary(lm_college_101)

##################################################################################################################################

#START HERE
College <- read.csv("~/Colleges6.csv", sep="")
#try cleaning up full data set using for loop
College<- subset(College, College$DEBT_MDN != "PrivacySuppressed")
College$DEBT_MDN <- as.numeric(College$DEBT_MDN)
College <- subset(College, !is.na(College$DEBT_MDN))
College <- College[complete.cases(College),] #6275

varlist = names(College)[1:length(names(College))-1]
varlist
for (i in names(College)[1:length(names(College))-1]){
  College[,i] <- as.numeric(College[,i])
  College[,i][is.na(College[,i])]=mean(College[,i], na.rm=TRUE)
  
}

##########END
lm1 <- lm(DEBT_MDN~., data = College)
summary(lm1)

install.packages("LEAP")
library(LEAP)
a <- regsubsets(DEBT_MDN~., data=College, method="forward")
summary(a)

#forward subset selection
null = lm(DEBT_MDN~1, data = College)
full = lm(DEBT_MDN~., data = College)
step(null, scope=list(lower=null, upper=full), direction="both")

#linear model from forward subset selection
step(null, scope=list(lower=null, upper=full), direction="forward")
lm1 <- lm(formula = DEBT_MDN ~ PELL_DEBT_MDN + FEMALE_DEBT_N + PREDDEG + 
            CONTROL + LOAN_DEATH_YR8_RT + PAR_ED_PCT_1STGEN + LOAN_EVER + 
            LOAN_YR2_N + HI_INC_RPY_5YR_N + NONCOM_RPY_7YR_RT + CUML_DEBT_P75 + 
            PCIP54 + TUITIONFEE_OUT + D150_L4_POOLED + PELL_ENRL_4YR_TRANS_YR6_RT + 
            FIRSTGEN_UNKN_ORIG_YR4_RT + LOAN_ENRL_4YR_TRANS_YR8_RT + 
            NOLOAN_COMP_ORIG_YR6_RT + LOAN_ENRL_4YR_TRANS_YR2_RT + ICLEVEL + 
            PCT_BA + WDRAW_ORIG_YR3_RT + NOT1STGEN_ENRL_ORIG_YR6_RT + 
            DEP_COMP_4YR_TRANS_YR4_RT + CIP26ASSOC + CIP09CERT4 + HBCU + 
            FEMALE_COMP_4YR_TRANS_YR6_RT + RPY_5YR_RT + NOTFIRSTGEN_RPY_5YR_N + 
            LOAN_WDRAW_ORIG_YR4_RT + LOAN_COMP_4YR_TRANS_YR3_RT + ACTEN75 + 
            WDRAW_DEBT_N + COUNT_NWNE_P9 + CURROPER + NOPELL_RPY_1YR_N + 
            NPT4_75UP_PRIV + APPL_SCH_PCT_GE2 + HI_INC_COMP_ORIG_YR6_RT + 
            IND_COMP_ORIG_YR8_RT + NOT1STGEN_COMP_ORIG_YR8_RT + C150_4_NRA + 
            CIP24BACHL + CIP39BACHL + CIP47BACHL + PPTUG_EF2 + NOT1STGEN_COMP_2YR_TRANS_YR3_RT + 
            DEP_RPY_3YR_RT + HIGHDEG + NOT1STGEN_COMP_ORIG_YR2_RT + MALE_ENRL_ORIG_YR8_RT + 
            D200_4_POOLED + SATWR25 + MD_INC_ENRL_4YR_TRANS_YR8_RT + 
            MALE_COMP_2YR_TRANS_YR4_RT, data = College)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

#Validation Set Approach- forward subset selection model
set.seed(1)
train_index = sample(nrow(College), nrow(College)/2)
college_train <-College[train_index, ]
college_test <- College[-train_index, ]

lm1.train <- lm(formula = DEBT_MDN ~ PELL_DEBT_MDN + FEMALE_DEBT_N + PREDDEG + 
            CONTROL + LOAN_DEATH_YR8_RT + PAR_ED_PCT_1STGEN + LOAN_EVER + 
            LOAN_YR2_N + HI_INC_RPY_5YR_N + NONCOM_RPY_7YR_RT + CUML_DEBT_P75 + 
            PCIP54 + TUITIONFEE_OUT + D150_L4_POOLED + PELL_ENRL_4YR_TRANS_YR6_RT + 
            FIRSTGEN_UNKN_ORIG_YR4_RT + LOAN_ENRL_4YR_TRANS_YR8_RT + 
            NOLOAN_COMP_ORIG_YR6_RT + LOAN_ENRL_4YR_TRANS_YR2_RT + ICLEVEL + 
            PCT_BA + WDRAW_ORIG_YR3_RT + NOT1STGEN_ENRL_ORIG_YR6_RT + 
            DEP_COMP_4YR_TRANS_YR4_RT + CIP26ASSOC + CIP09CERT4 + HBCU + 
            FEMALE_COMP_4YR_TRANS_YR6_RT + RPY_5YR_RT + NOTFIRSTGEN_RPY_5YR_N + 
            LOAN_WDRAW_ORIG_YR4_RT + LOAN_COMP_4YR_TRANS_YR3_RT + ACTEN75 + 
            WDRAW_DEBT_N + COUNT_NWNE_P9 + CURROPER + NOPELL_RPY_1YR_N + 
            NPT4_75UP_PRIV + APPL_SCH_PCT_GE2 + HI_INC_COMP_ORIG_YR6_RT + 
            IND_COMP_ORIG_YR8_RT + NOT1STGEN_COMP_ORIG_YR8_RT + C150_4_NRA + 
            CIP24BACHL + CIP39BACHL + CIP47BACHL + PPTUG_EF2 + NOT1STGEN_COMP_2YR_TRANS_YR3_RT + 
            DEP_RPY_3YR_RT + HIGHDEG + NOT1STGEN_COMP_ORIG_YR2_RT + MALE_ENRL_ORIG_YR8_RT + 
            D200_4_POOLED + SATWR25 + MD_INC_ENRL_4YR_TRANS_YR8_RT + 
            MALE_COMP_2YR_TRANS_YR4_RT, data = college_train)
summary(lm1.train)

lm1.test <- lm(formula = DEBT_MDN ~ PELL_DEBT_MDN + FEMALE_DEBT_N + PREDDEG + 
                  CONTROL + LOAN_DEATH_YR8_RT + PAR_ED_PCT_1STGEN + LOAN_EVER + 
                  LOAN_YR2_N + HI_INC_RPY_5YR_N + NONCOM_RPY_7YR_RT + CUML_DEBT_P75 + 
                  PCIP54 + TUITIONFEE_OUT + D150_L4_POOLED + PELL_ENRL_4YR_TRANS_YR6_RT + 
                  FIRSTGEN_UNKN_ORIG_YR4_RT + LOAN_ENRL_4YR_TRANS_YR8_RT + 
                  NOLOAN_COMP_ORIG_YR6_RT + LOAN_ENRL_4YR_TRANS_YR2_RT + ICLEVEL + 
                  PCT_BA + WDRAW_ORIG_YR3_RT + NOT1STGEN_ENRL_ORIG_YR6_RT + 
                  DEP_COMP_4YR_TRANS_YR4_RT + CIP26ASSOC + CIP09CERT4 + HBCU + 
                  FEMALE_COMP_4YR_TRANS_YR6_RT + RPY_5YR_RT + NOTFIRSTGEN_RPY_5YR_N + 
                  LOAN_WDRAW_ORIG_YR4_RT + LOAN_COMP_4YR_TRANS_YR3_RT + ACTEN75 + 
                  WDRAW_DEBT_N + COUNT_NWNE_P9 + CURROPER + NOPELL_RPY_1YR_N + 
                  NPT4_75UP_PRIV + APPL_SCH_PCT_GE2 + HI_INC_COMP_ORIG_YR6_RT + 
                  IND_COMP_ORIG_YR8_RT + NOT1STGEN_COMP_ORIG_YR8_RT + C150_4_NRA + 
                  CIP24BACHL + CIP39BACHL + CIP47BACHL + PPTUG_EF2 + NOT1STGEN_COMP_2YR_TRANS_YR3_RT + 
                  DEP_RPY_3YR_RT + HIGHDEG + NOT1STGEN_COMP_ORIG_YR2_RT + MALE_ENRL_ORIG_YR8_RT + 
                  D200_4_POOLED + SATWR25 + MD_INC_ENRL_4YR_TRANS_YR8_RT + 
                  MALE_COMP_2YR_TRANS_YR4_RT, data = college_test)
summary(lm1.test)
#MSE = 118499.6
mean((College$DEBT_MDN -predict (lm1.train,College))[-train_index ]^2)

#LOOCV - forward subset selection model
lm1.cv <- glm(formula = DEBT_MDN ~ PELL_DEBT_MDN + FEMALE_DEBT_N + PREDDEG + 
                  CONTROL + LOAN_DEATH_YR8_RT + PAR_ED_PCT_1STGEN + LOAN_EVER + 
                  LOAN_YR2_N + HI_INC_RPY_5YR_N + NONCOM_RPY_7YR_RT + CUML_DEBT_P75 + 
                  PCIP54 + TUITIONFEE_OUT + D150_L4_POOLED + PELL_ENRL_4YR_TRANS_YR6_RT + 
                  FIRSTGEN_UNKN_ORIG_YR4_RT + LOAN_ENRL_4YR_TRANS_YR8_RT + 
                  NOLOAN_COMP_ORIG_YR6_RT + LOAN_ENRL_4YR_TRANS_YR2_RT + ICLEVEL + 
                  PCT_BA + WDRAW_ORIG_YR3_RT + NOT1STGEN_ENRL_ORIG_YR6_RT + 
                  DEP_COMP_4YR_TRANS_YR4_RT + CIP26ASSOC + CIP09CERT4 + HBCU + 
                  FEMALE_COMP_4YR_TRANS_YR6_RT + RPY_5YR_RT + NOTFIRSTGEN_RPY_5YR_N + 
                  LOAN_WDRAW_ORIG_YR4_RT + LOAN_COMP_4YR_TRANS_YR3_RT + ACTEN75 + 
                  WDRAW_DEBT_N + COUNT_NWNE_P9 + CURROPER + NOPELL_RPY_1YR_N + 
                  NPT4_75UP_PRIV + APPL_SCH_PCT_GE2 + HI_INC_COMP_ORIG_YR6_RT + 
                  IND_COMP_ORIG_YR8_RT + NOT1STGEN_COMP_ORIG_YR8_RT + C150_4_NRA + 
                  CIP24BACHL + CIP39BACHL + CIP47BACHL + PPTUG_EF2 + NOT1STGEN_COMP_2YR_TRANS_YR3_RT + 
                  DEP_RPY_3YR_RT + HIGHDEG + NOT1STGEN_COMP_ORIG_YR2_RT + MALE_ENRL_ORIG_YR8_RT + 
                  D200_4_POOLED + SATWR25 + MD_INC_ENRL_4YR_TRANS_YR8_RT + 
                  MALE_COMP_2YR_TRANS_YR4_RT, data = College)
summary(lm1.cv)
coef(lm1.cv)
coef(lm1)


library(boot)
cv.err=cv.glm(College ,lm1.cv)
cv.err$delta #95569.49 95560.52


#LASSO
install.packages("glmnet")
library(glmnet)

x = model.matrix(DEBT_MDN~., College)
ridge.fit = glmnet(x, College$DEBT_MDN, alpha = 1, lambda = 0.5)
plot(ridge.fit)
coef(ridge.fit)
set.seed(1)

cv.out=cv.glmnet(x[train_index ,],College$DEBT_MDN[ train_index],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam 
lasso.pred=predict (ridge.fit ,s=bestlam ,newx=x[-train_index ,])
mean((lasso.pred -college_test)^2)

out= glmnet(x, College$DEBT_MDN, alpha = 1, lambda = 16)
lasso.pred=predict (out ,s=bestlam ,newx=x[-train_index ,])
mean((lasso.pred -college_test)^2)
coef(out)

lasso.coef=predict (out ,type=" coefficients",s= bestlam) [1:20,]
lasso.coef
#Chosen variables / Lasso model 
lm.lasso <- lm(DEBT_MDN~PREDDEG +CONTROL+CIP51CERT1 +TUITIONFEE_OUT + MD_INC_UNKN_ORIG_YR3_RT +WDRAW_2YR_TRANS_YR4_RT
+LOAN_WDRAW_ORIG_YR4_RT  + WDRAW_2YR_TRANS_YR4_RT
+ICLEVEL  +MN_EARN_WNE_INC2_P10  + VETERAN + HI_INC_RPY_5YR_N + MALE_RPY_3YR_N + CUML_DEBT_P75 
+FEMALE_DEBT_N + WDRAW_DEBT_N + PELL_DEBT_MDN + LOAN_YR2_N + MD_INC_YR2_N + PAR_ED_PCT_1STGEN
+NONCOM_RPY_7YR_RT + DEP_RPY_3YR_RT  + LOAN_ENRL_4YR_TRANS_YR8_RT + LOAN_DEATH_YR8_RT + DEP_WDRAW_2YR_TRANS_YR8_RT
+PELL_ENRL_4YR_TRANS_YR6_RT +FIRSTGEN_UNKN_ORIG_YR4_RT, data = College)

summary(lm.lasso)

#CV on lasso model 
lm.lasso <- lm(DEBT_MDN~PREDDEG +CONTROL+CIP51CERT1 +TUITIONFEE_OUT + MD_INC_UNKN_ORIG_YR3_RT +WDRAW_2YR_TRANS_YR4_RT
               +LOAN_WDRAW_ORIG_YR4_RT  + WDRAW_2YR_TRANS_YR4_RT
               +ICLEVEL  +MN_EARN_WNE_INC2_P10  + VETERAN + HI_INC_RPY_5YR_N + MALE_RPY_3YR_N + CUML_DEBT_P75 
               +FEMALE_DEBT_N + WDRAW_DEBT_N + PELL_DEBT_MDN + LOAN_YR2_N + MD_INC_YR2_N + PAR_ED_PCT_1STGEN
               +NONCOM_RPY_7YR_RT + DEP_RPY_3YR_RT  + LOAN_ENRL_4YR_TRANS_YR8_RT + LOAN_DEATH_YR8_RT + DEP_WDRAW_2YR_TRANS_YR8_RT
               +PELL_ENRL_4YR_TRANS_YR6_RT +FIRSTGEN_UNKN_ORIG_YR4_RT, data = College)

glm.lasso <- glm(DEBT_MDN~PREDDEG +CONTROL+CIP51CERT1 +TUITIONFEE_OUT + MD_INC_UNKN_ORIG_YR3_RT +WDRAW_2YR_TRANS_YR4_RT
               +LOAN_WDRAW_ORIG_YR4_RT  + WDRAW_2YR_TRANS_YR4_RT
               +ICLEVEL  +MN_EARN_WNE_INC2_P10  + VETERAN + HI_INC_RPY_5YR_N + MALE_RPY_3YR_N + CUML_DEBT_P75 
               +FEMALE_DEBT_N + WDRAW_DEBT_N + PELL_DEBT_MDN + LOAN_YR2_N + MD_INC_YR2_N + PAR_ED_PCT_1STGEN
               +NONCOM_RPY_7YR_RT + DEP_RPY_3YR_RT  + LOAN_ENRL_4YR_TRANS_YR8_RT + LOAN_DEATH_YR8_RT + DEP_WDRAW_2YR_TRANS_YR8_RT
               +PELL_ENRL_4YR_TRANS_YR6_RT +FIRSTGEN_UNKN_ORIG_YR4_RT, data = College)


summary(glm.lasso)
coef(glm.lasso)
coef(lm.lasso)


library(boot)
cv.err=cv.glm(College ,glm.lasso)
cv.err$delta #104251.7 104247.6




#Regression Tree
#Growing
install.packages("tree")
library(tree)
tree1 = tree(DEBT_MDN~., College)
cv1 = cv.tree(tree1)
par(mfrow=c(1,1))
plot(cv1$size, cv1$dev, type='b') # CV error vs. tree size
tree2 = prune.tree(tree1, best = 5)
plot(tree2)
text(tree2)
pred2 = predict(tree2, newdata = College)
summary(tree1)
#fitting a classification tree #wrong :( 
# set.seed(2)
# train=sample (1:nrow(College), nrow(College)/2)
# College.test=College[-train ,]
# College$DEBT_MDN.test <-College$DEBT_MDN[-train]
# tree.college =tree(DEBT_MDN~. , College ,subset=train)
# tree.pred=predict(tree.college ,College.test ,type="class")
# table(tree.pred, College$DEBT_MDN.test)

#fitting a regression tree
library(MASS)
set.seed(1)
train = sample (1:nrow(College), nrow(College)/2)
tree.college=tree(DEBT_MDN~.,College , subset=train)
summary(tree.college)
plot(tree.college)
text(tree.college, pretty=0)

cv.college=cv.tree(tree.college)
plot(cv.college$size ,cv.college$dev ,type='b')
prune.college=prune.tree(tree.college ,best=6)
plot(prune.college)
text(prune.college , pretty =0)
yhat=predict(tree.college ,newdata=College[-train ,])
college.test=College[-train ,"DEBT_MDN"]
plot(yhat ,college.test)
abline (0,1)
mean((yhat-college.test)^2) #151458.3

#Bagging and random forest
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.college= randomForest(DEBT_MDN~.,data=College, subset=train,mtry=13,importance =TRUE)
bag.college

yhat.bag = predict (bag.college, newdata=College[-train ,])
plot(yhat.bag , college.test)
abline (0,1)
mean((yhat.bag -college.test)^2) #148724.1

bag.college= randomForest(DEBT_MDN~.,data=College , subset=train ,
                          mtry=13,ntree=25)
yhat.bag = predict (bag.college , newdata=College[-train ,])
mean((yhat.bag -college.test)^2) #185255.6


set.seed(1)
rf.college= randomForest(DEBT_MDN~.,data=College , subset=train ,
                          mtry=6, importance =TRUE)
yhat.rf = predict(rf.college ,newdata=College[-train ,])
mean((yhat.rf-college.test)^2)# 186106.1

importance (rf.college)
varImpPlot (rf.college)


#Boosting with regression trees
install.packages("gbm")
library(gbm)
set.seed(1)
tree1 = gbm(DEBT_MDN~., data=College, distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(tree1)
par(mfrow=c(1,2))
plot(tree1 ,i="PELL_DEBT_MDN")
plot(tree1 ,i="LOAN_EVER")

yhat.boost=predict(tree1 ,newdata =College[-train ,], n.trees=5000)
mean((yhat.boost - college.test)^2) #64254.24


boost.college=gbm(DEBT_MDN~.,data=College[train ,], distribution= "gaussian",n.trees =5000, interaction.depth =4, shrinkage =0.2, verbose=F)
yhat.boost=predict (boost.college ,newdata =College[-train ,], n.trees=5000)
mean((yhat.boost - college.test)^2) #141021.2 worse fitting tree


#Principal Components Regression
install.packages("pls")
library(pls)
set.seed(2)
College <- College[,colSums(College != 0) != 0] 
pcr.fit = pcr(DEBT_MDN~., data = College, scale = TRUE, validation ="CV")
summary(pcr.fit)
