library(caret)
library(dplyr)
library(corrplot)
library(pscl)
library(randomForest)
library(pROC)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(scales)
library(naniar)
library(textstem)
library(lmtest)

library(crayon)
options(scipen=999)


library(tm)
library(SnowballC)
library(wordcloud)
library(psych)


setwd("C:\\prakash\\glim\\WMSA\\")
shark_set <- read.csv("Dataset.csv",header = TRUE,stringsAsFactors = FALSE)
names(shark_set)
round(prop.table(table(shark_set$deal)),2)
summary(shark_set)
str(shark_set)

#############EDA###################

#######UNIVARIATE PLOTS########


par(bg="light blue")

vis_miss(shark_set)

numcols<-Filter(is.numeric, shark_set)
summary(numcols)

askedcol <- shark_set$askedFor/100000
askedhist<-hist(askedcol,xlab = "Asked for (in lakhs)",col = "red",
      main=" Histogram for \"asked for\"",breaks=40)
xfit <- seq(min(askedcol), max(askedcol), length = 40) 
yfit <- dnorm(xfit, mean = mean(askedcol), sd = sd(askedcol)) 
yfit <- yfit * diff(askedhist$mids[1:2]) * length(askedcol) 
lines(xfit, yfit, col = "black", lwd = 2)

boxplot(askedcol,xlab = "Asked for(in lakhs)",col = "green",
        main=" Box plot for \"asked for\"",horizontal = TRUE)



valuation <- shark_set$valuation/100000
valuationhist<-hist((shark_set$valuation/100000),breaks=40,col="red",
                    xlab="Valuation (in lakhs)",main= "Histogram of Valuation")
xfit <- seq(min(valuation), max(valuation), length = 40) 
yfit <- dnorm(xfit, mean = mean(valuation), sd = sd(valuation)) 
yfit <- yfit * diff(valuationhist$mids[1:2]) * length(valuation) 
lines(xfit, yfit, col = "black", lwd = 2)


boxplot(valuation,xlab = "Valuation (in lakhs)",col = "green",
        main=" Box plot for \"Valuation\"",horizontal = TRUE)



stakeexchange <- shark_set$exchangeForStake
stakehist<-hist(shark_set$exchangeForStake,xlab = "ExchangeForStake",col = "red",
     main=" Histogram for \"Exchange For Stake\"",breaks=20)
xfit <- seq(min(stakeexchange), max(stakeexchange), length = 20) 
yfit <- dnorm(xfit, mean = mean(askedcol), sd = sd(stakeexchange)) 
yfit <- yfit * diff(stakehist$mids[1:2]) * length(stakeexchange) 
lines(xfit, yfit, col = "black", lwd = 2)


boxplot(stakeexchange,xlab = "exchangeForStake",col = "green",
        main=" Box plot for \"Exchange For Stake\"",horizontal = TRUE)

#######BIVARIATE PLOTS########


ggplot(shark_set,
       aes(x = valuation/100000, 
           y = askedFor/100000)) +
  geom_point(color= "green",cex=1.4) +
  geom_smooth(method = "lm",color="red")+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'light yellow'))+
  labs(y = "Asked (in lakhs)", x = "Valuation (in lakhs)", title = "Scatter plot of Valuation vs Asked")



ggplot(shark_set,
       aes(x = valuation/100000, 
           y = exchangeForStake)) +
  geom_point(color= "green",cex=1.4) +
  geom_smooth(method = "lm",color="red")+
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'light yellow'))+
  labs(y = "Stake", x = "Valuation (in lakhs)", title = "Scatter plot of Valuation vs Stake")


ggplot(shark_set,
       aes(x = askedFor/100000, 
           y = exchangeForStake)) +
  geom_point(color= "green",cex=1.4) +
  geom_smooth(method = "lm",color="red")+
  labs(y = "Stake", x = "Asked For (in lakhs)", title = "Scatter plot of Asked vs Stake")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'light yellow'))
 


#CORRELATION

cordatanum<-dplyr::select(shark_set, c(exchangeForStake,valuation,askedFor))
corval<-round(cor(cordatanum),4)
corval
corrplot(corval,method=c("number"),bg="light blue")


#BOX PLOTS

boxplot(shark_set$valuation/100000~shark_set$deal,xlab = "Deal",ylab="Valuation",
        col = "green", main="Box plot for Valuation on deals")
boxplot(shark_set$exchangeForStake~shark_set$deal,xlab = "Deal",ylab="Stake",
        col = "green", main=" Box plot for Stake on deals"   )
boxplot(shark_set$askedFor/100000~shark_set$deal,xlab = "deal",ylab="Asked For",
        col = "green", main=" Box plot of Asked For on deals")


#STACKED BARS AND DENSITY PLOT

sharkdataforseason <- shark_set %>%
  group_by(season, deal) %>%
  summarize(n = n()) %>% 
  mutate(percent = n/sum(n),
         lbl = scales::percent(percent))

ggplot(sharkdataforseason, aes(x = season,y=percent,fill=deal)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percent", 
     fill = "Deals",
     x = "Seasons",
     title = "Deal conversion per season")+
  theme(panel.background = element_rect(fill = 'light yellow'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))



ggplot(shark_set, 
       aes(x = valuation/100000, 
           fill = deal)) +
  geom_density(alpha = 0.8) +
  labs(title = "Density plot of deal conversion on the valuation amount")+
  theme(panel.background = element_rect(fill = 'light yellow'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))



df_shark=data.frame()
for (x in 1:5) {
  sk=data.frame()
  sk<-paste("df",x,sep="")
  
  if (x==1) {
    sk<-shark_set %>%
      group_by(shark1,deal) %>%
      summarise(count=n())
  } else if (x==2) {
     sk<-shark_set %>%
      group_by(shark2,deal) %>%
      summarise(count=n())
  } else if (x==3) {
     sk<-shark_set %>%
      group_by(shark3,deal) %>%
      summarise(count=n())   
  } else if (x==4) {
    sk<-shark_set %>%
      group_by(shark4,deal) %>%
      summarise(count=n())
  } else if (x==5) {
    sk<-shark_set %>%
      group_by(shark5,deal) %>%
      summarise(count=n())
   
  }
  sk<-as.data.frame(sk)
  names(sk)[names(sk) == paste("shark",x,sep="")] <- "shark"
  df_shark<-rbind(df_shark,sk)
  
 }
  

finalcountspershark<-df_shark%>%group_by(shark,deal)%>%summarise(n=sum(count))%>%
  mutate(percentofdeals=n/sum(n),lbl=scales::percent(percentofdeals))

finalcountspershark<-as.data.frame(finalcountspershark)

ggplot(finalcountspershark, aes(x = shark,y=percentofdeals,fill = deal)) + 
  geom_bar(stat="identity",position = "fill")+
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "Percent", 
     fill = "Deals",
     x = "Sharks",
     title = "Deal conversion per shark across seasons")+
  theme(panel.background = element_rect(fill = 'light yellow'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))



##################MODEL######################
CorpusShark = Corpus(VectorSource(shark_set$description))
CorpusShark=tm_map(CorpusShark,tolower)
CorpusShark=tm_map(CorpusShark,removePunctuation)
CorpusShark=tm_map(CorpusShark,removeWords,c(stopwords("en")))

#CorpusShark=tm_map(CorpusShark,stemDocument)  # not stemming as changes the meaning of some words

frequenciesShark = DocumentTermMatrix(CorpusShark)
corpus_dtm <- DocumentTermMatrix(CorpusShark)
corpus_matrix <- as.matrix(corpus_dtm)
wordcount <- sort(colSums(corpus_matrix),decreasing=TRUE)
final_lst <- data.frame(word = names(wordcount),freq=wordcount)


###################
#WORDCLOUD

wordcloud(CorpusShark,colors=brewer.pal(max(6,ncol(corpus_matrix)),"Dark2"),min.freq = 5,
          max.words = 300,random.order=FALSE) #gives the top 300 occurring words 

##################


CorpusShark=tm_map(CorpusShark,removeWords,c("can","also"))
CorpusShark=tm_map(CorpusShark,lemmatize_strings)


frequenciesShark = DocumentTermMatrix(CorpusShark)
frequenciesShark
#10/1000 = 0.010 - removes 10 occurence words across 1000 words
SharkSparse = removeSparseTerms(frequenciesShark,0.990)
SharkSparse


SharkSparse = as.data.frame(as.matrix(SharkSparse))
SharkSparse$deal = shark_set$deal

contains_any_na = sapply(SharkSparse, function(x) any(is.na(x)))
names(SharkSparse)[contains_any_na]
any(duplicated(names(SharkSparse)))

SharkSparse=as.data.frame(SharkSparse)
SharkSparse$deal=as.factor(SharkSparse$deal)
dim(SharkSparse)


createdatas <- function(SharkSparse){
  set.seed(12345)
  train.index <- createDataPartition(SharkSparse$deal, p = .7, list = FALSE)
  train <- as.list(SharkSparse[train.index,])
  test  <- as.list(SharkSparse[-train.index,])
  
  print(round(prop.table(table(train$deal)),2))
  print(round(prop.table(table(test$deal)),2))
  splitdatas=list(train,test)
  
  return(splitdatas)
  
}


########### LOGISTIC REGRESSION ###############

logregfunction<-function(datasets) {
  
  set.seed(10)
  lrtrain1<-data.frame(datasets[1])
  lrtest1<-data.frame(datasets[2])
  
  lrtrain1<<-data.frame(lrtrain1,check.names=TRUE)
  lrtest1<<-data.frame(lrtest1,check.names=TRUE)
  
  
  ## creating a validation dataset to validate the model
  train.index1 <- createDataPartition(lrtrain1$deal, p = .8, list = FALSE)
  modeltrain <<- as.list(lrtrain1[train.index1,])
  validatetest  <<- as.list(lrtrain1[-train.index1,])
  
  modeltrain<<-data.frame(modeltrain)
  validatetest<<-data.frame(validatetest)
  
  print(round(prop.table(table(modeltrain$deal)),2))
  print(round(prop.table(table(validatetest$deal)),2))
  print(dim(modeltrain))
  print(dim(validatetest))
  
  lr_model1 <- glm(modeltrain$deal ~ .,data = modeltrain,family=binomial())
  summary(lr_model1)
  names(lr_model1$coefficients)
  
  print(pR2(lr_model1))
  print(lrtest(lr_model1))
  
  
  ## CONFUSION MATRIX with TRUE and FALSE 
  pdata <- predict(lr_model1,validatetest, type = "response")
  confmat1 = table(Predicted=ifelse(floor(pdata+0.5)==1,TRUE,FALSE),
                   Actual=validatetest$deal)
  print(confusionMatrix(confmat1,positive="TRUE",mode="everything"))
  
  
  ## Creating the model with the full training dataset
  lr_finalmodel <- glm(lrtrain1$deal ~ .,data = lrtrain1,family=binomial())
  print(summary(lr_finalmodel))
  print(pR2(lr_finalmodel))
  
  
  ## Predict for test using the above final model
  tdata <- predict(lr_finalmodel,lrtest1, type="response")
  t_confmat = table(Predicted=ifelse(floor(tdata+0.5)==1,TRUE,FALSE),Actual=lrtest1$deal)
  print(confusionMatrix(t_confmat,positive="TRUE",mode="everything"))
  
  
  roc_lr=roc(lrtest1$deal,tdata)
  plot(roc_lr, col="red", lwd=3, main="ROC curve Logistic Regression")
  print(auc(roc_lr))
  
  print("Logistic Regression executed")
  
}



###################### CART ###################

cartfunction<-function(datasets) {

  set.seed(5555)
  
  crtrain=as.data.frame(datasets[1])
  crtest=as.data.frame(datasets[2])
  
  print(dim(crtrain))
  print(dim(crtest))
  
  cart.ctrls <- rpart.control(minsplit=40, minbucket=12 , cp = 0, xval = 5)
  
  
  carttrainmodel <- rpart(formula = deal ~. ,
                          data = crtrain, method = "class", control = cart.ctrls)
  
  if(any(names(crtrain) == 'ratio')){
    pdf("carttree_ratio.pdf") }
  else{  
    pdf("carttree_original.pdf")
  }  
  rpart.plot(carttrainmodel)
  dev.off()
  
  
  printcp(carttrainmodel)
  plotcp(carttrainmodel)
  print(carttrainmodel$variable.importance)
  
  crtrain$predict.class <- predict(carttrainmodel, crtrain, type="class")
  crtrain$predict.score <- predict(carttrainmodel, crtrain)
  
  confmattrain = table(crtrain[,c("deal","predict.class")])
  print(confusionMatrix(confmattrain,positive="TRUE"))
  
  
  # CART test
  crtest$predict.class <- predict(carttrainmodel,crtest, type="class")
  crtest$predict.score <- predict(carttrainmodel,crtest)
  
  confmattest = table(crtest[,c("deal","predict.class")])
  print(confusionMatrix(confmattest,positive="TRUE"))
  
  # ROC curve
  roc_cart=roc(crtest$deal,crtest$predict.score[,2])
  plot(roc_cart, col="red", lwd=3, main="ROC curve CART")
  print(auc(roc_cart))
  
  
  print("CART executed")
  
}



###################### RANDOM FOREST ##########

randomforestfunction <- function(datasets) {
  
  rftrain=as.data.frame(datasets[1])
  rftest=as.data.frame(datasets[2])
  
  rftrain<-data.frame(rftrain,check.names=TRUE)
  rftest<-data.frame(rftest,check.names=TRUE)
  print(dim(rftest))
  #mtry <- round(sqrt(ncol(rftrain)))
  
  rftrain.index1 <- createDataPartition(rftrain$deal, p = .8, list = FALSE)
  rfmodeltrain <- as.list(rftrain[rftrain.index1,])
  rfvalidatetest  <- as.list(rftrain[-rftrain.index1,])
  
  rfmodeltrain<-data.frame(rfmodeltrain)
  rfvalidatetest<-data.frame(rfvalidatetest)
  
  ##### Using mtry = 20  for the RF MODEL ############
  
  set.seed(9999)
  rf_premodel <- randomForest(rfmodeltrain$deal ~ ., data=rfmodeltrain, 
                     ntree=300, mtry = 20, nodesize = 15, importance=TRUE)
  
  print(rf_premodel)
  
  plot(rf_premodel, main="")
  legend("topright", c("OOB", "0", "1"), text.col=1:6, 
         lty=1:10, text.font=4,bg="lightblue",col=1:3,horiz = TRUE)
  title(main="Error Rates Random Forest Train")
  
  
  
  ############Tuning the Model and returning best mtry############
  
  set.seed(9999)
  tune_rf <- tuneRF(x=dplyr::select(rfmodeltrain, -(deal)), 
                y=rfmodeltrain$deal,
                mtryStart = 20, 
                ntreeTry=300, 
                stepFactor = 1.5, 
                improve = 0.00001, 
                trace=TRUE, 
                plot = TRUE,
                doBest = FALSE,
                nodesize = 15, 
                importance=TRUE
  )
  
  
  set.seed(9999)
  rf_model1 <- randomForest(rfmodeltrain$deal ~ ., data=rfmodeltrain, 
                                ntree=300, mtry = 20, nodesize = 15, importance=TRUE)
  print(rf_model1)
  
  
  
  ###### validate the model using the validation set
  
  rfvalidatetest$predict.class <- predict(rf_model1, rfvalidatetest, type="response")
  confmat_rf = table(Predicted=rfvalidatetest$predict.class,Actual=rfvalidatetest$deal)
  print(confusionMatrix(confmat_rf,positive="TRUE",mode="everything"))
  
  
  
  ####### Creating the final model to predict ###########
  
  
  set.seed(9999)
  rf_finalmodel <- randomForest(rftrain$deal ~ ., data=rftrain, 
                           ntree=300, mtry = 20, nodesize = 15, importance=TRUE)
  
  print(rf_finalmodel)
  
  varImpPlot(rf_finalmodel,sort=TRUE,n.var=30,scale=TRUE,
             bg="blue",main="Variable Importance Plot")
  
  
  ## Predicting for the test dataset
  rftest$predict.class <- predict(rf_finalmodel,rftest,type="response")
  confmat_rftest <- table(Predicted=rftest$predict.class,Actual=rftest$deal)
  print(confusionMatrix(confmat_rftest,positive="TRUE",mode="everything"))
  
  # ROC curve
  rftest$predict.score <- predict(rf_finalmodel,rftest,type="prob")
  roc_rf=roc(rftest$deal,rftest$predict.score[,2])
  plot(roc_rf, col="red", lwd=3, main="ROC curve CART")
  print(auc(roc_rf))
  
  print("Random Forest executed")
}
  

###########################################################


SharkSparse_ratio<-SharkSparse
ratio<-round(shark_set$askedFor/shark_set$valuation,2)
SharkSparse_ratio$ratio <- ratio


cat(green$bold("Going to create the train and test data sets for the 2 datasets"))
datasets<-createdatas(SharkSparse)
datasets1<-createdatas(SharkSparse_ratio)
cat(green$bold("Train and test created for both datasets"))


cat(green$bold("Going to start Logistic Regression"))
logregfunction(datasets)
cat(green$bold("Logistic Regression completed for first dataset"))
logregfunction(datasets1)
cat(green$bold("Logistic Regression completed for both datasets"))

cat(green$bold("Going to start CART"))
cartfunction(datasets)
cat(green$bold("CART completed for first dataset"))
cartfunction(datasets1)
cat(green$bold("CART completed for both datasets"))

cat(green$bold("Going to start Random Forest"))
randomforestfunction(datasets)
cat(green$bold("Random Forest completed for first dataset"))
randomforestfunction(datasets1)
cat(green$bold("Random Forest completed for both datasets. Execution Completed."))





