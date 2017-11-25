setwd("/Users/ryosuke/Google ドライブ/Seminar/DataMix/02_Basic Step/practical-stats-modeling-master-6481413f63e717d327f9e17a9e26079be9d8cc58")

#生データの読み込み
raw_data<-read.csv("homework_data/bank_marketing_train.csv")

#職業毎に違いを確認
png("homework_data/plot.png", width = 600, height = 400)
ggplot(data=raw_data, aes(x=age, fill=job)) + geom_histogram(alpha=0.4, position="identity",binwidth = 5,breaks=seq(0, 100, by=5))
dev.off()
#画像を出力するやつ
OutPicture <- function(data){
    colNamesJob <- unique(data$job)
    fileName_Pic <- "homework_data/plot2.png"
    #fileName_Pic <- "homework_data/plot_"+ as.character(colNamesJob[1]) + ".png"
    png(fileName_Pic, width = 600, height = 400)
    tmp_data<-data[data$job==colNamesJob[1],]
    ggplot(data=tmp_data, aes(x=age, fill=y)) + geom_histogram(alpha=0.4, position="identity",binwidth = 5,breaks=seq(0, 100, by=5))
    dev.off()
}
OutPicture(raw_data)
#colNamesJob <- 
unique(raw_data$job)
png("homework_data/plot_housemaid.png", width = 600, height = 400)
ggplot(data=raw_data[raw_data$job=="housemaid",], aes(x=age, fill=y)) + geom_histogram(alpha=0.4, position="identity",binwidth = 5,breaks=seq(0, 100, by=5))
dev.off()
ggplot(raw_data,aes(x=age,fill=y))+geom_density(alpha=0.2)

#重複なし
unique(raw_data$job)
#欠損値の確認
is.na(raw_data)
summary(raw_data)

require(ggplot2)
require(useful)

#年齢分布✕達せ
ggplot(raw_data,aes(x=age,fill=y))+geom_density(alpha=0.2)

library(reshape2)
ggplot(raw_data, aes(x = job)) + geom_bar(aes(fill = job))
ggplot(raw_data, aes(x = education)) + geom_bar(aes(fill = education))
student_data<-raw_data[raw_data$job=="student",]
boxplot(age~y, data=student_data)

raw_data$job<-as.factor(raw_data$job)
raw_data$marital<-as.factor(raw_data$marital)
raw_data$education<-as.factor(raw_data$education)
raw_data$default<-as.factor(raw_data$default)
raw_data$housing<-as.factor(raw_data$housing)
raw_data$loan<-as.factor(raw_data$loan)
raw_data$contact<-as.factor(raw_data$contact)
raw_data$month<-as.factor(raw_data$month)
raw_data$day_of_week<-as.factor(raw_data$day_of_week)
raw_data$poutcome<-as.factor(raw_data$poutcome)
raw_data$y<-ifelse(raw_data$y=="yes", 1, 0)
#class(raw_data$age)
#head(raw_data,10)
#
#raw_data<-raw_data[raw_data$job=="student",] #?????l?????ijob==unknown?̍폜?j
raw_data$age_level<-raw_data$age

Nenrei<-function(data,dev){
  level <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-110")
  #level <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99")
  count <- 1
  m <- length(level)
  n <- nrow(data)
  
  while( count <= m ){
    
    for( i in 1:n ){
      #print(sprintf("???ԁF%s - %s,?N???F%s",(count-1)*5,(count)*5,data$age[i]))
      if( data$age[i] >= (count-1)*dev && data$age[i] < count*dev ) {
        #print(sprintf("TRUE?F%s,%s,?N???F%s",i,data$age_level[i],level[count]))
        data$age_level[i] <- level[count]
        #print(data$age_level[i])
      }
    }
    count <- count + 1
  }
  return(data)
}
raw_data <- Nenrei(raw_data,5)
View(raw_data)
raw_data$age_level<-factor(raw_data$age_level)
raw_data<-raw_data[raw_data$age_level!="95-99",] #?????l????
raw_data<-raw_data[raw_data$default!="unknown",] #?????l?????ijob==unknown?̍폜?j

tmp_data<-raw_data
raw_data<-tmp_data
#raw_data<-raw_data[raw_data$month=="oct",] #?????l?????ijob==unknown?̍폜?j
#raw_data<-raw_data[raw_data$job!="student",]
#raw_data<-raw_data[raw_data$job=="retired",]
raw_data$new_feature <- raw_data$campaign / ( raw_data$previous + 0.0001) 
#?f?[?^?̕???
set.seed(1234) # ?R?[?h?̍Č??????ۂ???seed???Œ?
num_rows<-dim(raw_data)[1]
idx<-c(1:num_rows)
train_idx<-sample(idx,size=num_rows*0.7)
train_data<-raw_data[train_idx,]
test_data<-raw_data[-train_idx,]

#View(train_data)
#???W?X?e?B?b?N???A????
#data.lr<-glm(y~.-campaign-loan-nr.employed-month, data=train_data, family="binomial")
data.lr<-glm(y~age_level+campaign+poutcome+previous+euribor3m+day_of_week+education+new_feature, data=train_data, family="binomial")
summary(data.lr)
data2.lr<-step(data.lr)
data.lr <- data2.lr
summary(data.lr)

## ?I?b?Y???̌v?Z
exp(data.lr$coefficients)
require(coefplot)
coefplot(data.lr,intercept=F)

#student_data<-train_data[train_data$job=="student",]
#png("homework_data/plot1.png", width = 400, height = 400)
#boxplot(age~y,data=student_data,names=c("no","yes"),main="student",xlab="y", ylab="age")
#dev.off()
#boxplot(age~job,data=raw_data,names=c("no","yes"),main="all",xlab="y", ylab="age")
## F?????̑??????ɖޓx?䌟??
#null_data.lr<-glm(y~1, data=train_data, family="binomial")
#anova(data.lr, null_data.lr, test = "LRT")
## ?\??
#Log Odds???\??????
#ypred<-predict(data.lr, newdata = test_data, type="link")
#?m?????\??????
#test_data<-test_data[test_data$month!="oct",]
ypred<-predict(data.lr, newdata = test_data, type="response")
#?���?m?????t???O?ɕϊ????????ꍇ?́A臒l?����߂??K?v??????
test_data$predict<-ypred
#View(test_data)
real_yes <- test_data[test_data$y>0.8,]
real_no <- test_data[test_data$y<0.8,]
#?\?????z?̍쐬
test_data$y<-ifelse(test_data$y==1, "yes", "no")
ggplot(test_data,aes(x=predict,fill=y))+geom_density(alpha=0.2)+scale_x_continuous(breaks=seq(0,1,by=0.1),limits=c(0,1))


Target <- function(data,cost,income,dev,threshold){
  #ROI?ő剻
  ROI <- 0
  threshold_reserve <- threshold
  data$y<-ifelse(data$y=="yes", 1, 0)
  print("?T????")
  while( threshold < 0.3 ){
    
    ypred_flag<-ifelse(data$predict > threshold, 1, 0)
    conf_mat<-table(data$y, ypred_flag)
    #?v?Z
    tmp <- ( conf_mat[4] * income - ( conf_mat[3] + conf_mat[4] ) * cost )
    #print(conf_mat)
    #ROI?l?A臒l?̍X?V
    if( ROI < tmp ){
      ROI <- tmp
      threshold_reserve <- threshold
    }
    threshold <- threshold + dev
  }
  print(sprintf("ROI?F%s ,臒l?F%s",ROI,threshold_reserve))
  
  #???ʂ̕\??
  ypred_flag<-ifelse(data$predict> threshold_reserve, 1, 0)
  conf_mat<-table(data$y, ypred_flag)
  print(conf_mat)
  # ??????
  accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
  print(sprintf("accuracy:%5.3f",accuracy))
  # ?K????(precision)
  precision<-conf_mat[4] / (conf_mat[3] + conf_mat[4])
  print(sprintf("precision:%5.3f",precision))
  # ?Č???(Recall)
  recall<-conf_mat[4]/ (conf_mat[2] + conf_mat[4]) 
  print(sprintf("recall:%5.3f",recall))
  # F?l
  print(sprintf("F-score:%5.3f",2*precision*recall/(precision+recall)))
  #?????グ
  print(sprintf("Benefit:%5.3f",( conf_mat[4] * income - ( conf_mat[4] + conf_mat[3] ) * cost )))
  #
  print(sprintf("number:%5.3f",conf_mat[1]+conf_mat[2]+conf_mat[3]+conf_mat[4]))
  write.csv(conf_mat, "homework_data/cont_mat.csv", quote=FALSE, row.names=FALSE)
  return (threshold_reserve)
}
threshold <- Target(test_data,500,2000,0.001,0.05)
write.csv(test_data, "homework_data/output.csv", quote=FALSE, row.names=FALSE)


tmp_std_data<-raw_data[raw_data$job=="student",]
student_yes_data<-tmp_std_data[tmp_std_data$y=="yes",]
student_no_data<-tmp_std_data[tmp_std_data$y=="no",]
yes <- summary(student_yes_data)
no <- summary(student_no_data)
write.csv(yes, "homework_data/output_yes.csv", quote=FALSE, row.names=FALSE)
write.csv(yes, "homework_data/output_no.csv", quote=FALSE, row.names=FALSE)


ggplot(data=tmp_std_data, aes(x=education)) + geom_bar()+facet_grid(. ~ y)


#ここから予測
#データの読み込み
predict_data<-read.csv("homework_data/bank_marketing_test.csv")
#予測モデルに適用
Predict_New <- function(predict_data){
    predict_data$job<-as.factor(predict_data$job)
    predict_data$marital<-as.factor(predict_data$marital)
    predict_data$education<-as.factor(predict_data$education)
    predict_data$default<-as.factor(predict_data$default)
    predict_data$housing<-as.factor(predict_data$housing)
    predict_data$loan<-as.factor(predict_data$loan)
    predict_data$contact<-as.factor(predict_data$contact)
    predict_data$month<-as.factor(predict_data$month)
    predict_data$day_of_week<-as.factor(predict_data$day_of_week)
    predict_data$poutcome<-as.factor(predict_data$poutcome)
    predict_data$y<-ifelse(predict_data$y=="yes", 1, 0)
    predict_data$age_level<-predict_data$age
    predict_data <- Nenrei(predict_data,5)
    predict_data$age_level<-factor(predict_data$age_level)
    predict_data<-predict_data[predict_data$age_level!="95-99",] #?????l????
    predict_data<-predict_data[predict_data$default!="unknown",] #?????l?????ijob==unknown?̍폜?j
    predict_data$new_feature <- predict_data$campaign / ( predict_data$previous + 0.0001) 
    predict_data<-predict_data[predict_data$age_level!="90-94",]
    ypred<-predict(data.lr, newdata = predict_data, type="response")
    predict_data$predict<-ypred
    ypred_flag<-ifelse(predict_data$predict > threshold, 1, 0)
    predict_data$y_predict<-ypred_flag
    write.csv(predict_data, "homework_data/output_predict.csv", quote=FALSE, row.names=FALSE)
}

#結果の確認
Result_predict<-function(income,cost){
    conf_mat<-table(predict_data$y, ypred_flag)
    print(conf_mat)
    # ??????
    accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
    print(sprintf("accuracy:%5.3f",accuracy))
    # ?K????(precision)
    precision<-conf_mat[4] / (conf_mat[3] + conf_mat[4])
    print(sprintf("precision:%5.3f",precision))
    # ?Č???(Recall)
    recall<-conf_mat[4]/ (conf_mat[2] + conf_mat[4]) 
    print(sprintf("recall:%5.3f",recall))
    # F?l
    print(sprintf("F-score:%5.3f",2*precision*recall/(precision+recall)))
    #?????グ
    print(sprintf("Benefit:%5.3f",( conf_mat[4] * income - ( conf_mat[4] + conf_mat[3] ) * cost )))
    #
    print(sprintf("number:%5.3f",conf_mat[1]+conf_mat[2]+conf_mat[3]+conf_mat[4]))
    #test_data$y<-ifelse(test_data$y==1, "yes", "no")
}
Result_predict(2000,500)
