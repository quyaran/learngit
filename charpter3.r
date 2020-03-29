setwd("I:\\OneDrive - sxufe.edu.cn\\大数据统计方法\\data")
df=read.csv("simple4.csv")

#1、根节点（1号节点，全部12个观测值：7个F及5个M）拆分变量的竞争
table(df[,c(1,2)])
table(df[,c(1,3)])
table(df[,c(1,4)])

#2、左下节点（命名2号节点，全部5个观测值：4个F及1个M）拆分变量的竞争
table(df[df[,4]=="y",][,c(1,2)])
table(df[df[,4]=="y",][,c(1,3)])
table(df[df[,4]=="n",][,c(1,2)])
table(df[df[,4]=="n",][,c(1,3)])


library(rpart.plot)  #同时自动装入主要程序包rpart
a=rpart(sex~.,df,minsplit=2,model = TRUE) #产生决策树
rpart.plot(a,extra = 1) #画图
a #输出放在a中的决策树细节

#预测值，拟合值，误判率(预判精度)
predict(a,df,type = "class")

table(df$sex,predict(a,df,type = "class")) #混淆矩阵
(miss=sum(df$sex!=predict(a,df,type = "class")))#误判个数
miss/nrow(df)#误判率nrow(df)是数据df的行数(样本量)

df1=read.csv("new4.csv")
table(df1$sex,predict(a,df1,type = "class")) #混淆矩阵
sum(df1$sex!=predict(a,df1,type = "class"))/nrow(df1)


#5.2
library(rpart.plot)
library(lattice)
(a=rpart(voice.part~height,singer))
rpart.plot(a,type=2,extra=2)
mean(singer[,2]!=predict(a,singer,type = "class"))#训练集误判率


#训练简单最小二乘线性回归模型的例子
library(ggplot2)
ggplot(cars,aes(x=speed,y=dist))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)

(a=lm(dist~speed,cars))#lm为线性模型（linear model）的缩写
mean((cars$dist-a$fitted.values)^2)  #MSE

#分类情况
Fold=function(Z=10, w, D, seed=7777){
  n=nrow(w); d=1:n; e=levels(w[,D]);
  N=length(e)#目标变量的水平个数
  set.seed(seed)
  dd=lapply(1:N,function(i){
    d0=d[w[,D]==e[i]]; j=length(d0)
    ZT=rep(1:Z,ceiling(j/Z))[1:j]
    id=cbind(sample(ZT),d0);id})
  #上面每个dd[[i]]是随机1:Z及i类的下标集组成的矩阵
  mm=lapply(1:Z,
            function(i){u=NULL;for(j in 1:N)
            u=c(u,dd[[j]][dd[[j]][,1]==i,2]);u})
  return(mm)
}
  

library(tidyr)
Z=5
mm=Fold(5,singer,2)
pred=singer[,2]
for(i in 1:Z){
  pred[mm[[i]]] = rpart(voice.part~height,singer[-mm[[i]],])%>%
  predict(singer[mm[[i]],],type="class")
}
mean(pred!=singer[,2])


#回归情况
n=nrow(cars);Z=3;set.seed(1010)
I=sample(rep(1:Z, ceiling(n/Z)))[1:n]
mm=lapply(1:Z,function(i){(1:n)[I==i]})
mm

library(tidyr)
pd=rep(9999,n)
for (i in 1:Z)
  pd[mm[[i]]]=lm(dist~speed,cars[-mm[[i]],]) %>% predict(cars[mm[[i]],])
NMSE=sum((cars$dist-pd)^2)/sum((cars$dist-mean(cars$dist))^2)
NMSE


#最小二乘线性回归
w=read.csv("commun123.csv")
n=nrow(w)
Pr=data.frame(rf=rep(0,n),lm=rep(0,n))
Z=10;n=nrow(w);set.seed(1010)
I=sample(rep(1:Z,ceiling(n/Z)))[1:n]
library(randomForest)
for(i in 1:Z){
  Pr$rf[I==i]=randomForest(ViolentCrimesPerPop~.,w[I!=i,])%>%predict(w[I==i,])
  Pr$lm[I==i]=lm(ViolentCrimesPerPop~., w[I!=i,],maxit=1000,size=8)%>%predict(w[I==i,])
}
RSS=sum((w[,123]-mean(w[,123]))^2)
sum((w[,123]-Pr$rf)^2)/RSS
sum((w[,123]-Pr$lm)^2)/RSS


w=read.csv("commun123.csv")
nm=names(w)
fo=list()
for (i in 1:122) fo[[i]]=formula(paste(nm[123],"~", nm[i], "-1"))
sbeta=vector()
for(i in 1:122) sbeta[i]=lm(fo[[i]],w)$coef
mbeta=lm(ViolentCrimesPerPop~.-1, w)$coef
b=data.frame(sbeta, mbeta)
row.names(b)=nm[-1]

barplot(t(b), beside=T, col=1:2,las=2,cex.names=.3)
title("Coefficient comparison between multiple and univariate regression without constant term")
legend("topleft", c("Coefficients of univariate regressions", "Coefficients of multiple regressions"),fill=c("black","red"))


#7.2
w=read.csv("dataR2.csv");w[,10]=factor(w[,10])
Z=10;D=10;n=nrow(w)
mm=Fold(Z,w,D)

pred=rep(0,n)  #在数据中增加一列准备放预测的值
for (i in 1:Z) {
  pred[mm[[i]]]=glm(Classification~.,w[-mm[[i]],],family=binomial) %>%
  predict(w[mm[[i]],],type="response")
}

table(w$Classification,pred>0.5)#如果用0.5分割

#二分类问题的ROC曲线
library(ROCR)
par(mfrow=c(1,2),mar=c(4,4,3,2))
ROCRpred<-prediction(pred,w$Classification)
ROCRperf<-performance(ROCRpred,'tpr','fpr')
plot(ROCRperf,colorize=TRUE, text.adj=c(-0.2,1.7))
abline(0,1,lty=2)
title("ROC curve")
plot(performance(ROCRpred,"acc"))
title("Accuracy - Cutoffs plot")

auc <-performance(ROCRpred,measure = "auc")
auc@y.values[[1]]

table(w$Classification,pred>0.56)

#第八章决策树及其组合方法
#8.1决策树
library(rpart.plot)
w=read.csv("Sports.csv")
b=rpart(Label~.,w)
rpart.plot(b,extra=1)

library(tidyverse)
sort(unique(w$LS))

gip=function(y,x,p){#计算自变量观测值在分割点p的Gini不纯度
  pr=prop.table(table(x>=p))
  p1=prop.table(table(y[x<p]))
  p2=prop.table(table(y[x>=p]))
  return(c(1-sum(p1^2),1-sum(p2^2))%*%pr)
}

d=sort(unique(w$LS))#变量w$LS的不重复观测值
                    
g=NULL
for (i in d[-1]){  #计算43个分割对应的Gini不纯度
  g=c(g,gip(w$Label,w$LS,i))
}

plot(d[-1],g, col=4,pch=16,
xlab=expression(paste(LS<=x," vs ",LS>x)),
ylab="Gini impurity",
main="Gini impurity for 43 split points of covariate LS")


v=w[,-c(17,40)]#去掉全为0的两个变量的数据
G=NULL
for(i in 2:ncol(v)){
  d=sort(unique(v[,i]))
  g=NULL
  for(j in d[-1]){
    g=c(g,gip(v$Label,v[,i],j))
  }
    G=c(G,min(g))
}
#绘图:
barplot(G,names.arg=names(v)[-1],horiz=TRUE,las=1,cex.names=.4,
        xlab="Gini impurity", col=4)
title("Gini impurity for all covariate")
    
b=rpart(Label~.,w)
printcp(b)

b$cptable[which.min(b$cptable[,"xerror"]),"CP"]

ptree<-prune(b,cp=b$cptable[which.min(b$cptable[,"xerror"]),"CP"])
rpart.plot(ptree,extra = 1)

#决策树分类的混淆矩阵
table(w$Label,predict(ptree,w,type = "class"))
mean(w$Label!=predict(ptree,w,type = "class"))


#决策树分类的交叉验证
library(magrittr)
Z=10; D=1;n=nrow(w)
mm=Fold(Z,w,D)
pr=data.frame(cp10=w$Label,cp109=w$Label)#准备放预测的值
for(i in 1:Z){
  pr$cp10[mm[[i]]]=rpart(Label~.,w[-mm[[i]],],cp=0.01)%>%
    predict(w[mm[[i]],],type="class")
  pr$cp109[mm[[i]]]=rpart(Label~.,w[-mm[[i]],],cp=0.0109589)%>%
    predict(w[mm[[i]],],type="class")        
}    
mean(pr$cp10!=w$Label)#0.192
mean(pr$cp109!=w$Label)#.189

#分类的ROC曲线
w=read.csv("Sports.csv")
library(rpart)
library(magrittr)
Z=10; D=1;n=nrow(w)
mm=Fold(Z,w,D)
p=rep(0,n)

for(i in 1:Z){
  p[mm[[i]]] =rpart(Label~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],],type="prob")%>%
    .[,2]
}  

library(ROCR)
par(mfrow=c(1,2), mar=c(4,4,3,2))
ROCRpred <-prediction(p, w$Label)
ROCRperf<-performance(ROCRpred,'tpr','fpr')
plot(ROCRperf, colorize=TRUE,text.adj=c(-0.2,1.7))
abline(0,1,lty =2)
title("ROC curve")
plot(performance(ROCRpred, "acc"))
title("Accuracy -Cutoffs plot")

auc <-performance(ROCRpred,measure = "auc")
auc@y.values[[1]]

#决策树回归
library(rpart.plot)
w=read.csv("commun123.csv")
a=rpart(ViolentCrimesPerPop~.,w)
rpart.plot(a,extra=1)
a

#8.2.2经验分布
library(tidyr)
x=c(1,2,2,3,7,10,4,4,4)
sample(x,100000,rep=TRUE)%>%table()%>%prop.table()%>%barplot()

#8.2.3OOB数据
x=0:9;set.seed(100)
for (i in 1:10) {
  s=sample(x,10,replace = TRUE);oob=setdiff(x,s)
  cat(paste0("sample ", i,":"), s,"oob:",oob,"\n") #cat是一种输出格式
}

#8.2.4非等权放回再抽样
x=c(2,2,2,9,9,7,7,7,6,6,6,7,9,9,3)
set.seed(1111)
sample(x,15,rep=T) #等概率抽样
pr=c(rep(1,14),10) #设定x中3被抽中的概率为前14个数目每个被抽中概率的10倍
sample(x,rep=T,prob = pr) #增加3被抽中的概率

#8.3.2Bagging分类
library(ipred)
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])#把除年龄之外的哑元变量因子化
a=bagging(V35~.,w)#主程序
table(w$V35,predict(a,w))#混淆矩阵


library(tidyr)
Z=10;D=35;n=nrow(w)
mm=Fold(Z,w,D)
Pr=data.frame(bag=w$V35,tree=w$V35)
for(i in 1:Z) {
  Pr$bag[mm[[i]]]=bagging(V35~.,w[-mm[[i]],])%>% #I!=i的训练集建模
    predict(w[mm[[i]],])   #I=i的训练集作预测
  Pr$tree[mm[[i]]]=rpart(V35~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],],type="class")
}

table(w$V35,Pr$bag)
mean(Pr$bag!=w$V35)
table(w$V35,Pr$tree)
mean(Pr$tree!=w$V35)

#8.3.2Bagging回归
w=read.csv("commun123.csv");n=nrow(w)
Pr=data.frame(bag=rep(0,n),tree=rep(0,n))
Z=10;set.seed(1010)
I=sample(rep(1:Z,ceiling(n/Z)))[1:n]
for (i in 1:Z) {
  Pr$bag[I==i]=
    bagging(ViolentCrimesPerPop~.,w[I!=i,])%>%
    predict(w[I==i,])
  Pr$tree[I==i]=
    rpart(ViolentCrimesPerPop~.,w[I!=i,])%>%
    predict(w[I==i,])
}
sum((Pr$bag-w[,123])^2)/sum((w[,123]-mean(w[,123]))^2)
sum((Pr$tree-w[,123])^2)/sum((w[,123]-mean(w[,123]))^2)

#8.4.1随机森林分类
library(randomForest)
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34])w[,i]=factor(w[,i]) 
a=randomForest(V35~.,w,localImp=TRUE,proximity=TRUE)
a
#OOB交叉验证
layout(matrix(c(1:6,rep(7,3),rep(8,3)),2,6,by=T))
for(i in 1:8) {
  if(i>6) ca=.5 else ca=.3
  barplot(a$importance[,i],
          main = colnames(a$importance)[i],
          las=2,cex.names = ca)
}

#随机森林分类的变量局部重要性
matplot(1:34,a$localImportance,
        type = "l",las=2,main="Local Importance",
        cex.axis=.7,at=1:34,labels=names(w)[-35])

#随机森林分类的变量部分依赖性
par(mfcol=c(3,6))
dl=levels(w$V35)
for (j in dl) {
  partialPlot(a,pred.data=w[w[,35]==j,],x.var=V20,
              main = paste0("V35=",j," on V20"))
  partialPlot(a,pred.data=w[w[,35]==j,],x.var=V34,
              main = paste0("V35=",j," on V34"))
  partialPlot(a,pred.data=w[w[,35]==j,],x.var=V11,
              main = paste0("V35=",j," on V11"))
}

#随机森林分类的离群点
plot(outlier(a$proximity),type="h",main = "Outlying measure")

#8.4.2随机森林回归
w=read.csv("commun123.csv")
library(randomForest)
a=randomForest(ViolentCrimesPerPop~.,w,localImp=TRUE,proximity=TRUE)

mean((w[,123]-a$predicted)^2)/mean((w[,123]-mean(w[,123]))^2)

#随机森林回归的变量重要性
par(mfrow=c(2,1))
for(i in 1:2) {
  barplot(a$importance[,i],
  main = colnames(a$importance)[i],
  las=2,cex.names =.1,horiz = TRUE,cex.axis = .3)
}

#随机森林回归的变量局部重要性
matplot(1:122,a$localImportance,
        type = "l",las=2,main="Local Importance",
        cex.axis=.2,at=1:122,labels=names(w)[-123])
#随机森林回归的变量部分依赖性
par(mfcol=c(1,2),mar=c(2,3,3,1))
partialPlot(a,pred.data=w,x.var=PctKids2Par)
partialPlot(a,pred.data=w,x.var=pctWFarmSelf)



#8.4.3
#最小深度分布
library(randomForestExplainer)
library(randomForest)
set.seed(1010)
a=randomForest(ViolentCrimesPerPop~.,w,localImp=TRUE,proximity=TRUE)
min_depth_frame<-min_depth_distribution(a)
min_depth_frame %>% head(n=10)

plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees")

#各种变量重要性信息
importance_frame<-measure_importance(a)
importance_frame %>% head(n=10)

plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

#各种重要性的成对散点图及重要性排序的成对散点图
plot_importance_ggpairs(importance_frame)
plot_importance_rankings(importance_frame)


#自变量交互作用
vars<-important_variables(importance_frame,k=5,
                          measures = c("mean_min_depth","no_of_trees"))
interactions_frame<-min_depth_interactions(a,vars)
interactions_frame%>%head()

plot_min_depth_interactions(interactions_frame)

#8.5AdaBoost
library(adabag)
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
  a=boosting(V35~.,w)
predict(a,w)$confusion

a.cv<-boosting.cv(V35~.,v=10,data=w,mfinal = 100,
                  control = rpart.control(cp=0.01))
a.cv$confusion;a.cv$error

importanceplot(a,cex.names=.7,las=2)


#支持向量机
#9.1
library(kernlab)
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
set.seed(1010)
a=ksvm(V35~.,w,cross=10)
a@error #基于训练集的误判率
a@cross #10折交叉验证的误判率

#支持向量机回归
library(e1071)
w=read.csv("commun123.csv")
set.seed(1010)
b=svm(ViolentCrimesPerPop~.,w,cros=10)

sum((w[,123]-b$fitted)^2)/sum((w[,123]-mean(w[,123]))^2)
b$tot.MSE/mean((w[,123]-mean(w[,123]))^2)



#10.1人工神经网络
library(nnet)
w=read.csv("commun123.csv")
set.seed(1010)
sel=c(4,44,45,50,51,20,30)#选取7个自变量
w1=w[,c(sel,123)]#因变量是第123个
a=nnet(ViolentCrimesPerPop~.,data=w1,method="nnet",
       maxit=1000,size=5,decay=0.01,trace=F)
library(NeuralNetTools)
plotnet(a,pad_x=.7)

u=read.csv("derm.csv")[,c(1:5,35)]
for (i in 1:ncol(u)) u[,i]=factor(u[,i])
set.seed(1010)
b=nnet(V35~.,data=u,method="nnet",
       maxit=1000,size=10,decay=0.01,trace=F)
plotnet(b,pad_x = 1,circle_cex = 3,cex_val = .5)

#10.2神经网络分类
library(e1071)
library(magrittr)
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
tune.model=tune.nnet(V35~.,data=w,size=1:9)
summary(tune.model)

Z=10;D=35;mm=Fold(Z,w,D)
Pr=data.frame(rf=w$V35,net=w$V35)
for (i in 1:Z) {
  Pr$rf[mm[[i]]]=
    randomForest(V35~.,w[-mm[[i]],]) %>% predict(w[mm[[i]],])
  Pr$net[mm[[i]]]=
    a=nnet(V35~.,w[-mm[[i]],],method="nnet",
           maxit=100,size=9,trace=F) %>%
    predict(w[mm[[i]],],type="class")
  
}

table(w$V35,Pr$rf);mean(w$V35!=Pr$rf)
table(w$V35,Pr$net);mean(w$V35!=Pr$net)

#10.3神经网络回归
w=read.csv("BostonHousing2.csv")
s.w=lapply(w[,-(1:5)], function(x){(x-min(x))/(max(x)-min(x))})%>%as.data.frame()

library(neuralnet)
nm<-names(s.w)
f<-as.formula(paste("cmedv ~",paste(nm[!nm %in% "cmedv"], collapse = " + ")))
res<-neuralnet(f,data=s.w,hidden=c(5,3),linear.output=T)
plot(res)


library(randomForest)
n=nrow(s.w)
Pr=data.frame(rf=rep(0,n),net=rep(0,n))
Z=10;set.seed(1010)
I=sample(rep(1:Z,ceiling(n/Z)))[1:n]
for (i in 1:Z) {
  Pr$rf[I==i]=randomForest(f,w[I!=i,-(1:5)])%>%
    predict(w[I==i,-(1:5)])
  nn=neuralnet(f,data=s.w[I!=i,],hidden = c(5,3),linear.output = T)
  Pr$net[I==i]=compute(nn,s.w[I==i,-1])$net.result
}
Pr$net=Pr$net*(max(w$cmedv)-min(w$cmedv))+min(w$cmedv)#变换原先的尺度
RSS=sum((w$cmedv-mean(w$cmedv))^2)
sum((w$cmedv-Pr$rf)^2)/RSS
sum((w$cmedv-Pr$net)^2)/RSS


#11.2朴素贝叶斯方法分类
w=read.csv("derm.csv")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
library(e1071);library(randomForest)

Z=10;D=35
mm=Fold(Z,w,D)
Pr=data.frame(rf=w$V35,NB=w$V35)
for (i in 1:Z) {
  Pr$rf[mm[[i]]]=
    randomForest(V35~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
  Pr$NB[mm[[i]]]=
    a=naiveBayes(V35~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
}
table(w$V35,Pr$rf);mean(w$V35!=Pr$rf)
table(w$V35,Pr$NB);mean(w$V35!=Pr$NB)

#K最邻近分类
w=read.csv("SCADI.csv")
for (i in (1:ncol(w))[-2]) w[,i]=factor(w[,i])
ss=sapply(w, function(x)length(unique(x)))
id=which(ss==1)
length(id) #63
v=w[,-id]

library(kknn)
a=kknn(Classes~.,train=v,test=v)

table(v$Classes,a$fitted.values)
mean(v$Classes!=a$fitted.values)

set.seed(1010)
cv.kn=cv.kknn(Classes~.,v[,-id],kcv=10)
mean(cv.kn[[1]][,1]!=cv.kn[[1]][,2])

library(randomForest)
set.seed(1010)
(rf=randomForest(Classes~.,v))#OOB交叉验证误判率
sum(v$Classes!=predict(rf,v))#训练集误判率


#12.1.2K最邻近回归
w=read.csv("commun123.csv")
Z=10;n=nrow(w);set.seed(1010)
I=sample(rep(1:Z,ceiling(n/Z)))[1:n]
pred=rep(0,n)
for (i in 1:Z) {
  pred[I==i]=
  kknn(ViolentCrimesPerPop~.,w[I!=i,],w[I==i,])$fit
}
sum((w[,123]-pred)^2)/sum((w[,123]-mean(w[,123]))^2)

#13.1多分类问题例子
library(mlbench)
library(adabag) ;library(ipred); library(kknn)
library(e1071); library(randomForest)
data(DNA)#3186 181 names(w) [181]
w=DNA
Z=10;D=181;n=nrow(w)
mm=Fold(Z, w, D, 1010)
kn=w[,181]->prf->NB->bag->svmc->ada#在数据中增加一列准备放预测的值
set.seed(1010)
for(i in 1:Z) {
  kn[mm[[i]]]=kknn(Class~.,w[-mm[[i]],],w[mm[[i]],])$fit
  prf[mm[[i]]]=randomForest(Class~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
  svmc[mm[[i]]]=svm(Class~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
  bag[mm[[i]]]=ipred:: bagging(Class~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
  a=boosting(Class~.,w[-mm[[i]],])
  ada[mm[[1]]]=predict(a,w[mm[[i]],])$class
  NB[mm[[i]]]=naiveBayes(Class~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
}
Pred=data.frame(bag,ada, prf, svmc, NB, kn)
error=sapply(Pred, function(x)sum(w$Class!=x)/n)   


#13.1.1二分类问题
w=read.csv("Sports.csv")[,-c(17, 40, 49, 59)]
library(adabag);library(ipred);library(kknn)
library(e1071);library (randomForest);
Z=10;D=1;n=nrow(w)
mm=Fold(Z,w,D,1010)
kn=w[,1]->prf->NB->bag->svmc->ada
set.seed(1010)
for(i in 1:Z){
    kn[mm[[i]]]=kknn(Label~.,w[-mm[[i]],],w[mm[[i]],])$fit
    prf[mm[[i]]]=randomForest(Label~.,w[-mm[[i]],]) %>%
      predict(w[mm[[i]],])
    svmc[mm[[i]]]=svm(Label~.,w[-mm[[i]],])%>%
      predict(w[mm[[i]],])
    bag[mm[[i]]]=ipred::bagging(Label~.,w[-mm[[i]],])%>%
      predict(w[mm[[i]],])
    a=boosting(Label~.,w[-mm[[i]],])
    ada[mm[[i]]]=predict(a,w[mm[[i]],])$class
    NB[mm[[i]]]=naiveBayes(Label~.,w[-mm[[i]],])%>%
      predict(w[mm[[i]],])
}
Pred=data.frame(bag, ada, prf, svmc, NB, kn)
error=sapply(Pred, function(x) sum(w[,1]!=x)/n)

#例13.2
w=read.csv("mushroom.csv")
Z=10; D=1;n=nrow(w)
mm=Fold(Z,w,D,1010)
w[,1]->prf->NB->bag->ada
set.seed(1010)
for(i in 1:Z){
  prf[mm[[i]]]=randomForest(type~.,w[-mm[[i]],])%>%
     predict(w[mm[[i]],])
  bag[mm[[i]]]=ipred::bagging(type~.,w[-mm[[i]],])%>%
     predict(w[mm[[i]],])
 a=boosting(type~.,w[-mm[[i]],])
 ada[mm[[i]]]=predict(a,w[mm[[i]],])$class
 NB[mm[[i]]]=naiveBayes(type~.,w[-mm[[i]],])%>%
    predict(w[mm[[i]],])
}
Pred=data.frame(bag, ada, prf, NB)
error=sapply(Pred, function(x)sum(w$Class!=x)/n) 

#13.1.2回归问题
library(mlbench);data(BostonHousing2)
w=BostonHousing2[,-c(1:5)]
w$chas=factor(w$chas)

w=read.csv("BostonHousing2.csv")[,-c(1:5)]
s.w=lapply(w,function(x){(x-min(x))/(max(x)-min(x))})%>%as.data.frame()
nm<-names(s.w)
f<-as.formula(paste("cmedv ~", paste(nm[!nm %in% "cmedv"], collapse=" + ")))
w$chas=factor(w$chas)
library(dplyr) ;library(ipred);library(kknn)
library(e1071);library(randomForest);library(neuralnet)
  
  
Z=10;n=nrow(w);set.seed(1010)
I=sample(rep(1:Z,ceiling(n/Z)))[1:n]
ff=formula("cmedv~.")
rep(0,n)->kn->prf->bag->svmc->Lm->net

set.seed(1010)
for(i in 1:Z){
  kn[I==i]=kknn(ff,w[I!=i,],w[I==i,])$fit
  prf[I==i]=randomForest(ff,w[I!=i,])%>%
    predict(w[I==i,])
  svmc[I==i]=svm(ff,w[I!=i,])%>%
    predict(w[I==i,])
  bag[I==i]=ipred::bagging(ff, w[I!=i,])%>%
    predict(w[I==i,])
  Lm[I==i]=lm(ff,w[I!=i,])%>%
    predict(w[I==i,])
  nn=neuralnet(f,data=s.w[I!=i,], hidden=c(5,3),linear.output=T)
  net[I==i]=compute(nn,s.w[I==i,-1])$net.result
}
net=net*(max(w$cmedv)-min(w$cmedv))+min(w$cmedv)

Pred=data.frame(kn, prf, bag, svmc, Lm, net)
NMSE=sapply(Pred, function(x) sum((w[,1]-x)^2)/sum((w[,1]-mean(w[,1]))^2))

barplot(NMSE, names.arg=c("KNN", "Rabdom Forest", "Bagging", "SVM", "Linear Model","Neural Network"),
        horiz=TRUE, col=4, las=2, main="10-Fold CV NMSE of Boston Housing Regression")















































