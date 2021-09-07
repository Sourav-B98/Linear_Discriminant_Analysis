library("MASS")
library(ggplot2)
library(ggExtra)
library(gridExtra)
library("dplyr")
library(ggcorrplot)
library(reshape2)
head(iris)
summary(iris)
setosa <- iris %>% filter(Species %in% "setosa")
#setosa
versicolor <- iris %>% filter(Species %in% "versicolor")
versicolor
virginica <- iris %>% filter(Species %in% "virginica")
#virginica


setosa_y1 <-log(setosa$Sepal.Length/setosa$Sepal.Width)  
setosa_y2 <- log(setosa$Petal.Length/setosa$Petal.Width)
setosa1 <- cbind(setosa,setosa_y1,setosa_y2)
#setosa1


versicolor_y1 <- log(versicolor$Sepal.Length/versicolor$Sepal.Width)
versicolor_y2 <- log(versicolor$Petal.Length/versicolor$Petal.Width)
versicolor1 <- cbind(versicolor,versicolor_y1,versicolor_y2)
#versicolor1


virginica_y1 <- log(virginica$Sepal.Length/virginica$Sepal.Width)
virginica_y2 <- log(virginica$Petal.Length/virginica$Petal.Width)
virginica1 <- cbind(virginica,virginica_y1,virginica_y2)
#virginica1

#scatter3D(virginica_y1, virginica_y2,virginica_y1, colvar = NULL, col = NULL, add = FALSE)

#persp3D(virginica_y1,virginica_y2,f,shade=.5,theta=30, phi=90,ticktype = "detailed",expand=.4,d=10)


melted<-melt(iris)


melted%>%ggplot(aes(x=Species,y=value,fill=Species))+geom_violin(alpha=0.5)+geom_boxplot(width=0.1,fill="white")+facet_wrap(~variable, scales = "free")

pcor1<-iris%>%ggplot(aes(x=Sepal.Length,y=Petal.Length,alpha=Sepal.Width,size=Petal.Width,color=Species))+geom_point()
pcor1+labs(x="Sepal length",y="Petal length",subtitle = "Distribution of datapoints",size= "Petal width",alpha="Sepal width")

corr <- cor(iris[,1:4])
corr
pcor<-ggcorrplot(corr,type="upper")
grid.arrange(pcor1,pcor,nrow=1)


irisnew <- iris
irisnew$Logy1 <- log(iris$Sepal.Length)-log(iris$Sepal.Width)
irisnew$Logy2 <- log(iris$Petal.Length)-log(iris$Petal.Width)
irisnew$Species<-iris$Species

melted2<-melt(irisnew[5:7])

melted2%>%ggplot(aes(x=Species,y=value,fill=Species))+geom_violin(alpha=0.5)+geom_boxplot(width=0.05,fill="white")+facet_wrap(~variable,scales="free")


plot_setosa_y1vsy2 <- ggplot(setosa1) + geom_point(aes(setosa_y1,setosa_y2, color = "red",size = 5)) + ylab("petal shape of setosa iris") + xlab("sepal shape of setosa iris")
plot_setosa_y1vsy2


plot_versicolor_y1vsy2 <- ggplot(versicolor1) + geom_point(aes(versicolor_y1,versicolor_y2, color = "green",size = 5)) + ylab("petal shape of versicolor iris") + xlab("sepal shape versicolor iris")#+stat_ellipse()
plot_versicolor_y1vsy2


plot_virginica_y1vsy2 <- ggplot(virginica1) + geom_point(aes(virginica_y1,virginica_y2, color = "black",size = 5)) + ylab("petal shape of virginica iris") + xlab("sepal shape of virginica iris")
plot_virginica_y1vsy2

b1 <- ggplot(setosa1,aes(setosa_y1,setosa_y2))+geom_point(color = "red", size = 5)+stat_ellipse()
ggMarginal(b1,type="densigram",margins="both",size=5)

b2 <-ggplot(versicolor1,aes(versicolor_y1,versicolor_y2))+geom_point(color = "blue", size = 5)+stat_ellipse()
ggMarginal(b2,type="densigram",margins="both",size=5)

b3<-ggplot(virginica1,aes(virginica_y1,virginica_y2))+geom_point(color = "green", size = 5)+stat_ellipse()
ggMarginal(b3,type="densigram",margins="both",size=5)

View(iris)
irisnew <- iris
irisnew$Logy1 <- log(iris$Sepal.Length)-log(iris$Sepal.Width)
irisnew$Logy2 <- log(iris$Petal.Length)-log(iris$Petal.Width)
irisnew$Species<-iris$Species
irisnew


plot_species_y1vsy2 <- ggplot(irisnew) + geom_point(aes(Logy1, Logy2, group = Species, color = Species, size = 5,shape=Species)) + ylab(" logy2") + xlab("logy1")+
  labs(subtitle = "Actual classes on log(y1) vs log(y2) plane")
#plot_species_y1vsy2

pl<-ggMarginal(plot_species_y1vsy2,type="densigram",margins="both",size=5,groupFill=T, groupColour = T)
pl
### APER with both logy1 and logy2

fit<-lda(Species~.,irisnew[5:7],prior=c(1,1,1)/3)
#fit<-lda(Species~irisnew[,6]+irisnew[,7],irisnew,prior=c(1,1,1)/3)

fitvalues<- predict(fit,irisnew)
fitclass<- fitvalues$class


pd<-cbind(as.data.frame(fitvalues$x),pred=fitvalues$class,actual=iris$Species,check=(fitvalues$class==iris$Species))

p1<-pd%>%ggplot(aes(x=LD1,y=LD2,color=actual,shape=actual,alpha=0.6))+
  geom_point(size=4)+labs(subtitle = "Actual classes on the discriminant plane",color="Actual classes",shape="Actual classes")

p11<-ggMarginal(p1,type = "densigram",margins = "both", groupColour = T,groupFill = T)
p11
p2<-pd%>%ggplot(aes(x=LD1,y=LD2,color=pred,shape=pred,size=check, alpha=0.6))+
  geom_point()+labs(subtitle = "Predicted classes on the discriminant plane",color="Predicted classes",shape="Predicted classes")
p21<-ggMarginal(p2,type = "densigram",margins = "both", groupColour = T,groupFill = T)
p21

grid.arrange(p11,p21,pl,nrow=2)

comb<-grid.arrange(p11,p21,nrow=2)

ct <- table(irisnew$Species,fitclass,dnn=c("Actual","Predicted"))
print(ct)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))
APER=1-sum(diag(prop.table(ct)))
APER

## expected APER with both logy1 and logy2

irn<-irisnew[5:7]


Er<-c()
El<-c()
for (i in 1:150){
  holdout<-irn[-i,]
  fith<-lda(Species~.,holdout,prior=c(1,1,1)/3)
  fitvalh<-predict(fith,holdout)
  Er<-c(Er,1-mean(holdout$Species==fitvalh$class))
  El<-c(El,1-mean(holdout$Species==fitvalh$class))
}
EEr<-sum(Er)/150
EEr

Ep<-ggplot(mapping = aes(x=1:150,y=El,color=pd$actual))+geom_point()+geom_line()+
  labs(x="Index",y="APER",title = "Errors in the holdout process",subtitle = "logy1 and logy2", color = "classes")+
  geom_hline(yintercept = EEr,color="magenta")+annotate("text",x=18,y=EEr,label=paste("Expected AER=",signif(EEr,4)),vjust=1,color="magenta",size=5)+
  geom_hline(yintercept = APER,color="navyblue")+annotate("text",x=18,y=APER,label=paste("APER=",signif(APER,4)),vjust=1,color="navyblue",size=5)+
  geom_text(aes(label=1:150,color=pd$actual),hjust=1,vjust=1)
Ep
grid.arrange(comb,Ep,nrow=1)

### APER with logy1

fit_y1<-lda(Species~.,irisnew[5:6],prior=c(1,1,1)/3)

fitvalues_y1<- predict(fit_y1,irisnew)
fitclass_y1<- fitvalues_y1$class


pd_y1<-cbind(as.data.frame(fitvalues_y1$x),pred=fitvalues_y1$class,actual=iris$Species,check=(fitvalues_y1$class==iris$Species))

p1_y1<-pd_y1%>%ggplot(aes(x=LD1,y=0,color=actual,shape=actual,alpha=0.6))+
  geom_point(size=4)+labs(subtitle = "Actual class on the discriminant line",color="Actual classes",shape="Actual classes")
p11_y1<-ggMarginal(p1_y1,type = "densigram",margins = "x", groupColour = T,groupFill = T)
p11_y1

p2_y1<-pd_y1%>%ggplot(aes(x=LD1,y=0,color=pred,shape=pred,size=check,alpha=0.6))+
  geom_point()+labs(subtitle = "Predicted classes on the discriminant line",color="Predicted classes",shape="Predicted classes")
p21_y1<-ggMarginal(p2_y1,type = "densigram",margins = "x", groupColour = T,groupFill = T)
p21_y1

grid.arrange(p11_y1,p21_y1,nrow=2)

comb_y1<-grid.arrange(p11_y1,p21_y1,nrow=2)




ct_y1 <- table(irisnew$Species,fitclass_y1,dnn=c("Actual","Predicted"))
print(ct_y1)
diag(prop.table(ct_y1, 1))
# total percent correct
sum(diag(prop.table(ct_y1)))
APER_y1=1-sum(diag(prop.table(ct_y1)))
APER_y1

## expected APER with logy1

irn_y1<-irisnew[5:6]


Er_y1<-c()
El_y1<-c()
for (i in 1:150){
  holdout_y1<-irn_y1[-i,]
  fith_y1<-lda(Species~.,holdout_y1,prior=c(1,1,1)/3)
  fitvalh_y1<-predict(fith_y1,holdout_y1)
  Er_y1<-c(Er_y1,1-mean(holdout_y1$Species==fitvalh_y1$class))
  El_y1<-c(El_y1,1-mean(holdout_y1$Species==fitvalh_y1$class))
}
EEr_y1<-sum(Er_y1)/150
EEr_y1



Ep_y1<-ggplot(mapping = aes(x=1:150,y=El_y1,color=pd_y1$actual))+geom_point()+geom_line()+
  labs(x="Index",y="APER",title = "Errors in the holdout process",subtitle = "logy1", color= "classes")+
  geom_hline(yintercept = EEr_y1,color="magenta")+annotate("text",x=18,y=EEr_y1,label=paste("Expected AER=",signif(EEr_y1,4)),vjust=1,color="magenta",size=5)+
  geom_hline(yintercept = APER_y1,color="navyblue")+annotate("text",x=18,y=APER_y1,label=paste("APER=",signif(APER_y1,4)),vjust=2,color="navyblue",size=5)+
  geom_text(aes(label=1:150,color=pd_y1$actual),hjust=1,vjust=1)
Ep_y1
grid.arrange(comb_y1,Ep_y1,nrow=1)

### APER with logy2

fit_y2<-lda(Species~.,irisnew[seq(5,7,2)],prior=c(1,1,1)/3)

fitvalues_y2<- predict(fit_y2,irisnew)
fitclass_y2<- fitvalues_y2$class


pd_y2<-cbind(as.data.frame(fitvalues_y2$x),pred=fitvalues_y2$class,actual=iris$Species,check=(fitvalues_y2$class==iris$Species))

p1_y2<-pd_y2%>%ggplot(aes(x=LD1,y=0,color=actual,shape=actual,alpha=0.6))+
  geom_point(size=4)+labs(subtitle = "Actual class on the discriminant line",color="Actual classes",shape="Actual classes")
p11_y2<-ggMarginal(p1_y2,type = "densigram",margins = "x", groupColour = T,groupFill = T)
p11_y2

p2_y2<-pd_y2%>%ggplot(aes(x=LD1,y=0,color=pred,shape=pred,size=check,alpha=0.6))+
  geom_point()+labs(subtitle = "Predicted classes on the discriminant line",color="Predicted classes",shape="Predicted classes")
p21_y2<-ggMarginal(p2_y2,type = "densigram",margins = "x", groupColour = T,groupFill = T)
p21_y2

grid.arrange(p11_y2,p21_y2,nrow=2)

comb_y2<-grid.arrange(p11_y2,p21_y2,nrow=2)



ct_y2 <- table(irisnew$Species,fitclass_y2,dnn=c("Actual","Predicted"))
print(ct_y2)
diag(prop.table(ct_y2, 1))
# total percent correct
sum(diag(prop.table(ct_y2)))
APER_y2=1-sum(diag(prop.table(ct_y2)))
APER_y2

## expected APER with logy2

irn_y2<-irisnew[seq(5,7,2)]


Er_y2<-c()
El_y2<-c()
for (i in 1:150){
  holdout_y2<-irn_y2[-i,]
  fith_y2<-lda(Species~.,holdout_y2,prior=c(1,1,1)/3)
  fitvalh_y2<-predict(fith_y2,holdout_y2)
  Er_y2<-c(Er_y2,1-mean(holdout_y2$Species==fitvalh_y2$class))
  El_y2<-c(El_y2,1-mean(holdout_y2$Species==fitvalh_y2$class))
}
EEr_y2<-sum(Er_y2)/150
EEr_y2

Ep_y2<-ggplot(mapping = aes(x=1:150,y=El_y2,color=pd_y2$actual))+geom_point()+geom_line()+
  labs(x="Index",y="APER",title = "Errors in the holdout process", color= "classes",subtitle = "logy2")+
  geom_hline(yintercept = EEr_y2,color="magenta")+annotate("text",x=18,y=EEr_y2,label=paste("Expected AER=",signif(EEr_y2,4)),vjust=1,color="magenta",size=5)+
  geom_hline(yintercept = APER_y2,color="navyblue")+annotate("text",x=18,y=APER_y2,label=paste("APER=",signif(APER_y2,4)),vjust=2.5,color="navyblue",size=5)+
  geom_text(aes(label=1:150,color=pd_y2$actual),hjust=1,vjust=1)
Ep_y2
grid.arrange(comb_y2,Ep_y2,nrow=1)

grid.arrange(Ep,Ep_y1,Ep_y2,nrow=2)
#partimat(Species ~ irisnew[,6]+irisnew[,7], data=irisnew, method="lda")

###
