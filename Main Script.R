#Data
mydata<-read.table(choose.files(), header = TRUE)
head(mydata)

#boxplot
boxplot(mydata$K121R)

#horizental boxplot
boxplot(mydata$K121R, main = "Gene expression", xlab = "logFC", ylab = "Genes", col = "orange",
        border = "brown",horizontal = TRUE, notch = TRUE)

#Multiple Boxplots
K121Q <- mydata$K121Q
K121R <- mydata$K121R
H.NSKO <- mydata$H.NSKO
H.NS <- mydata$H.NS

boxplot(K121Q, K121R, H.NSKO, H.NS, main = "Gene expression",
        at= c(1,3,5,7), names = c("K121Q", "K121R", "H.NSKO", "H.NS"),
        las = 2, col = c("orange","red"), border = "brown", horizontal = TRUE, notch = T)


#Boxplot form Formula
boxplot(K121Q~CON, data=mydata, main="Different boxplots for each K121Q",
        xlab="K121Q level",  ylab="K121Q", col="orange", border="brown")

boxplot(K121Q ~ CON, data = mydata, col = "lightgray")
boxplot(K121R ~ CON, data = mydata, col = "bisque")
boxplot(K121R ~ CON, data = mydata, notch = TRUE, add = TRUE, col = "blue")


boxplot(K121R ~ CON, data = mydata, boxwex = 0.5, col = c("orange", "yellow", "green"),
        main = "Gene expression",xlab = "Concentration", ylab = "K121R", ylim = c(-6, 6))







#opening_file
new_data4<-read.delim(file.choose(),header=FALSE)
head(new_data4)
#Renaming_column(Mainscene)
new_data<-data.frame(lapply(new_data,function(x){gsub("Main_scene","",x)}))

#Colnames
colnames(new_data4)<-c("id","username","age","sex","gaming","diss","nameofdis","emptytime","emptylevel","passivetime","passivelevel","activetime","activecookie","activelevel","noncomptime","noncompcookie","noncomplevel","comptime","compcookie","complevel","limittime","limitcookie","limitlevel","nonlimittime","nonlimitcookie","nonlimitlevel","rpetime","rpecookie","rpelevel","rpebug","nonrpetime","nonrpecookie","nonrpelevel","nonrpebug")
#removing_column
vector<-c(2,7)
new_data<-new_data[-vector]

#adding row names
vector2<-c()
for (x in c(new_data$id)){
  vector2<-append(vector2,(paste("subject",x,sep="")))}
vector2<-data.frame(vector2)           
colnames(vector2)<-"number_of_subjects"
new_data<-cbind(new_data,vector2)

#Making_our_value_integer_for_further_analysis
head(new_data)
vector_level<-c(7,9,12,15,18,21,24,27,31)
new_data[vector_level]
vector_time<-c(6,8,10,13,16,19,22,25,29)
head(new_data[vector_time])
new_data_integer<-data.frame(lapply(new_data[vector_time],function(x){as.integer(x)}))
new_data_integer

#Melting the data 
library(reshape2)
new_data2<-melt(cbind(new_data[-vector_time],new_data_integer))
new_data2
#setting the appropriate real time
number_subject<-nrow(new_data)
vector_number<-c()
for (i in 1:9){
vector_number<-append(vector_number,i*number_subject)}
vector_number
vector_number2<-c(1)
for(i in vector_number){vector_number2<-append(vector_number2,i+1)}
highest_number<-vector_number2[length(vector_number2)]-1
vector_number2<-vector_number2[-length(vector_number2)]
vector_number2<-append(vector_number2,highest_number)
new_data3<-new_data2
level_time<-new_data[vector_level]
list(level_time[1])
new_var<-c()
for (i in 1:9){
  for(x in 1:nrow(level_time)){new_var<-append(new_var,level_time[x,i])} 
  new_data3[vector_number2[i]:vector_number[i],6]<-new_var
  new_var<-c()
}
#saving the data
saved_data1<-new_data2
saved_data2<-new_data
new_data2<-new_data3

#colname for realtime and taking out new_data2 used columns
new_data2<-new_data2[-c(7,9,11,13,15,17,19,22,24)]
colnames(new_data2)[6]<-"real_time"


#data_witout_NAs
data_without_NAs<-subset(new_data2,id<70|id>70)
data_without_NAs<-subset(data_without_NAs,id<74|id>74)
data_without_NAs<-subset(data_without_NAs,id<79|id>79)
data_without_NAs
new_data2
real_1<-as.integer(data_without_NAs$real_time) 
vector3<-data_without_NAs$value-real_1
vector3<-data.frame(vector3)
colnames(vector3)<-"realexc"
new_data2<-cbind(data_without_NAs,vector3)

#changing the name of col main_variable
colnames(new_data2)[16]<-"main_variable"
#Taking out people with disorders
new_data2<-subset(new_data2,diss<1| diss>1)
#first shapiro wilk tests
emptytime<-subset(new_data2,main_variable=="emptytime")
shapiro.test(emptytime$realexc)

passivetime<-subset(new_data2,main_variable=="passivetime")
shapiro.test(passivetime$realexc)

activetime<-subset(new_data2,main_variable=="activetime")
shapiro.test(as.integer(activetime$realexc))



comptime<-subset(new_data2,main_variable=="comptime")
shapiro.test(as.integer(comptime$realexc))

noncomptime<-subset(new_data2,main_variable=="noncomptime")
shapiro.test(noncomptime$realexc)



limittime<-subset(new_data2,main_variable=="limittime")
shapiro.test(as.integer(limittime$realexc))

nonlimittime<-subset(new_data2,main_variable=="nonlimittime")
shapiro.test(nonlimittime$realexc)



nonrpetime<-subset(new_data2,main_variable=="nonrpetime")
shapiro.test(nonrpetime$realexc)

rpetime<-subset(new_data2,main_variable=="rpetime")
shapiro.test(rpetime$realexc)

new_data
#(((((((vectores<-c("rpetime","passivetime")
#nemudar3<-list()
#for(i in vectores){nemudar3<-append(nemudar3,ggdensity(data=i,x="value"))}
#ggdensity(rpetime,"value")
#nemudar3<-append(nemudar3,ggdensity(rpetime,"value"))
#nemudar4[2]
#list_3<-append(list_3,1)
#nemudar3[1])))))))))
nrow(data_without_NAs)/9
nrow(subset(data_without_NAs,diss!=1))/9
#for analyzing reward
boxplot.stats(as.integer(activetime$realexc))$out
boxplot.stats(as.integer(passivetime$realexc))$out
boxplot.stats(as.integer(emptytime$realexc))$out
new_data2<-subset(new_data2,id!=49)

#For normalzing empty time
library(caret)
process <- preProcess(as.data.frame(emptytime$Value), method=c("range"))
norm_scale <- predict(process, as.data.frame(emptytime$Value))
ggdensity(as.integer(unlist(scale_dataemptytime)))
shapiro.test(as.integer(unlist(norm_scale)))
shapiro.test(as.integer(unlist(scale_dataemptytime)))
shapiro.test(as.integer(unlist(log_scale)))
scale_dataemptytime <- as.data.frame(scale(emptytime$realexc))
scale_dataactivetime <- as.data.frame(scale(activetime$realexc))
scale_datapassivetime<-as.data.frame(scale(passivetime$realexc))
colnames(scale_datapassivetime)<-"scale_realexc"
for_passive<-cbind(scale_datapassivetime,passivetime)
for_active<-cbind(scale_dataactivetime,activetime)
#binding the data for analysis
for_emptytime<-cbind(scale_dataemptytime,emptytime)
for_rewardtest1<-rbind(for_emptytime,for_passive)
for_rewardtest2<-rbind(for_emptytime,for_active)
for_rewardtest3<-rbind(activetime,passivetime)
for_rewardtest4<-rbind(emptytime,passivetime)
#for analyzing comp time 
#for_comp_time
subset(new_data2,realexc==-35)
boxplot.stats(as.integer(comptime$realexc))$out
boxplot.stats(as.integer(noncomptime$realexc))$out
for_comptime_first<-subset(new_data2,id!=61)
comptime<-subset(for_comptime_first,main_variable=="comptime")
shapiro.test(as.integer(comptime$realexc))
noncomptime<-subset(for_comptime_first,main_variable=="noncomptime")
s31<-shapiro.test(noncomptime$realexc)
for_comptime<-rbind(comptime,noncomptime)
#for analyzing limittime
boxplot.stats(as.integer(limittime$realexc))$out
boxplot.stats(as.integer(nonlimittime $realexc))$out
for_limittime<-rbind(limittime,nonlimittime)

#for analyzing rpetime 
#for_rpetime
boxplot.stats(as.integer(nonrpetime$realexc))$out
boxplot.stats(as.integer(rpetime $realexc))$out
for_rpetime_first<-subset(new_data2,id!=64)
nonrpetime<-subset(for_rpetime,main_variable=="nonrpetime")
shapiro.test(nonrpetime$realexc)
rpetime<-subset(for_rpetime,main_variable=="rpetime")
shapiro.test(rpetime$realexc)
for_rpetime<-subset(for_rpetime_first,main_variable=="rpetime")
for_nonrpetime<-subset(for_rpetime_first,main_variable=="nonrpetime")
for_rpetime<-rbind(for_nonrpetime,for_rpetime)
nrow(activetime)
nrow(passivetime)
nrow(comptime)
nrow(rpetime)
nrow(limittime)
#for analyzing reward
for_reward<-rbind(activetime,emptytime)
kruskal.test(data=for_reward,realexc~main_variable)
for_reward2<-rbind(passivetime,emptytime)
kruskal.test(data=for_reward2,realexc~main_variable)
#t.tests and sign tests
library(BSDA)
s1<-SIGN.test(emptytime$realexc,passivetime$realexc,alternative = "two.sided")
s2<-SIGN.test(emptytime$realexc,activetime$realexc,alternative = "two.sided")
s3<-t.test(data=for_passivetime_test,scale_realexc~main_variable,pair=TRUE)
s4<-t.test(data=for_activetime_test,scale_realexc~main_variable,pair=TRUE)
s5<-t.test(data=for_rewardtest3,realexc~main_variable,pair=TRUE)
s6<-t.test(data=for_comptime,pair=TRUE,realexc~main_variable)
s7<-t.test(data=for_limittime,pair=TRUE,realexc~main_variable)
s8<-t.test(data=for_rpetime,pair=TRUE,realexc~main_variable)


#question?
density.g<-function(X){ggdensity(as.integer(x$realexc))}
density.g(rpetime)

#density functions
library(ggpubr)
ggdensity(as.integer(passivetime$realexc), xlab="Passive time",fill = "lightgray",add = "mean")
ggdensity(as.integer(activetime$realexc),xlab="Active time",fill = "lightgray",add = "mean")
ggdensity(as.integer(comptime$realexc),xlab="Competition time",fill = "lightgray",add = "mean")
ggdensity(as.integer(rpetime$realexc),xlab="Reward prediction error time",fill = "lightgray",add = "mean")
ggdensity(as.integer(nonlimittime$realexc),xlab="Non limit time",fill = "lightgray",add = "mean")
ggdensity(as.integer(noncomptime$realexc),xlab="Non competition time",fill = "lightgray",add = "mean")
ggdensity(as.integer(emptytime$realexc),xlab="Control",fill = "lightgray",add = "mean")
ggdensity(as.integer(nonrpetime$realexc),xlab="Non Reward prediction error time",fill = "lightgray",add = "mean")
ggdensity(as.integer(limittime$realexc),xlab="Limit time",fill = "lightgray",add = "mean")
ggdensity(as.integer(unlist(normal_emptytime)),xlab="Normalized Control",fill = "lightgray",add = "mean")
ggdensity(as.integer(unlist(normal_activetime)),xlab="Normalized Active time",fill = "lightgray",add = "mean")
ggdensity(as.integer(unlist(normal_passivetime)),xlab="Normalized Passive time",fill = "lightgray",add = "mean")
?ggdensity
#boxplot
library(ggpubr)

ggboxplot(data=for_comptime,y="realexc",x="main_variable")
ggboxplot(data=for_rpetime,y="realexc",x="main_variable")
ggboxplot(data=for_limittime,y="realexc",x="main_variable")
ggboxplot(data=for_activetime_test,y="scale_realexc",x="main_variable")
ggboxplot(data=for_passivetime_test,y="scale_realexc",x="main_variable")
ggboxplot(data=for_reward,y="realexc",x="main_variable")
ggboxplot(data=for_rewardtest4,y="realexc",x="main_variable")
ggboxplot(data=for_reward2,y="realexc",x="main_variable")
#robust_scalar
robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
robust_normaling<-robust_scalar(as.integer(emptytime$realexc))
shapiro.test(robust_normaling)
a<-unlist(robust_normaling)
boxplot.stats(robust_normaling)$out

#mino_max
as<-lapply(as,function(x){norm_minmax(x)})
as<-as.integer(rpetime$value)
as<-data.frame(ass)
as<-unlist(as)
names(as)<-c()
is.atomic(ass)
boxplot.stats(as)$out
shapiro.test(as)

#unit_lenth
unit_length <- function(x) {
  x / sqrt(sum(x^2))
}
normal_unit<-lapply(ass,function(x){unit_length(x)})

#our old data
old_data<-read.delim(choose.files(),header=TRUE)
Ls<-c()
for(i in 1:5){Ls<-append(Ls,paste("L",sep="",i))}
Ns<-c(19,25,31,37,43)
for(i in 1:5){
  old_data<-data.frame(lapply(old_data,function(x){gsub(Ls[i],Ns[i],x)}))
  }
H<-as.integer(unlist(old_data$value))-as.integer(unlist(old_data$real_time))
old_to_bind<-data.frame(H)
colnames(old_to_bind)<-"realexc"
old_data<-cbind(old_to_bind,old_data)
old_data
old_data<-subset(old_data,diss!=1)
name_of_variables<-names(table(old_data$main_variable))
name<-c()
for(i in name_of_variables){
  name<-append(name,paste(i,sep = "","old"))
}
for (i in 1:9){assign(name[i],subset(old_data,main_variable==name_of_variables[i]))}
#question
shapiro.test(as.name("activetimeold"))[8]

#want to change a string to variable name
shapiro.test(as.integer(activetimeold$realexc))
shapiro.test(as.integer(emptytimeold$realexc))
shapiro.test(as.integer(passivetimeold$realexc))
shapiro.test(as.integer(limittimeold$realexc))
shapiro.test(as.integer(nonlimittimeold$realexc))
shapiro.test(as.integer(comptimeold$realexc))
shapiro.test(as.integer(noncomptimeold$realexc))
shapiro.test(as.integer(rpetimeold$realexc))
shapiro.test(as.integer(nonrpetimeold$realexc))

boxplot.stats(as.integer(activetimeold$realexc))$out
boxplot.stats(as.integer(emptytimeold$realexc))$out
boxplot.stats(as.integer(passivetimeold$realexc))$out

#Active and empty old
subset(old_data,realexc==-30)
subset(old_data,realexc==-22)
for_rewardold_active<-subset(old_data, number_of_subject!="subject2")
for_rewardold_active<-subset(for_rewardold_active,number_of_subject!="subject3")
activetimeold2<-subset(for_rewardold_active,main_variable=="activetime")
shapiro.test(as.integer(activetimeold2$realexc))
boxplot.stats(as.integer(activetimeold2$realexc))$out
empty_time_for_active<-subset(for_rewardold_active,main_variable=="emptytime")
shapiro.test(as.integer(empty_time_for_active$value))
boxplot.stats(as.integer(empty_time_for_active$realexc))$out
for_active_reward_old_test<-rbind(activetimeold2,empty_time_for_active)
t.test(data=for_active_reward_old_test,realexc~main_variable,pair=TRUE)

#passive and empty old
for_rewardold_passive_empty<-subset(old_data,main_variable=="emptytime")
for_rewardold_passive<-subset(old_data,main_variable=="passivetime")
for_passive_reward_old_test<-rbind(for_rewardold_passive_empty,for_rewardold_passive)
t.test(data=for_passive_reward_old_test,realexc~main_variable,pair=TRUE)


#limit time and non limit time old
boxplot.stats(as.integer(limittimeold$realexc))$out
boxplot.stats(as.integer(nonlimittimeold$realexc))$out
limit_time_old<-subset(old_data,main_variable=="limittime")
non_limit_time_old<-subset(old_data,main_variable=="nonlimittime")
limit_time_for_test<-rbind(limit_time_old,non_limit_time_old)
t.test(data=limit_time_for_test,realexc~main_variable,pair=TRUE)

#comp time and non comp time old
boxplot.stats(as.integer(noncomptimeold$realexc))$out
boxplot.stats(as.integer(comptimeold$realexc))$out
comp_time_old<-subset(old_data,main_variable=="comptime")
non_comptime_old<-subset(old_data,main_variable=="noncomptime")
comp_time_old_for_test<-rbind(comp_time_old,non_comptime_old)
t.test(data=comp_time_old_for_test,realexc~main_variable,pair=TRUE)

#rpetime and nonrpe time old
boxplot.stats(as.integer(nonrpetimeold$realexc))$out
boxplot.stats(as.integer(rpetimeold$realexc))$out
rpe_time_old<-subset(old_data,main_variable=="rpetime")
nonrpe_time_old<-subset(old_data,main_variable=="nonrpetime")
rpetimeold_for_test<-rbind(rpe_time_old,nonrpe_time_old)
t.test(data=rpetimeold_for_test,realexc~main_variable,pair=TRUE)

#boxplot old
library(ggpubr)
ggboxplot(data=for_passive_reward_old_test,y="realexc",x="main_variable")
ggboxplot(data=for_active_reward_old_test,y="realexc",x="main_variable")
ggboxplot(data=limit_time_for_test,y="realexc",x="main_variable")
ggboxplot(data=comp_time_old_for_test,y="realexc",x="main_variable")
ggboxplot(data=rpetimeold_for_test, y="realexc",x="main_variable")






#our old data+diss
old_data2<-read.table(choose.files(),header=TRUE)
Ls1<-c()
for(i in 1:5){Ls1<-append(Ls1,paste("L",sep="",i))}
Ns1<-c(19,25,31,37,43)
for(i in 1:5){
  old_data2<-data.frame(lapply(old_data2,function(x){gsub(Ls1[i],Ns1[i],x)}))
}
H1<-as.integer(unlist(old_data2$value))-as.integer(unlist(old_data2$real_time))
old_to_bind1<-data.frame(H1)
colnames(old_to_bind1)<-"realexc"
old_data2<-cbind(old_to_bind1,old_data2)
old_data2
name_of_variables1<-names(table(old_data2$main_variable))
name1<-c()
for(i in name_of_variables1){
  name1<-append(name1,paste(i,sep = "","old1"))
}
for (i in 1:9){assign(name1[i],subset(old_data2,main_variable==name_of_variables1[i]))}

#want to change a string to variable name+diss
shapiro.test(as.integer(activetimeold1$realexc))
shapiro.test(as.integer(emptytimeold1$realexc))
shapiro.test(as.integer(passivetimeold1$realexc))
shapiro.test(as.integer(limittimeold1$realexc))
shapiro.test(as.integer(nonlimittimeold1$realexc))
shapiro.test(as.integer(comptimeold1$realexc))
shapiro.test(as.integer(noncomptimeold1$realexc))
shapiro.test(as.integer(rpetimeold1$realexc))
shapiro.test(as.integer(nonrpetimeold1$realexc))

boxplot.stats(as.integer(activetimeold1$realexc))$out
boxplot.stats(as.integer(emptytimeold1$realexc))$out
boxplot.stats(as.integer(passivetimeold1$realexc))$out

#Active and empty old+diss
subset(old_data2,realexc==-30)
for_rewardold_active1<-subset(old_data2, number_of_subject!="subject2")
activetimeold3<-subset(for_rewardold_active1,main_variable=="activetime")
shapiro.test(as.integer(activetimeold3$realexc))
boxplot.stats(as.integer(activetimeold3$realexc))$out
empty_time_for_active1<-subset(for_rewardold_active1,main_variable=="emptytime")
shapiro.test(as.integer(empty_time_for_active1$realexc))
boxplot.stats(as.integer(empty_time_for_active1$realexc))$out
for_active_reward_old_test1<-rbind(activetimeold3,empty_time_for_active1)
t.test(data=for_active_reward_old_test1,realexc~main_variable,pair=TRUE)

#aya baad az hazf kardan outlier bayad dobare code oulier ro ejra kard?
#passive and empty old+diss
for_rewardold_passive_empty<-subset(old_data2,main_variable=="emptytime")
for_rewardold_passive<-subset(old_data2,main_variable=="passivetime")
for_passive_reward_old_test<-rbind(for_rewardold_passive_empty,for_rewardold_passive)
t.test(data=for_passive_reward_old_test,realexc~main_variable,pair=TRUE)


#limit time and non limit time old+diss
boxplot.stats(as.integer(limittimeold$realexc))$out
boxplot.stats(as.integer(nonlimittimeold$realexc))$out
limit_time_old<-subset(old_data2,main_variable=="limittime")
non_limit_time_old<-subset(old_data2,main_variable=="nonlimittime")
limit_time_for_test<-rbind(limit_time_old,non_limit_time_old)
t.test(data=limit_time_for_test,realexc~main_variable,pair=TRUE)

#comp time and non comp time old+diss
boxplot.stats(as.integer(noncomptimeold$realexc))$out
boxplot.stats(as.integer(comptimeold$realexc))$out
comp_time_old<-subset(old_data2,main_variable=="comptime")
non_comptime_old<-subset(old_data2,main_variable=="noncomptime")
comp_time_old_for_test<-rbind(comp_time_old,non_comptime_old)
t.test(data=comp_time_old_for_test,realexc~main_variable,pair=TRUE)

#rpetime and nonrpe time old+diss
boxplot.stats(as.integer(nonrpetimeold$realexc))$out
boxplot.stats(as.integer(rpetimeold$realexc))$out
rpe_time_old<-subset(old_data2,main_variable=="rpetime")
nonrpe_time_old<-subset(old_data2,main_variable=="nonrpetime")
rpetimeold_for_test<-rbind(rpe_time_old,nonrpe_time_old)
t.test(data=rpetimeold_for_test,realexc~main_variable,pair=TRUE)

#boxplot old+diss
library(ggpubr)
ggboxplot(data=for_passive_reward_old_test,y="realexc",x="main_variable")
ggboxplot(data=for_active_reward_old_test,y="realexc",x="main_variable")
ggboxplot(data=limit_time_for_test,y="realexc",x="main_variable")
ggboxplot(data=comp_time_old_for_test,y="realexc",x="main_variable")
ggboxplot(data=rpetimeold_for_test, y="realexc",x="main_variable")

#for drawing
for_drawing<-c()
for_drawing<-append(for_drawing,as.character(s8$p.value))
for_drawing<-data.frame(for_drawing)
colnames(for_drawing)<-"P value"
row.names(for_drawing)<-c("Passive reward","Active Reward","Percentized passive reward","Percentized active reward","Passive reward vs Active reward","Competition time","Limit time","Reward prediction error")
for(i in emptytime$realexc){ss<-append(ss,i)}
summary(emptytime)
for_drawing2<-c("","")
for_drawing2<-append(for_drawing2,as.character(s8$parameter))
for_drawing2<-data.frame(for_drawing2)
colnames(for_drawing2)<-"Degrees of Freedom"
for_drawing<-cbind(for_drawing,for_drawing2)

for_drawing3<-data.frame(c("Signtest","Signtest",rep(times=6,"Paired t.test")))
colnames(for_drawing3)<-"Method"
for_drawing<-cbind(for_drawing,for_drawing3)



#drawing tables
install.packages("kableExtra")
library(kableExtra)
for_drawing4 %>%
kbl()%>%
  kable_paper("hover")  

for_drawing4%>%
  kbl()%>%
  kable_paper("hover")  



#for drawing 2
s11<-summary(emptytime$realexc)
s12<-summary(passivetime$realexc)
s13<-summary(activetime$realexc)
s14<-summary(for_emptytime$scale_realexc)
s15<-summary(for_active$scale_realexc)
s16<-summary(for_passive$scale_realexc)

s17<-summary(comptime$realexc)
s18<-summary(noncomptime$realexc)

s19<-summary(limittime$realexc)
s20<-summary(nonlimittime$realexc)

s21<-summary(rpetime$realexc)
s22<-summary(nonrpetime$realexc)
vector_name<-c()
for(i in 1:6){vector_name<-append(vector_name,paste("Table",i,sep=""))}
for(i in 1:6){
assign(vector_name[i],rbind(s11[i],s12[i],s13[i],s14[i],s15[i],s16,s17[i],s18[i],s19[i],s20[i],s21[i],s22[i]))}
row.names(for_drawing4)<-c("Control","Passive Reward","Active Reward","Percentized Control","Percentized Passive Reward","Percentized Active Reward","Competition time","non Competition time","limit time","non limit time","Reward prediction error time","non Reward prediction error time")
vector_name
for_drawing4<-data.frame(Table1)
#shapiro for drawing
s21<-shapiro.test(emptytime$realexc)
s22<-shapiro.test(passivetime$realexc)
s23<-shapiro.test(activetime$realexc)
s24<-shapiro.test(normal_emptytime$scale_realexc)
s25<-shapiro.test(normal_passivetime$scale_realexc)
s26<-shapiro.test(normal_activetime$scale_realexc)
s27<-shapiro.test(comptime$realexc)
s28<-shapiro.test(noncomptime$realexc)
s29<-shapiro.test(limittime$realexc)
s30<-shapiro.test(nonlimittime$realexc)
s31<-shapiro.test(rpetime$realexc)
s32<-shapiro.test(nonrpetime$realexc)
vectt<-c()
vectt<-append(vectt,s32$p.value)
vectt<-data.frame(vectt)
row.names(vectt)<-row.names(for_drawing4)<-c("Control","Passive Reward","Active Reward","Percentized Control","Percentized Passive Reward","Percentized Active Reward","Competition time","Non Competition time","Limit time","Non limit time","Reward prediction error time","Non reward prediction error time")
colnames(vectt)<-"Shapiro Wilk P-value"
for_drawing4<-cbind(for_drawing4,vectt)
ggdensity(as.integer(unlist(scale_dataemptytime)),xlab="Normalized Control",fill = "lightgray",add = "mean")
ggdensity(as.integer(unlist(scale_dataactivetime)),xlab="Normalized Active time",fill = "lightgray",add = "mean")
ggdensity(as.integer(unlist(scale_datapassivetime)),xlab="Normalized Passive time",fill = "lightgray",add = "mean")
s30
s21
normalize(emptytime$realexc)
install.packages("heatmaply")
library(heatmaply)
ass<-normalize(emptytime$realexc)
normal_emptytime<-data.frame(percentize(emptytime$realexc))
colnames(normal_emptytime)<-"scale_realexc"
normal_activetime<-data.frame(percentize(activetime$realexc))
colnames(normal_passivetime)<-"scale_realexc"
normal_passivetime<-data.frame(percentize(passivetime$realexc))
colnames(normal_activetime)<-"scale_realexc"
for_passive<-cbind(normal_passivetime,passivetime)
for_active<-cbind(normal_activetime,activetime)
for_emptytime<-cbind(normal_emptytime,emptytime)
for_passivetime_test<-rbind(for_passive,for_emptytime)
for_activetime_test<-rbind(for_active,for_emptytime)

save.image()

s31<-var(emptytime$realexc)
s32<-var(passivetime$realexc)
s33<-var(activetime$realexc)
s34<-var(normal_emptytime$scale_realexc)
s35<-var(normal_passivetime$scale_realexc)
s36<-var(normal_activetime$scale_realexc)
s37<-var(comptime$realexc)
s38<-var(noncomptime$realexc)
s39<-var(limittime$realexc)
s40<-var(nonlimittime$realexc)
s41<-var(rpetime$realexc)
s42<-var(nonrpetime$realexc)
vectr<-c()
vectr<-append(vectr,s42)
vectr<-data.frame(vectr)
colnames(vectr)<-"Variance"             
for_drawing4<-cbind(for_drawing4,vectr)
version()
RStudio.Version()

citation("BSDA")
table(new_data2)
table(new_data2$sex)
117/9

table(new_data4$sex)
