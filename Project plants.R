#opening file
data_question_one<-read.delim(choose.files())
data_question_two<-read.delim(choose.files())
#calculating means 
mean_petalwidth<-mean(unlist(data_question_one$PetalWidth))
mean_petalLength<-mean(unlist(data_question_one$PetalLength))
mean_number_of_flowers<-mean(unlist(data_question_one$NFlowers))
#petal width
data_question_one_low_petalwidth<-subset(data_question_one,Line=="L",select="PetalWidth")
data_question_one_control_petalwidth<-subset(data_question_one,Line=="C",select="PetalWidth")
data_question_one_control_petalwidth<-subset(data_question_one,Line=="C",select="PetalWidth")
data_question_one_high_petalwidth<-subset(data_question_one,Line=="H",select="PetalWidth")

mean_petalwidth_low<-mean(unlist(data_question_one_low_petalwidth))
mean_petalwidth_high<-mean(unlist(data_question_one_high_petalwidth))
mean_petalwidth_control<-mean(unlist(data_question_one_control_petalwidth))
#Petal Length
data_question_one_low_PetalLength<-subset(data_question_one,Line=="L",select="PetalLength")
data_question_one_control_PetalLength<-subset(data_question_one,Line=="C",select="PetalLength")
data_question_one_high_PetalLength<-subset(data_question_one,Line=="H",select="PetalLength")

mean_PetalLength_low<-mean(unlist(data_question_one_low_PetalLength))
mean_PetalLength_high<-mean(unlist(data_question_one_high_PetalLength))
mean_PetalLength_control<-mean(unlist(data_question_one_control_PetalLength))
i
#Selection and response value
selection_value_high<-16.5-13.1
selection_value_low<-10.4-13.1
respone_value_low<-mean_petalwidth_low-13.1
respone_value_high<-mean_petalwidth_high-13.1
14.461-13.1
#Heritability
realized_heritability_high<-respone_value_high/selection_value_high
realized_heritability_low<-respone_value_low/selection_value_low

#Difference for the question 1
aov()
#Calcuating Va based on bv
mean_overall_petalwidth<-mean(data_question_two$MPetWOff)
bv<-2(data_question_two$MPetWOff)
bv<-2*(data_question_two$MPetWOff-mean_overall_petalwidth)
bv_2<-(bv)^2
Va_petalwidth<-mean(bv_2)
#Heritibility value
h<-Va_petalwidth/var(data_question_two$MPetWOff)
#calculating the regression of petal width and Petal Length
at_base<-sqrt(cov(data_question_two$PetWSire,data_question_two$MPetWOff)*cov(data_question_two$PetLSire,data_question_two$MPetLOff))
rA1<-cov(data_question_two$PetWSire,data_question_two$MPetLOff)/at_base
rA2<-cov(data_question_two$MPetWOff,data_question_two$PetLSire)/at_base
#Petal width and flower nulber regression
at_base_flower<-sqrt(cov(data_question_two$PetWSire,data_question_two$MPetWOff)*cov(data_question_two$FlrNoSire,data_question_two$MFlrNoOff))
rA1_flower<-cov(data_question_two$PetWSire,data_question_two$MFlrNoOff)/at_base_flower
rA2_flower<-cov(data_question_two$MPetWOff,data_question_two$FlrNoSire)/at_base_flower

ima