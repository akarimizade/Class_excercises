#Opening the data
data<-read.delim(choose.files())

#relative fitness
mean_value_of_number_of_seeds<-mean(data$NSeeds)
relative_fitness<-data$NSeeds/mean_value_of_number_of_seeds

#Standardize and showing what Scale function is doing
standard_dev_value_Nflowers<-sqrt(var(data$NFlowers))
mean_Nflowers<-mean(data$NFlowers)
standard_value_of_Nflowers<-(data$NFlowers-mean_Nflowers)/standard_dev_value_Nflowers
standard_value_of_PetL<-scale(data$PetL)
lm(standard_value_of_PetL~standard_value_of_Nflowers)
#Plots
plot(relative_fitness~standard_value_of_Nflowers)
plot(relative_fitness~standard_value_of_PetL)
#Linear and Non linear models
lm(relative_fitness~standard_value_of_Nflowers)
lm(relative_fitness~standard_value_of_Nflowers+I(standard_value_of_Nflowers^2))
#Only an small deviation

lm(relative_fitness~standard_value_of_PetL+I(standard_value_of_PetL^2))

lm(relative_fitness~standard_value_of_PetL)
lm(relative_fitness~standard_value_of_PetL+standard_value_of_Nflowers)
lm(relative_fitness~standard_value_of_PetL+standard_value_of_Nflowers+I(standard_value_of_Nflowers^2)+I(standard_value_of_PetL^2))

cor.test(standard_value_of_PetL,relative_fitness)
cor.test(standard_value_of_Nflowers,relative_fitness)
cor.test(standard_value_of_Nflowers,standard_value_of_PetL)


#matrix for question 2
B<-matrix(c(0.95,0.12,0.07),nrow=3)
matri1<-matrix(c(0.03,0.12,-0.28,0.12,0.78,-0.15,-0.28,-0.15,0.63),nrow=3,ncol=3,byrow = TRUE)

matri1%*%B
data
