#Modeling class Excercise
N<-10
NT<-100
NewP<-N/NT
deltat<-0.01
number_of_steps<-10000
vector<-c()
c<-0.5
e<-0.2
for(i in 1:number_of_steps ){
  
  deltap<-deltat*(c*NewP*(1-NewP)-e*NewP)
  NewP<-deltap+NewP
  vector<-append(vector,NewP)

}

time<-c(1:10000)
time1<-c(1:10000)
P0<-N/NT
P<-P0*(c-e)/(c*P0+(c-e-c*P0)*exp((e-c)*time1*deltat))
par( mfrow= c(1,2) )
plot(vector~time)+abline(h=1-e/c,col="Pink")
plot(P~time1,col="purple")+abline(h=1-e/c,col="red")






