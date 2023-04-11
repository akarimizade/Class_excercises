library(fields)
library(coda)
library(PopGenInfeR)

sample <- sim.inference.model(number.of.sampled.demes = 10,sample.sizes =
                                50,M = 2,pi = 0.5)

sample1<-sim.inference.model(number.of.sampled.demes = 20,sample.sizes =
                               50,M = 2,pi = 0.5)

sample2<-sim.inference.model(number.of.sampled.demes = 30,sample.sizes =
                               50,M = 2,pi = 0.5)


sample3<-sim.inference.model(number.of.sampled.demes = 100,sample.sizes =
                               50,M = 2,pi = 0.5)

result <- maximum.likelihood(sample,alpha = 0.05,M = seq(0.01,10,0.01),pi
                             = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)
result1 <- maximum.likelihood(sample1,alpha = 0.05,M = seq(0.01,10,0.01),pi
                             = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)
result2 <- maximum.likelihood(sample2,alpha = 0.05,M = seq(0.01,10,0.01),pi
                      = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)
result3 <- maximum.likelihood(sample3,alpha = 0.05,M = seq(0.01,10,0.01),pi
                             = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)

results <- maximum.likelihood(sample,alpha = 0.05,M = seq(0.01,10,0.01),pi
                             = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)

results1 <- maximum.likelihood(sample1,alpha = 0.05,M = seq(0.01,10,0.01),pi
                             = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)

results2 <- maximum.likelihood(sample2,alpha = 0.05,M = seq(0.01,10,0.01),pi
                               = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)

results3 <- maximum.likelihood(sample3,alpha = 0.05,M = seq(0.01,10,0.01),pi
                               = seq(0.01,0.99,0.01),graphics = TRUE,true.M = 2,true.pi = 0.5)
