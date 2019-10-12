##________________________________________________##
####  Earnungs discontinuity - Simulation       ####
##________________________________________________##

#rm(list=ls())

#### set up the enviroment #####
#devtools::install_github('cttobin/ggthemr', upgrade_dependencies = F)

packages<-c("data.table","ggplot2",
            #"reshape2",
            'ggthemr',
            #'tidyverse',
            'gridExtra',
            'VGAM','MASS','normalp','nortest',
            'bbmle','evir',
            'gridExtra')

sapply(packages,require,character.only=T)

library(foreach)
library(doParallel)


options(scipen = 999)

ggthemr('fresh')


#setup parallel backend to use many processors
# cores = detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)

## load functons 

#setwd('C:/Users/Paulina/Desktop/Misiowe/SimulationBDtest')
source('src/99_functions.R', echo=F)


#### define tested variants ####

## earnings
variants <- list()

variants$earnings <- list()

variants$earnings$name[[1]] <- 'norm'
variants$earnings$params[[1]] <- c(0.035,0.05)

variants$earnings$name[[2]] <- 'logis'
variants$earnings$params[[2]] <- c(0.036,0.03)

variants$earnings$name[[3]] <- 'laplace'
variants$earnings$params[[3]] <- c(0.03,0.05)

variants$earnings$name[[4]] <- 'cauchy'
variants$earnings$params[[4]] <- c(0.03,0.04)

variants$earnings$name[[5]] <- 'laplace'
variants$earnings$params[[5]] <- c(-0.03,0.05)

variants$earnings$name[[6]] <- 'laplace'
variants$earnings$params[[6]] <- c(0,0.05)

variants$earnings$name[[7]] <- 'laplace'
variants$earnings$params[[7]] <- c(0.01,0.05)

variants$earnings$name[[8]] <- 'laplace'
variants$earnings$params[[8]] <- c(-0.01,0.05)

variants$earnings$name[[9]] <- 'laplace'
variants$earnings$params[[9]] <- c(0.005,0.05)

variants$earnings$name[[10]] <- 'laplace'
variants$earnings$params[[10]] <- c(-0.005,0.05)

## earnings management
variants$earn_mngt <- list()

variants$earn_mngt$name[[1]] <- 'exp'
variants$earn_mngt$params[[1]] <- c(20,NA,0.05)

variants$earn_mngt$name[[2]] <- 'weibull'
variants$earn_mngt$params[[2]] <- c(1.1,0.05,0.042)

variants$earn_mngt$name[[3]] <- 'gamma'
variants$earn_mngt$params[[3]] <- c(1.2,25,0.044)


## management type
variants$type$name[[1]] <-'0'
variants$type$name[[2]] <- 'norm_sd'
variants$type$name[[3]] <- 'norm_0.5sd'


#### plots ###

#range_h
#size

ee <- 1
m <- 1
t <- 1

ranges <- c(0.0025, 0.005, 0.01)
sizes <- c(1e3, 1e4, 1e5)
occurs <- c(0.2, 0.4, 0.6)


cores = detectCores()
cl <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)

rm(resultsPower)

occurs <- c(0.2, 0.4, 0.6)

for (ee in 3:10){
  for (m in 1:3){
    for (t in 1:3){
      for (o in 1:3){
        rm(resultsPower)
        if(paste0(ee,'_',
                  m,'_',
                  t,'_',
                  o,'_') %in% c('2_1_1_1_',
                                '2_1_1_2_',
                                '2_1_1_3_',
                                '2_1_2_1_')){
          next()
        }
        
        for (s in 1:3){
          for (r in 1:3){
            results <- foreach(j=1:1e5, .combine=rbind) %dopar% {
              packages<-c("data.table",
                          'VGAM','MASS','normalp','nortest',
                          'bbmle','evir')
              
              sapply(packages,require,character.only=T)
              
              temp <- simulate_earnings(size = sizes[s],
                                        earnings = variants$earnings$name[[ee]],
                                        params = variants$earnings$params[[ee]],
                                        earn_mngt = variants$earn_mngt$name[[m]],
                                        earn_mngt_params = variants$earn_mngt$params[[m]],
                                        occur = occurs[o],
                                        type = variants$type$name[[t]],
                                        range_h=ranges[r],
                                        print = F,
                                        simulateMngt = T)
              
              temp[, N := N_final]
              
              results <- BD_test(temp, range = ranges[r], output = 'all')
              results[,size := sizes[s]]
              results[,earnings := variants$earnings$name[[ee]]]
              results[,params := variants$earnings$params[[ee]]]
              results[,earn_mngt := variants$earn_mngt$name[[m]]]
              results[,earn_mngt_params := variants$earn_mngt$params[[m]]]
              results[,occur := occurs[o]]
              results[,type := variants$type$name[[t]]]
              
              results[,range_h := ranges[r]]
              results[,j := j]
              
              results
            }
            
            ifelse(!exists('resultsPower'),
                   resultsPower <- results,
                   resultsPower <- rbind(resultsPower,
                                         results))
          }
        }
        
        print(ee)
        save(resultsPower, file = paste0('data/resultsPower_',
                                         ee,'_',
                                         m,'_',
                                         t,'_',
                                         o,'_',
                                         '.RData'))
        print(Sys.time())
        
      }
    }
  }
  
}

