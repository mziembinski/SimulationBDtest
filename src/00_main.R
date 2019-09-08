##________________________________________________##
####  Earnungs discontinuity - Simulation       ####
##________________________________________________##

#rm(list=ls())

#### set up the enviroment #####
#devtools::install_github('cttobin/ggthemr', upgrade_dependencies = F)

packages<-c("data.table","ggplot2",
            "reshape2",'ggthemr',
            'tidyverse','gridExtra',
            'VGAM','MASS','normalp','nortest',
            'bbmle','evir',
            'gridExtra')

sapply(packages,require,character.only=T)

options(scipen = 999)

ggthemr('fresh')

## load functons 

setwd('C:/Users/Paulina/Desktop/Misiowe/SimulationBDtest')
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


for (ee in 1:4){
  for (m in 1:3){
    for (t in 1:3){
      range_h <- 0.01
      
      temp <- simulate_earnings(earnings = variants$earnings$name[[ee]],
                                params = variants$earnings$params[[ee]],
                                earn_mngt = variants$earn_mngt$name[[m]],
                                earn_mngt_params = variants$earn_mngt$params[[m]],
                                occur = 0.4,
                                type = variants$type$name[[t]],
                                range_h=range_h)
      
      #ggplot(temp,
      #      aes(range + range_h/2,N_final))+
      # geom_vline(xintercept = 0, col = 'red') +
      # geom_bar(stat='identity')+
      #  theme_bw() 
      
      temp1<-copy(temp)
      temp1[,N_final:=NULL]
      temp1[,N:=sum(N,(-1)*N_mngt,na.rm=T),by=id]
      temp1[N<0,N:=0]
      temp1[,id:=NULL]
      temp1<-data.table(melt(temp1,id.vars = c('range')))
      
      temp1$variable <- factor(temp1$variable,
                               levels = c('N_mngt','N_mngt2','N'))
      
      ggplot(temp1,
             aes(range + range_h/2,value ,fill=variable))+
        geom_vline(xintercept = 0, col = 'red') +
        geom_bar(stat='identity', position = 'stack')+
        scale_fill_manual( values=c("grey30", "grey50", "grey70"))+
        theme_bw()+theme(legend.position = "none",legend.title = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+
        ggtitle(paste0('Symulacja (h=1%, N=100000, k=40%): ',variants$earnings$name[[ee]],', ',variants$earn_mngt$name[[m]],', ',variants$type$name[[t]]))+
        coord_cartesian(xlim=c(-0.2,0.2))+
        ggsave(paste0('../PHDthesis/simulation_input/sym_example_',ee,m,t,'.pdf'),width=7,height=5)
      

    }
  }
}

#### test size ####

ranges <- c(0.0025, 0.005, 0.01)
sizes <- c(1e3, 1e4, 1e5)


rm(resultsSize)

for (ee in 1:10){
  for (s in 1:3){
    for (r in 1:3){
      
      for(j in 1:1e5){
        temp <- simulate_earnings(size = sizes[s],
                                  earnings = variants$earnings$name[[ee]],
                                  params = variants$earnings$params[[ee]],
                                  range_h=ranges[r],
                                  print = F,
                                  simulateMngt = F)
        
        temp[, N := N]
        
        results <- BD_test(temp, range = ranges[r], output = 'all')
        results[,size := sizes[s]]
        results[,earnings := variants$earnings$name[[ee]]]
        results[,params := variants$earnings$params[[ee]]]
        results[,range_h := ranges[r]]
        results[,j := j]
        
        ifelse(!exists('resultsSize'),
               resultsSize <- results,
               resultsSize <- rbind(resultsSize,
                                    results))
      }
    }
  }
  print(ee)
  print(Sys.time())
}

save(resultsSize, file = 'data/resultsSize.RData')

#### test power ####
rm(resultsPower)

occurs <- c(0.2, 0.4, 0.6)

for (ee in 1:10){
  for (m in 1:3){
    for (t in 1:3){
      for (o in 1:3){
        for (s in 1:3){
          for (r in 1:3){
            
            for(j in 1:1e5){
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
              
              ifelse(!exists('resultsPower'),
                     resultsPower <- results,
                     resultsPower <- rbind(resultsPower,
                                           results))
            }
          }
        }
      }
    }
  }
  print(ee)
  print(Sys.time())
}


save(resultsPower, file = 'data/resultsPower.RData')

#### simulate earnings ####
e = 1
m = 1
t = 2

print(variants$earnings$name[[e]])
print(variants$earn_mngt$name[[m]])
print(variants$type$name[[t]])

for(i in 1:100){
  temp <- simulate_earnings(earnings = variants$earnings$name[[e]],
                            params = variants$earnings$params[[e]],
                            earn_mngt = variants$earn_mngt$name[[m]],
                            earn_mngt_params = variants$earn_mngt$params[[m]],
                            occur = 0.4,
                            type = variants$type$name[[t]])
  
  temp[,i := i]
  
  dataplotBD <- temp[,BD_test(x_mngd,
                              output='all',
                              range_h=0.005,
                              lim1=-0.2)][,.(range, N, N_f, N_diff, N_lag, N_lead, sd_diff, t_stat, type = 'managed')]
  dataplotBD <- rbind(dataplotBD,
                      temp[,BD_test(x,
                                    output='all',
                                    range_h=0.005,
                                    lim1=-0.2)][,.(range, N, N_f, N_diff, N_lag, N_lead, sd_diff, t_stat, type = 'un_mananged')])
  
  dataplotBD[,i := i]
  
  ifelse(i == 1,
         data_simulation <- temp,
         data_simulation <- rbind(data_simulation,
                                  temp))
  ifelse(i == 1,
         data_test <- dataplotBD,
         data_test <- rbind(data_test,
                            dataplotBD))
  if(i%%100 == 0){
    print(Sys.time())
    print(i)
  }
}

#data_test[abs(range) < 0.01 & i == 5]

ggplot(data_test[range == 0],
       aes(abs(t_stat))) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = qnorm(0.99), col = 'red', lty = 3, size = 2) +
  facet_grid(type ~ .) +
  theme_light()

#### test plots ####

## N
p1 <- ggplot(dataplotBD,
       aes(range, N, col = type)) +
  geom_step() +
  geom_vline(aes(xintercept = 0), col = 'red', lty = 3) +
  theme(legend.position = 'bottom')

dataplot <- data_test[,.(Q25 = quantile(N,0.25),
                         Q50 = quantile(N,0.50),
                         Q75 = quantile(N,0.75)), by = .(range,type)]

p2 <- ggplot(dataplot,
       aes(range, Q50, col = type, fill = type)) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.4) +
  geom_step() +
  geom_vline(aes(xintercept = 0), col = 'red', lty = 3) +
  theme(legend.position = 'bottom')

## t_stat
p3 <- ggplot(dataplotBD,
       aes(range, t_stat, col = type)) +
  geom_step() +
  geom_vline(aes(xintercept = 0), col = 'red', lty = 3) +
  theme(legend.position = 'bottom')

dataplot <- data_test[,.(Q25 = quantile(t_stat,0.25),
                         Q50 = quantile(t_stat,0.50),
                         Q75 = quantile(t_stat,0.75)), by = .(range,type)]

p4 <- ggplot(dataplot,
       aes(range, Q50, col = type, fill = type)) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.4) +
  geom_step() +
  geom_vline(aes(xintercept = 0), col = 'red', lty = 3) +
  theme(legend.position = 'bottom')

p <- grid.arrange(p1,p2,p3,p4)

##_____________##
#### working ####
##_____________##
temp <- simulate_earnings(earnings = 'norm', earn_mngt = 'norm')
temp <- simulate_earnings(earnings = 'laplace', earn_mngt = 'norm')

temp <- simulate_earnings(earnings = 'laplace',
                          earn_mngt = 'gamma',
                          earn_mngt_params = c(1,25),
                          type = 'norm')

#temp <- simulate_earnings(earnings = 'logis', earn_mngt = 'norm')
#temp <- simulate_earnings(earnings = 'cauchy', earn_mngt = 'norm')
temp[x > -0.01 & x < 0]
temp[flag_mngd == 1]

ggplot(temp,aes(x)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = 0, col = 'red', lty = 3, size = 2) +
  theme_light()

ggplot(temp,aes(x,em_prob)) +
  geom_vline(xintercept = 0, col = 'red', lty = 3, size = 1) +
  geom_point() +
  xlim(-0.4,0.4) +
  theme_light()

#managed
ggplot(temp,aes(x_mngd)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = 0, col = 'red', lty = 3, size = 2) +
  theme_light()


#### run test ####


