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

source('src/99_functions.R', echo=F)


#### define tested variants ####

## earnings
variants <- list()

variants$earnings <- list()

variants$earnings$name[[1]] <- 'laplace'
variants$earnings$params[[1]] <- c(0,0.2)

variants$earnings$name[[2]] <- 'norm'
variants$earnings$params[[2]] <- c(0,0.2)

variants$earnings$name[[3]] <- 'logis'
variants$earnings$params[[3]] <- c(0,0.2)

variants$earnings$name[[4]] <- 'cauchy'
variants$earnings$params[[4]] <- c(0,0.2)

## earnings management
variants$earn_mngt <- list()
variants$earn_mngt$name[[1]] <- 'norm'
variants$earn_mngt$params[[1]] <- c(-0.01,0.01)

variants$earn_mngt$name[[2]] <- 'weibull'
variants$earn_mngt$params[[2]] <- c(-0.01,0.01)

variants$earn_mngt$name[[3]] <- 'gamma'
variants$earn_mngt$params[[3]] <- c(1,25)

variants$earn_mngt$name[[4]] <- 'exp'
variants$earn_mngt$params[[4]] <- c(-0.01,0.01)

## management type
#variants$type$name[[1]] <-'-x'
variants$type$name[[2]] <- 'norm'
variants$type$name[[3]] <- '0'
  
#### simulate earnings ####
e = 1
m = 3
t = 2

print(variants$earnings$name[[e]])
print(variants$earn_mngt$name[[m]])
print(variants$type$name[[t]])

for(i in 1:100){
  temp <- simulate_earnings(earnings = variants$earnings$name[[e]],
                            params = variants$earnings$params[[e]],
                            earn_mngt = variants$earn_mngt$name[[m]],
                            earn_mngt_params = variants$earn_mngt$params[[m]],
                            occur = 0.5,
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


