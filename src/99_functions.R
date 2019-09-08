#' Add together two numbers.
#' 
#' @param size A number of companies to be simulated.
#' @param earnings A name of distribution to simulate un-managed earnings. One of c('laplace','norm','logis','cauchy')
#' @param params The parameters of the distribution function. 
#' @param earn_mngt The name of the distribution to simulate earnings management. One of c('weibull','gamma','exp') 
#' @param earn_mngt_params The parameters of the distribution function to simulate managed earnings.
#' @param occur What percentage of companies decides to manage.
#' @param type The type of earnings management. One of c('norm_sd','norm_0.5sd,'0')
#' @param range_h The width of the histogram to align simulated distributions.
#' @return The dataset with all observations.
#' @examples
#' simulate_earnings()
#' simulate_earnings(earnings = 'norm', earn_mngt = 'norm')


simulate_earnings <- function(size = 10000,
                              earnings = 'laplace',
                              params = c(0.03,0.04),
                              earn_mngt = 'exp',
                              earn_mngt_params = c(20,NA,0.05),#last is the sd dev 
                              occurr = 0.4,
                              type = 'norm_sd',
                              range_h=0.0025,
                              print = T,
                              simulateMngt = T){
  
  ## simulate earnings
  if(earnings == 'laplace') sim_earn <- round(rlaplace(size, 
                                                       location = params[1],
                                                       scale = params[2]),4)
  if(earnings == 'norm') sim_earn <- round(rnorm(size, 
                                                 mean = params[1],
                                                 sd = params[2]),4)
  if(earnings == 'logis') sim_earn <- round(rlogis(size, 
                                                   location = params[1],
                                                   scale = params[2]),4)
  if(earnings == 'cauchy') sim_earn <- round(rcauchy(size, 
                                                     location = params[1],
                                                     scale = params[2]),4)
  
  sim_data <- data.table(x = sim_earn)  
  
  
  sim_data[,range:=floor(x/range_h)*range_h]
  sim_data<-sim_data[,.N,by=range][order(range)]
  
  N0<-round(sum(sim_data[range<0&range>(-0.2)]$N)*occurr)

  
  ##  simulate earmings management 
  
  if(simulateMngt){
    #### earn mgnt probability
    # if(earn_mngt == 'norm'){
    #    sim_data[,em_prob := round(dlnorm(x,
    #                              mean = earn_mngt_params[1],
    #                              sd = earn_mngt_params[2]),4)]
    #  }
    if(earn_mngt == 'weibull'){
      mngt= round(rweibull(N0,earn_mngt_params[1], earn_mngt_params[2]),4)*(-1)
    }
    if(earn_mngt == 'gamma'){
      mngt =round(rgamma(N0,shape = earn_mngt_params[1],rate = earn_mngt_params[2]),4)*(-1)
    }
    if(earn_mngt == 'exp'){
      mngt = round(rexp(N0,rate = earn_mngt_params[1]),4)*(-1)
    }
    
    mngt <- data.table(x = mngt)  
    mngt[,range:=floor(x/range_h)*range_h]
    mngt<-mngt[,.N,by=range][order(range)]
    setnames(mngt,'N','N_mngt')
    mngt<-mngt[range<0]
    
    
    sim_data<-merge(sim_data,mngt,by='range',all=T)
    
    #if simulation of managed gives more observations in a given interval than simulation fo whole sample - move them to the interval closer to zero
    setkey(sim_data,range)
    sim_data[,id := .I]
    sim_data[,N:=as.numeric(N)]
    sim_data[,N_mngt:=as.numeric(N_mngt)]
    
    
    #sim_data[,range_lag:=shift(range,type='lead')]
    #wrong<-sim_data[(is.na(N)&!is.na(N_mngt))|(N_mngt>N)]
    #while(nrow(wrong)>0){
    #  wrong[,diff:=sum((-1)*N,N_mngt,na.rm=T),by=id]
    #  
    #  #print(nrow(wrong))
    #  
    #  for (i in 1:nrow(wrong)){
    #    #print(i)
    #    sim_data[range==wrong$range[i],N_mngt:=sum(N_mngt,(-1)*wrong$diff[i],na.rm=T),by=id]
    #    sim_data[range==wrong$range_lag[i],N_mngt:=sum(N_mngt,wrong$diff[i],na.rm=T),by=id]
    #  }
    #  
    #  sim_data[N_mngt==0,N_mngt:=NA]
    #  wrong<-sim_data[(is.na(N)&!is.na(N_mngt))|(N_mngt>N)]
    #}
    
    wrong<-sim_data[is.na(N)&!is.na(N_mngt)]
    
    if(print){
      print(paste0('usuwamy ',round(sum(wrong$N_mngt,na.rm=T)/N0*100,2),' %'))
    }
    
    
    sim_data<-sim_data[!is.na(N)]
    #sim_data[,range_lag:=NULL]
    
    sum(sim_data$N,na.rm=T)
    sum(sim_data$N_mngt,na.rm=T)
    
    sim_data[,N_final:=sum(N,(-1)*N_mngt,na.rm=T),by=id]
    sim_data[N_final<0,N_final:=0]
    
    
    if(type == '0'){
      sim_data[range==0,N_mngt2:=N0]
      sim_data[range==0,N_final:=N_final+N0]
    }
    if(type == 'norm_sd'){
      sim_mngt<-abs(rnorm(N0,0,earn_mngt_params[3]))
      sim_mngt <- data.table(x = sim_mngt)  
      sim_mngt[,range:=floor(x/range_h)*range_h]
      sim_mngt<-sim_mngt[,.N,by=range][order(range)]
      setnames(sim_mngt,'N','N_mngt2')
      sim_data<-merge(sim_data,sim_mngt,by='range',all=T)
      sim_data[,N_final:=sum(N_final,N_mngt2,na.rm=T),by=id]
      
    }  
    if(type == 'norm_0.5sd'){
      sim_mngt<-abs(rnorm(N0,0,earn_mngt_params[3]*0.5))
      sim_mngt <- data.table(x = sim_mngt)  
      sim_mngt[,range:=floor(x/range_h)*range_h]
      sim_mngt<-sim_mngt[,.N,by=range][order(range)]
      setnames(sim_mngt,'N','N_mngt2')
      sim_data<-merge(sim_data,sim_mngt,by='range',all=T)
      sim_data[,N_final:=sum(N_final,N_mngt2,na.rm=T),by=id]
      
    }  
  }
  #sim_data[,id:=NULL]
  #sim_data<-sim_data[,.(range,N_final)]
  return(sim_data)
  
}



##_________________________________________________________##
#### BD test ####
##_________________________________________________________##

BD_test<-function(x,range_h=0.0025,lim1=-0.1,lim2=-lim1,output='t_stat'){
  #x<-data.table('x'=x)
  #x[,range:=floor(x/range_h)*range_h]
  #x<-x[,.N,by=range][order(range)]
  x[,N_f:=as.numeric(stats::filter(N,c(1/2,0,1/2)))]
  x[,N_diff:=N-N_f]
  x[,N_lag:=shift(N,type='lag')]
  x[,N_lead:=shift(N,type='lead')]
  N_all<-x[,sum(N)]
  x[,sd_diff:=N_all*(N/N_all)*(1-N/N_all)+
      (1/4)*N_all*(N_lag/N_all+N_lead/N_all)*(1-N_lag/N_all-N_lead/N_all)]
  x[,t_stat:=N_diff/sqrt(sd_diff)]
  if(output=='t_stat'){return(x[range>=lim1&range<=lim2,c('range','t_stat'),with=F])}
  if(output=='N'){return(x[range>=lim1&range<=lim2,c('range','N'),with=F])}
  if(output=='all'){return(x[range>=lim1&range<=lim2])}
}

