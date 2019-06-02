#' Add together two numbers.
#' 
#' @param size A number of companies to be simulated.
#' @param earnings A name of distribution to simulate un-managed earnings. One of c('laplace','norm','logis','cauchy')
#' @param params The parameters of the distribution function. 
#' @param earn_mngt The name of the distribution to simulate earnings management. One of c('norm','weibull','gamma','exp') 
#' @param earn_mngt_params The parameters of the distribution function to simulate managed earnings.
#' @param occur What percentage of companies decides to manage.
#' @param type The type of earnings management. One of c('norm','0')
#' @return The dataset with all observations.
#' @examples
#' simulate_earnings()
#' simulate_earnings(earnings = 'norm', earn_mngt = 'norm')


simulate_earnings <- function(size = 10000,
                              earnings = 'laplace',
                              params = c(0,0.2),
                              earn_mngt = 'norm',
                              earn_mngt_params = c(-0.01,0.01),
                              occurr = 0.1,
                              type = 'none'){
  
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

  
  ##  simulate earmings management 
  
  #### earn mgnt probability
  if(earn_mngt == 'norm'){
    sim_data[,em_prob := round(dnorm(x,
                              mean = earn_mngt_params[1],
                              sd = earn_mngt_params[2]),4)]
  }
  if(earn_mngt == 'weibull'){
    sim_data[,em_prob := round(dweibull(x,
                                     earn_mngt_params[1],
                                     earn_mngt_params[2]),4)]
  }
  if(earn_mngt == 'gamma'){
    sim_data[,em_prob := round(dgamma(-x,
                                      shape = earn_mngt_params[1],
                                      rate = earn_mngt_params[2]),4)]
  }
  if(earn_mngt == 'exp'){
    sim_data[,em_prob := round(dexp(x,
                                     rate = earn_mngt_params[1]),4)]
  }
  
  ##  managed earnings 
  sim_data[,id := .I]
  #sim_data[,flag_mngd := rbinom(1, size=2, prob=em_prob/100), by = id]
  sim_data[,flag_mngd := runif(1) * occurr, by = id]
  sim_data[,flag_mngd := ifelse(em_prob > flag_mngd, 1, 0)]
  sim_data[,x_mngd := x]
  
  if(type == 'norm')  sim_data[flag_mngd == 1, x_mngd := abs(rnorm(1,0.01,0.01)), by = id]
  if(type == '0')  sim_data[flag_mngd == 1, x_mngd := 0]
  
  return(sim_data)
  
}



##_________________________________________________________##
#### BD test ####
##_________________________________________________________##

BD_test<-function(x,range_h=0.0025,lim1=-0.1,lim2=-lim1,output='t_stat'){
  x<-data.table('x'=x)
  x[,range:=floor(x/range_h)*range_h]
  x<-x[,.N,by=range][order(range)]
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

