
library(tidyverse)
library(broom)


power_plot = function(min_n = 10, max_n=100, n_steps = 10, min_es = 0.0, max_es=1, es_steps=30, sim_n = 100){
  #Sample size
  n_per_group <- seq(from = min_n, to = max_n, length.out = n_steps) #creates sequences of numbers.
  # 
  pop_es <- seq(from = min_es, to = max_es, length.out = es_steps) #creates sequences of numbers.
  # 
  #Container power
  
  print('Generating plot:')
  
  powerValue <- NULL
  overall_power = NULL
  overall_N = NULL
  overall_pop_es = NULL
  
  for(N in n_per_group) {
    
    powerValue=NULL
    nValue=NULL
    
    print(N)
  
    for(es in pop_es){
      
      #Container pvalues
      pvalues <- NULL
      
      
      
      for(i in 1:sim_n){
        
        
        
        group1 <- rnorm(n = N, mean = 0, sd = 1)
        group2 <- rnorm(n = N, mean = es, sd = 1)
        
        score <- c(group1, group2)
        groupID <- rep(c("gr1", "gr2"), each = N)
        
        MyData <- data.frame(groupID, score)
        t_model <- t.test(score ~ groupID, data = MyData)
        pvalues <- c(pvalues, tidy(t_model)$p.value[1])
        
      }
      
      #Power
      powerValue <- c(powerValue, sum(pvalues <= 0.05)/length(pvalues))
      nValue <- c(nValue, N)
      overall_pop_es = c(overall_pop_es, pop_es)
      
    }
    
    overall_power = c(overall_power, powerValue)
    overall_N = c(overall_N, nValue)
    overall_pop_es = c(overall_pop_es, pop_es)
  }
  
  MyDataPower <- data.frame(overall_pop_es, overall_power, overall_N)
  
  print('finished sim')
  #Plot
  plot = ggplot(MyDataPower, aes(x=overall_pop_es, y=overall_power, colour=as.factor(overall_N))) + 
    geom_line() +
    xlab("Effect Sizes")+
    ylab("Power")+
    labs(colour = "Sample") +
    theme_bw() +
    geom_hline(yintercept= 0.8, linetype="dashed", color = "red") +
    ggtitle(paste0("Power Curve for an Independent Sample t-test (n per group:",min_n, "_", max_n, ")"))
  
  plot
  
  # return(MyDataPower)
}



