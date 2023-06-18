
library(tidyverse)
library(broom)


power_plot = function(n_per_group = 50, min_es = 0.1, max_es=0.8, es_steps=30, sim_n = 100){
  #Sample size
  # n_per_group <- 50
  # 
  pop_es <- seq(from = min_es, to = max_es, length.out = es_steps) #creates sequences of numbers.
  # 
  #Container power
  
  print('Generating plot:')
  
  powerValue <- NULL
  
  for(es in pop_es){
    
    #Container pvalues
    pvalues <- NULL
    
    print(es)
    
    for(i in 1:sim_n){
      
      
      
      group1 <- rnorm(n = n_per_group, mean = 0, sd = 1)
      group2 <- rnorm(n = n_per_group, mean = es, sd = 1)
      
      score <- c(group1, group2)
      groupID <- rep(c("gr1", "gr2"), each = n_per_group)
      
      MyData <- data.frame(groupID, score)
      t_model <- t.test(score ~ groupID, data = MyData)
      pvalues <- c(pvalues, tidy(t_model)$p.value[1])
      
    }
    
    #Power
    powerValue <- c(powerValue, sum(pvalues <= 0.05)/length(pvalues))
    
  }
  
  MyDataPower <- data.frame(pop_es, powerValue)
  
  print('finished sim')
  #Plot
  plot = ggplot(MyDataPower, aes(x=pop_es, y=powerValue)) + 
    geom_line() +
    xlab("Effect Sizes")+
    ylab("Power")+
    theme_bw() +
    geom_hline(yintercept= 0.8, linetype="dashed", color = "red") +
    ggtitle("Power Curve for an Independent Sample t-test (n per group: 50)")
  
  plot
  
  
}



