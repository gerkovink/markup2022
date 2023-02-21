library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

describe_data<-function(x, #data vector 
                        mu,sd #populaiton parameters
){
  x.bar<-mean(x)
  df<-length(x) - 1
  bias<-abs(x.bar-mu)
  SE<-sd/sqrt(length(x))
  CI<-c(x.bar-qt(.975, df) * SE, x.bar+qt(.975, df) * SE)
  
  return(c(x.bar,bias, SE, CI))
}

iter<-100
mu<-0
sd<-1

results<-data.frame(sample=factor(1:iter),
                    mean=NA,
                    bias=NA,
                    SE=NA,
                    CI_lb=NA,
                    CI_ub=NA
                    )

set.seed(123)
for(i in 1:iter){
  results[i,-1]<-rnorm(n=1000, mean = mu, sd=sd) %>% 
              describe_data(mu=mu, sd=sd)
  
}

#plot
results %>% 
  ggplot(aes(x=sample)) + 
  geom_point(aes(y=mean))+
  geom_errorbar(aes(ymin=CI_lb, ymax=CI_ub))+
  geom_hline(yintercept = mu, linetype="dashed")+
  coord_flip()

#table
results %>% 
  filter(CI_lb>mu | CI_ub<mu) %>% 
  mutate_if(is.numeric, function(x) round(x, digits=2)) %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")


