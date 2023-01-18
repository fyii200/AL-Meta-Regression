# The following code was run to produce the results reported in the manuscript 
# entitled "Normal eye growth from 6 to 24 years old: 
# a meta-regression of 28 studies including 4402 emmetropic eyes".

# Author: Fabian SL Yii
# Email: fabian.yii@ed.ac.uk

# For the purpose of open access, the author has applied a creative commons 
# attribution (CC BY) licence to any Author Accepted Manuscript version 
# arising from this work. 
################################################################################
rm(list=ls())
# # Uncomment the following if running for the first time
# install.packages('meta')
# install.packages('metafor')
# install.packages('ggplot2')
# install.packages('hrbrthemes')
# install.packages('viridis')
# install.packages('ggrepel')
# install.packages('lmtest')
# install.packages('nlraa')
# install.packages('ggthemes')
# install.packages('extrafont')
# install.packages('remotes')
library('meta')
library('metafor')
library('ggplot2')
library('hrbrthemes')
library('viridis')
library('ggrepel')
library('nlme')
library('lmtest')
library('nlraa')
library('ggthemes')
library('extrafont')
library('remotes')
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
################################################################################
# Read data into workspace
d <- read.csv('/Users/fabianyii/Desktop/AlRateEmmetropes/data/cleaned_data.csv')
d$quality <- as.factor(d$quality)
d$dataset <- as.factor(d$dataset) 
d$emm_code <- as.factor(d$emm_code) 
d$ser_mean <- as.numeric(d$ser_mean) 

#### comment out for primary analysis: only run for sensitivity analysis ####
# # Remove studies rated as having high risk of bias or low applicability #
# d <- subset(d, quality!=3)

# Create a new column indicating which studies correspond to East Asia (value=1)
d$east_asia <- 1
d[which(d$location=='spain' | d$location=='sweeden' | d$location=='australia' | d$location=='norway' | d$location=='turkey' | d$location=='uk' | d$location=='denmark'), ]$east_asia <- 0
d$east_asia <- as.factor(d$east_asia)
# Compute AL & age standard errors
d$age_se = d$age_sd / sqrt(d$n)
d$AL_se = d$AL_sd / sqrt(d$n)
# Function to normalise Age SE and AL SE into range [0,1], make sure they are on the same scale
norm <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
# Compute study weights
d$weight = 1/(norm(d$age_se) + norm(d$AL_se) + 0.2)
# Create a new column indicating which studies have weights>1.5
d$label=""
d[d$weight>1.5,]$label = d[d$weight>1.5,]$location
d[d$weight>1.5,]$label <- c('SE Norway', 'Sydney', 'Sydney', 'Anyang', 'Anyang', 
                            'SH', 'HK', 'HK', 'BJ', 'BJ', 'BJ',
                            'BJ', 'BJ', 'BJ', 'Spain', 'Anyang', 'SG', 'UK: NI')

# Nonlinear model selection: fitting AL growth curve using different weighted mixed-effects models #
# Fit to the full dataset. Note that weights need to be specified as ~1/weight when using "nlme" which is different from "nlmer" 
m_allmix1 <- nlme(AL_mean ~ a+(b/exp(c*age_mean)), data = d, fixed = a + b + c ~ 1, random = a ~ 1 | dataset, start = c(a=24, b=-2, c=0.1), weights= ~1/weight)
m_allmix2 <-  nlme(AL_mean ~ a+(1/exp(c*age_mean)), data = d, fixed = a + c ~ 1, random = a ~ 1 | dataset, start = c(a=24, c=0.04), weights= ~1/weight)
m_allmix3 <-  nlme(AL_mean ~ a+(b/exp(age_mean)), data = d, fixed = a + b ~ 1, random = a ~ 1 | dataset, start = c(a=24, b=-300), weights= ~1/weight)
m_allmix4 <-  nlme(AL_mean ~ a+(b/age_mean), data = d, fixed = a + b ~ 1, random = a ~ 1 | dataset, start = c(a=24, b=-300), weights= ~1/weight)
# Likelihood ratio test
anova(m_allmix2, m_allmix1)
anova(m_allmix3, m_allmix1)
anova(m_allmix4, m_allmix1)
# "m_allmix1" is the best-fitting (and most parsimonious) nonlinear model
summary(m_allmix1)
intervals(m_allmix1, which='fixed') # 95% CI: parameter estimates

# EA and non-EA nonlinear models #
# refit the best model (originally fitted to the full dataset) with "east_asia" as a grouping variable 
ages <- seq(6,24,0.5)
dgrouped <- groupedData(AL_mean ~ age_mean | east_asia, data = d[,c(2,10,12,21,24)])
dgrouped$east_asia <- factor(dgrouped$east_asia, ordered=FALSE)
fxf <- fixef(m_allmix1)
refit_allmix1 <- update(m_allmix1, fixed = a + b + c ~ east_asia,
                        start = c(fxf[1], 0,
                                  fxf[2], 0,
                                  fxf[3], 0))
summary(refit_allmix1) 
intervals(refit_allmix1, which='fixed') # 95% CI for each parameter estimate 
anova.lme(refit_allmix1, type='marginal') # Wald test of parameter differences (marginal sums of squares)

# Refit the best model with "east_asia" as a grouping variable on one parameter at a time
refit_allmix_a <- update(m_allmix1, fixed = list(a~1, b~east_asia, c~east_asia),
                        start = c(fxf[1],
                                  fxf[2], 0,
                                  fxf[3], 0))
refit_allmix_b <- update(m_allmix1, fixed = list(a~east_asia, b~1, c~east_asia),
                         start = c(fxf[1], 0,
                                   fxf[2],
                                   fxf[3], 0))
refit_allmix_c <- update(m_allmix1, fixed = list(a~east_asia, b~east_asia, c~1),
                         start = c(fxf[1], 0,
                                   fxf[2], 0,
                                   fxf[3]))
# Compare models fitted without and with the grouping variable 
# on a specific parameter using likelihood-ratioo test
lrtest(refit_allmix_a, refit_allmix1)
lrtest(refit_allmix_b, refit_allmix1)
lrtest(refit_allmix_c, refit_allmix1)


set.seed(22)
# Predict age-specific AL values and their 95% CIs using the full model 
preds_all <- data.frame(predict_nlme(m_allmix1, newdata=data.frame(age_mean=ages), interval='confidence'))
preds_all <- cbind(ages,preds_all); names(preds_all)[c(2,4:5)] <- c('fit', 'lwr', 'upr')
# Predict age-specific AL values and their 95% CIs using the EA model
preds_EA <- data.frame(predict_nlme(refit_allmix1, newdata=data.frame(age_mean=ages, east_asia=1), interval='confidence'))
preds_EA <- cbind(ages,preds_EA); names(preds_EA)[c(2,4:5)] <- c('fit', 'lwr', 'upr')
# Predict age-specific AL values and their 95% CIs using the non-EA model
preds_nonEA <- data.frame(predict_nlme(refit_allmix1, newdata=data.frame(age_mean=ages, east_asia=0), interval='confidence'))
preds_nonEA <- cbind(ages,preds_nonEA); names(preds_nonEA)[c(2,4:5)] <- c('fit', 'lwr', 'upr')

## Compute first and second derivatives of the nonlinear growth curve (fitted to the full dataset )
DD <- function(expr, name, order = 1) {
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1) }

DD(expression(23.5989687+(-5.5980919/exp(0.2986106 *age_mean))), name='age_mean', order=1) # equation for the first derivative of full model
DD(expression(23.68938+(-6.912471/exp(0.3322147 *age_mean))), name='age_mean', order=1) # equation for the first derivative of EA model
DD(expression(23.53750767+(-4.14714578/exp(0.24296210 *age_mean))), name='age_mean', order=1) # equation for the first derivative of non-EA model
DD(expression(23.5989687+(-1.0948085/exp(0.2986106 *age_mean))), name='age_mean', order=2) # equation for the second derivative of the best-fitting nonlinear model
m_all1_first_deriv <- function(age_mean, b=5.5980919, c=0.2986106 ){ b * (exp(c * age_mean) * c)/exp(c * age_mean)^2 }
m_all1_sec_deriv <- function(age_mean){ 5.5980919 * (exp(0.2986106  * age_mean) * 0.2986106  * 0.2986106 )/exp(0.2986106  * age_mean)^2 - 5.5980944 * (exp(0.2986106  * age_mean) * 0.2986106 ) * (2 * (exp(0.2986106  * age_mean) * 0.2986106  * exp(0.2986106  * age_mean)))/(exp(0.2986106  * age_mean)^2)^2 }
round(m_all1_first_deriv(6:24), 2) # instantaneous rate of change in AL from 6 to 24 years old
round(m_all1_sec_deriv(6:24), 2) # rate of change in annual AL growth rate from 6 to 24 years old

# Age-specific mean growth rate (full model, EA model and non-EA model)
for(i in 6:24){ 
  full_gr <- round(mean(m_all1_first_deriv(seq(i,i+1,0.001))),3)
  EA_gr <- round(mean(m_all1_first_deriv(seq(i,i+1,0.001), b=6.912471, c=0.3322147)),3)
  nonEA_gr <- round(mean(m_all1_first_deriv(seq(i,i+1,0.001), b=4.14714578, c=0.2429621)),3)
  print(paste('Full=',full_gr, 'EA=',EA_gr, 'non-EA=',nonEA_gr)) }
###################################################################################
# # Age-specific mean growth rate (full model, EA model and non-EA model)
# # SENSITIVITY ANALYSIS: run this instead #
# for(i in 6:24){ print(round(mean(m_all1_first_deriv(seq(i,i+1,0.001), b=6.1405269, c=0.3204229)),2)) }
###################################################################################

# Fitting piecewise linear growth model, with an interaction term between #
# "age" and "east_asia" (on AL). 12y/o is used as the breakpoint because #
# nonlinear growth curve is practically linear before and after age 12 #
lin_before12 <- rma.mv(AL_mean, AL_se^2, mods=~age_mean*east_asia, random=~1|dataset, W=weight, data=subset(d, age_mean>=6  & age_mean<= 12))
lin_after12 <- rma.mv(AL_mean, AL_se^2, mods=~age_mean*east_asia, random=~1|dataset, W=weight, data=subset(d, age_mean>=12  & age_mean<= 24))
# predict AL values using the piecewise linear model
ages_trunc <- seq(6,12,0.5)
preds_EA_lin <- data.frame(predict(lin_before12, newmods=matrix(c(ages_trunc, rep(1,length(ages_trunc)), ages_trunc), ncol=3)))
preds_nonEA_lin <- data.frame(predict(lin_before12, newmods=matrix(c(ages_trunc, rep(0,length(ages_trunc)), rep(0,length(ages_trunc))), ncol=3)))

###################################################################################
###################################### PLOTS ######################################
###################################################################################
ggobject <- function(data, ylimits=c(22.5, 23.9)){
  ggplot(data) + 
    geom_point(aes(x=age_mean, y=AL_mean, size=weight, fill=east_asia), alpha=0.25, shape=21, color="black") + 
    xlab("Age (year)") + 
    ylab("AL (mm)") + 
    scale_size(range = c(0.1, 15), name="Weight", guide='none') +
    scale_fill_manual(name='East Asia', labels=c('No', 'Yes'), values=c('maroon', 'darkblue')) +
    # theme_ipsum() + 
    theme(legend.position=c(1, 0.1), 
          legend.justification = c(1, 0.1),
          legend.background=element_blank(), 
          legend.title=element_text(face="bold", size=12),
          legend.text=element_text(size=10, face='bold'),
          panel.background = element_rect(fill="#fbf9f4", color="#fbf9f4"),
          plot.background = element_rect(fill="#fbf9f4", color="#fbf9f4"),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.y=element_blank()) + 
    guides(fill=guide_legend(keywidth=2, keyheight=0.5, override.aes=list(size=5))) +
    scale_x_continuous(labels=6:24, breaks=6:24) + 
    scale_y_continuous(labels=seq(ylimits[1],ylimits[2],0.1), breaks=seq(ylimits[1],ylimits[2],0.1), limits=c(ylimits[1],ylimits[2])) }  

# PLOT 1: model comparisons
ggobject(d) + scale_y_continuous(labels=seq(22.8,23.9,0.1), breaks=seq(22.8,23.9,0.1), limits=c(22.8,23.9)) +
  labs(subtitle = 'Full dataset') +
  scale_fill_manual(values=c('black', 'black'), guide='none') +
  geom_line(data=data.frame(ages), linetype='solid', size=1, aes(x=ages, y=predict(m_allmix1, list(age_mean=ages), level=0), color='1')) +
  geom_line(data=data.frame(ages), linetype="solid", size=1, aes(x=ages, y=predict(m_allmix2, list(age_mean=ages), level=0), color='2')) +
  geom_line(data=data.frame(ages), linetype="solid", size=1, aes(x=ages, y=predict(m_allmix3, list(age_mean=ages), level=0), color='3')) +
  geom_line(data=data.frame(ages), linetype="solid", size=1, aes(x=ages, y=predict(m_allmix4, list(age_mean=ages), level=0), color='4')) +
  theme(plot.subtitle=element_text(face='bold'),
        legend.key.size=unit(1,'cm'),
        legend.key.height= unit(1.3, 'cm'),
        legend.position=c(0.8, 0.07),
        legend.title.align=0.3, 
        legend.text.align=0, 
        legend.text=element_text(size=12, face='bold')) +
  guides(fill=FALSE) + 
  scale_color_manual(values=c('black', 'blue',' maroon', 'orange'), name="", 
                     labels = expression(23.6 - frac(5.60, epsilon^scriptstyle(0.3%*%age)),
                                         21.99 + frac(1, epsilon^scriptstyle(-0.03%*%age)),
                                         23.45 - frac(515.51, epsilon^scriptstyle(age)),
                                         24.03 - frac(7.55, age) ))
ggsave('/Users/fabianyii/Desktop/AlRateEmmetropes/plots/modelSelectFullDataset.pdf', width=160, height=160, units='mm')

# PLOT 2: Nonlinear AL growth curve fitted tot he full dataset
ggobject(d) + 
  geom_line(data=preds_all, aes(x=ages, y=fit, size=0.52), color='black', lty='solid') +
  geom_ribbon(data=preds_all, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.3, fill='gray') +
  scale_y_continuous(labels=seq(22.55,23.85,0.1), breaks=seq(22.55,23.85,0.1), limits=c(22.55,23.85)) +
  geom_text_repel(
    aes(x=age_mean, y=AL_mean, label=label), 
    size=3,
    min.segment.length = 0, 
    seed = 42, 
    box.padding = 0.7,
    point.padding=1.5,
    max.overlaps = Inf,
    arrow = arrow(length = unit(0.01, "npc")),
    nudge_x = ifelse(d$age_mean>6.7 & d$age_mean<9.5, 0.05, 0.9),
    nudge_y = ifelse(d$age_mean>6.7 & d$age_mean<9.5, 0.175, -0.15),
    color = "grey50")
ggsave('/Users/fabianyii/Desktop/AlRateEmmetropes/plots/NlsFullDataset.pdf', width=170, height=165, units='mm')

# PLOT 3: Nonlinear EA and non-EA AL growth curves (linear ineraction shown as inset)
main_plot <- ggobject(d, ylimits=c(22.4, 23.9)) + 
  geom_line(data=preds_EA, aes(x=ages, y=fit, size=0.5), color='darkblue') +
  geom_ribbon(data=preds_EA, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.09, fill='darkblue') +
  geom_line(data=preds_nonEA, aes(x=ages, y=fit, size=0.5), color='maroon') +
  geom_ribbon(data=preds_nonEA, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.09, fill='maroon')
inset_plot <- ggobject(d, ylimits=c(22.5, 23.8)) + xlim(6,12) + 
  geom_line(data=data.frame(age_mean=ages_trunc), size=0.8, aes(x=age_mean, y=preds_EA_lin$pred), color='darkblue') +
  geom_ribbon(data=preds_EA_lin, aes(x=ages_trunc, ymin=ci.lb, ymax=ci.ub), alpha=0.15, fill='darkblue') +
  geom_line(data=data.frame(age_mean=ages_trunc), size=0.8, aes(x=age_mean, y=preds_nonEA_lin$pred), color='maroon') +
  geom_ribbon(data=preds_nonEA_lin, aes(x=ages_trunc, ymin=ci.lb, ymax=ci.ub), alpha=0.15, fill='maroon') 
main_plot + geom_vline(xintercept=12, lty='dashed', color='lightgray') + 
  annotation_custom(ggplotGrob(inset_plot + xlim(6,12) +
                                 theme_linedraw() +
                                 labs(subtitle=expression(AL == 22.3 + 0.09(age) + 0.09(EA) + 0.02(age)(EA) )) +
                                 theme(legend.position='none',
                                       panel.border = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       panel.grid.major = element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.title.y=element_blank(),
                                       plot.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
                                       plot.subtitle=element_text(size=9, hjust=0.5, face="bold", color="darkgray")) +
                                 scale_size(range = c(0.1, 9), guide='none') +
                                 scale_y_continuous(labels=seq(22.4,23.8, 0.2), breaks=seq(22.4,23.8, 0.2), lim=c(22.4, 23.8)) +
                                 scale_x_continuous(labels=seq(6,12,2), breaks=seq(6,12,2), lim=c(6,12)) ), 
                    xmin=13.5, xmax=24, ymin=22.4, ymax=23.2) +  theme(legend.position=c(0.2,0.8))
ggsave('/Users/fabianyii/Desktop/AlRateEmmetropes/plots/EaVsNonEa.pdf', width=160, height=150, units='mm')

###################################################################################
################################### DISCUSSION ####################################
###################################################################################
### Comparing annual rate of change in AL as predicted by our model vs Wong et al. vs Zadnik et al. 
sgal <- function(age){17.18 - 10.045/age } # Wong et al. growth model: vitreous chamber growth model used as surrogate for AL growth
usal <- function(age){ # Zadnik et al. growth model
  al <- rep(0,length(age))
  for (i in 1:length(age)){
    if(age[i]<=10.5){al[i]<-20.189+1.258*log(age[i])}
    else{al[i]<-21.353+0.759*log(age[i])} }
  return(al) }
  
ages1 <- seq(6,12,1)
ages2 <- seq(12.2,24,0.2)
ages <- c(ages1, ages2)
usal_rate <- usal(ages+1) - usal(ages) # compute growth rates from Zadnik et al. growth model
sgal_rate <- sgal(ages+1) - sgal(ages) # compute growth rates from Wong et al. growth model
OurAlRate <- rep(0, length(ages)) # growth rates from this work
for(i in 1:length(ages)){ OurAlRate[i] <- mean(m_all1_first_deriv(seq(ages[i],ages[i]+1,0.01))) }

data <- data.frame('age'=ages, 'OurAlRate'=OurAlRate, 'UsAlRate'=usal_rate, 'SgAlRate'=sgal_rate)
data$sg_extrapolate <- 'no'; data$sg_extrapolate[which(data$age>12)] <- 'yes'
data$us_extrapolate <- 'no'; data$us_extrapolate[which(data$age>14)] <- 'yes'
ggplot(data) + 
  geom_line(aes(x=age, y=OurAlRate, color='This work'), size=1) +
  geom_line(aes(x=age, y=SgAlRate, color='Wong et al.', linetype=sg_extrapolate), size=1) +
  geom_line(aes(x=age, y=UsAlRate,color='Zadnik et al.', linetype=us_extrapolate), size=1) +
  scale_color_manual(name=' ',
                     values=c('This work'='darkblue', 'Wong et al.'='maroon', 'Zadnik et al.'='darkgreen')) +
  theme(legend.position=c(0.6,0.8),
        legend.background=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank()) +
  guides(color=guide_legend(keywidth=1.8, keyheight=1.2)) +
  scale_x_continuous(labels=seq(6,24,2), breaks=seq(6,24,2), lim=c(6,24)) +
  scale_y_continuous(labels=seq(0,0.24,0.04), breaks=seq(0,0.24,0.04), lim=c(0,0.241)) +
  xlab("Age (year)") + 
  ylab("AL Growth Rate (mm/y)") +
  scale_linetype(guide='none')
ggsave('/Users/fabianyii/Desktop/AlRateEmmetropes/plots/ThisWorkVsOthers.pdf', width=85, height=100, units='mm')




