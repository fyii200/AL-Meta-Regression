# Emmetropic eye growth from 6 to 24 years old: a meta-regression including 
# 4402 East Asian and non-East Asian emmetropes"

# Author: Fabian SL Yii
# Email: fabian.yii@ed.ac.uk
################################################################################
rm(list=ls())
dirName <- dirname(getwd())
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
library('gridExtra')
################################################################################
## Read data into workspace ##
d <- read.csv(paste0(dirName,'/data/cleaned_data.csv'))
d$quality <- as.factor(d$quality)
d$dataset <- as.factor(d$dataset) 
d$emm_code <- as.factor(d$emm_code) 
d$ser_mean <- as.numeric(d$ser_mean) 

## Create a new column indicating which studies correspond to East Asia (value=1) ##
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

## Test if definition of emmetropia differs between East Asians (EA) and non-EA ##
# emm_code is a 7-level factor: ±0.50D SER (level 1), 
# -0.25D to +0.50D SER (level 2), -0.25D to +0.75D SER (level 3), 
# -0.25D to +1.00D SER (level 4), -0.50D to +0.75D SER (level 5),
# -0.50D to +1.00D SER (level 6), -0.50D to +1.25D SER (level 7),
# ±0.50D spherical power (level 8).
codes <- data.frame(emm_code=rep(NA, 28),
                    EA=rep(NA, 28))
for(i in 1:28){
  codes[i,1] <- subset(d, study==i)$emm_code[1]
  codes[i,2] <- subset(d, study==i)$east_asia[1] }
codes$EA <- factor(codes$EA, labels=c(0,1))
# Code ±0.50D SER as 1 and the rest as 2
codes$emm_code <- ifelse(codes$emm_code==1, 1, 2)
table(codes)
# No statistical difference in the proportion of studies that adopted the ±0.50D SER 
# definition between EA and non-EA studies 
chisq.test(codes$emm_code, codes$EA, simulate.p.value=TRUE)

# Nonlinear model fitted to the full dataset. Note that weights need to be specified as ~1/weight when using "nlme" which is different from "nlmer" 
m_allmix1 <- nlme(AL_mean ~ a+(b/exp(c*age_mean)), data = d, fixed = a + b + c ~ 1, random = a ~ 1 | dataset, start = c(a=24, b=-2, c=0.1), weights= ~1/weight)
summary(m_allmix1)
intervals(m_allmix1, which='fixed') # 95% CI: parameter estimates

# EA and non-EA nonlinear models #
# refit the combined model with "east_asia" as the grouping variable 
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
    theme_ipsum(ticks=TRUE) +
    theme(legend.position=c(1, 0.1), 
          legend.justification = c(1, 0.1),
          legend.background=element_blank(), 
          legend.title=element_text(face="bold", size=12),
          legend.text=element_text(size=10, face='bold'),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.y=element_blank()) + 
    guides(fill=guide_legend(keywidth=2, keyheight=0.5, override.aes=list(size=5))) +
    scale_x_continuous(labels=6:24, breaks=6:24) + 
    scale_y_continuous(labels=seq(ylimits[1],ylimits[2],0.1), breaks=seq(ylimits[1],ylimits[2],0.1), limits=c(ylimits[1],ylimits[2])) }  

# PLOT 1: Nonlinear AL growth curve fitted to the full dataset
full_plot <-
  ggobject(d, ylimits=c(22.4, 23.9)) + 
  geom_line(data=preds_all, aes(x=ages, y=fit, size=0.52), color='black', lty='solid') +
  geom_ribbon(data=preds_all, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.3, fill='gray') +
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
    color = "grey50") +
  theme(plot.margin = unit(c(5.5, 0.5, 5.5, 0), "pt"))

# PLOT 2: Nonlinear EA and non-EA AL growth curves 
ethnic_specific_plot <- ggobject(d, ylimits=c(22.4, 23.9)) + 
  geom_line(data=preds_EA, aes(x=ages, y=fit, size=0.5), color='darkblue') +
  geom_ribbon(data=preds_EA, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.09, fill='darkblue') +
  geom_line(data=preds_nonEA, aes(x=ages, y=fit, size=0.5), color='maroon') +
  geom_ribbon(data=preds_nonEA, aes(x=ages, ymin=lwr, ymax=upr), alpha=0.09, fill='maroon') +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(c(5.5, 0, 5.5, 0.5), "pt"))

combined_plot <- grid.arrange(full_plot, ethnic_specific_plot, ncol=2)
ggsave(file="whatever.png", plot=combined_plot, width=8.5, height=5)





