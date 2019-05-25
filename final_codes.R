#1
library(pastecs) 
library(sandwich)
require(lmtest)
library(tidyverse)

Fatalities = read.csv("us-traffic-fatalities.csv")

northeast = c("ct","me", "ma","nh", "ri", "vt","nj","ny","pa")
midwest = c("il", "in","mi","oh","wi","ia","ks","mn","mo","ne","nd","sd") 
south=c("de","fl","ga","md","nc","sc","va","wv","al","ky","ms","tn","ar","la","ok","tx") 
west=c("az","co","id","mt","nv","nm","ut","wy","ca","or","wa")

# Switch region to factor so that it can be included into the descriptive statistics
Fatalities$region = factor(Fatalities$region)

Fatalities$region <- case_when(Fatalities$state %in% northeast ~ 'northeast',
                               Fatalities$state %in% midwest ~ 'midwest',
                               Fatalities$state %in% south ~ 'south',
                               Fatalities$state %in% west ~ 'west',
                               
)  

# define the fatality rate (got from internet, they divide it by 10000)
Fatalities$fatal_rate <- Fatalities$fatal / (Fatalities$pop * 10000)

attach(Fatalities) 
options(scipen=100)
options(digits=2)
descriptiveStatistics<-cbind(fatal_rate=Fatalities$fatal_rate,unemp=Fatalities$unemp,income=Fatalities$income,beertax=Fatalities$beertax,drinkage=Fatalities$drinkage,miles=Fatalities$miles,region=Fatalities$region)
stat.desc(descriptiveStatistics,basic=F)

Fatalities$miles2 = (Fatalities$miles)^2
lmfit4 = lm(frate ~ unemp + log(income) + beertax + drinkage + miles + region+ miles2, data = Fatalities)
lmfit5 = lm(frate ~ log(income) + miles + region+ miles2, data = Fatalities)
summary(lmfit4)
summary(lmfit5)

#5

confint(lmfit4, 'unemp', level=0.9)

confint(lmfit4, 'log(income)', level=0.9)

confint(lmfit4, 'beertax', level=0.9)

confint(lmfit4, 'drinkage', level=0.9)

confint(lmfit4, 'miles', level=0.9)

confint(lmfit4, 'region', level=0.9)

confint(lmfit4, 'miles2', level=0.9)

#6

#7
#https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmfit4)

# Durbin Watson test (Serial Correlation)
require(lmtest)
dwtest(lmfit4)

#Run NeweyWest
NeweyWest(lmfit4, prewhite = FALSE)

# Multicollinearity (VIF)
library(car)
vif(lmfit4)


#8

