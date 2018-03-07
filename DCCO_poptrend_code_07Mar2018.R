##need to run pop analysis code first to get inputs

##GAMM (poptrend)
##poptrend
library(poptrend)


## Fit a smooth trend with fixed site effects, random time effects,
## and automatic selection of degrees of freedom

regions<-unique(counts$Region)

j<-2
trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") 
                + s(Colony, bs="re") ##site as a random effect
                + s(day)
                #+ s(bridge.dist)
                #+ Region
                + Survey.type
                , family = quasipoisson, data = subset(counts, Region==regions[j] & Year >1990)) ##subset by region
#, family = quasipoisson, data = counts) ##all regions

## Check the model fit
checkFit(trFit)
## Plot the trend
plot(trFit, ciBase=mean, main=regions[j])
#summary(trFit)
## Check the estimated percent change from year 8 to 25
change(trFit, 2002, 2017)

##plot predicted pop change for each region
par(mfrow = c(3,2), bty="L", mar=c(4,4,4,2))
counts.temp<-subset(counts, Region!="")

trend.estimates<-dim(0)
regional.models<-list()
for (j in 1:length(unique(counts.temp$Region))) {
  reg.temp<-unique(counts.temp$Region)[order(unique(counts.temp$Region))][j]
  data.temp<-subset(counts.temp, Region==reg.temp)
  
  if (reg.temp=="Bridges") { ##if the region is bridges, don't include day. else include day as a fixed effect
    trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = data.temp) ##site as a random effect
  }
  if (reg.temp == "South Farallon Islands") {
    trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + Survey.type, family = quasipoisson, data = data.temp) ##no site effect
  }
  if (reg.temp != "Bridges" & reg.temp != "South Farallon Islands") {
    trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(day), family = quasipoisson, data = data.temp) ##site as a random effect
  }
  
  plot(trFit, main=reg.temp, ylab="Trend in nest counts", ciBase=mean, plotGrid = F)#; mtext(text = str_c( round(change(trFit, 2006, 2016)$percentChange[1],1),"% change from 2006-2016"), side = 3)
  
  #trend.estimates.temp<-cbind(region = rep(reg.temp, nrow(summary(trFit)$estimates)), summary(trFit)$estimates)
  #trend.estimates.temp<-trend.estimates.temp %>% mutate(Perc_Chg = (trend-lag(trend))/lag(trend))
  #trend.estimates<-rbind(trend.estimates, trend.estimates.temp)
  
  ##new version of trend estimates for every year in region
  years.temp<-min(data.temp$Year):max(data.temp$Year)
  for (i in 2:length(years.temp)) {
    year.temp<-years.temp[i]
    change.temp<-change(trFit, year.temp-1, year.temp)
    trend.estimates.temp<-data.frame(Region=reg.temp, Year=year.temp, Perc_Chg=round(change.temp$percentChange,2), CI_lower=round(change.temp$CI[[1]],2), CI_upper=round(change.temp$CI[[2]],2))
    trend.estimates<-rbind(trend.estimates, trend.estimates.temp)
  }
  
  
  regional.models[[j]]<-trFit
  names(regional.models)[[j]]<-as.character(reg.temp)
  
}
par(mfrow=c(1,1))

#trend.estimates.spread<-subset(trend.estimates, select=c(region, Year, Perc_Chg)) %>% spread(key = region, value = Perc_Chg)

##model for north bay without effect of day
trFit.northbay.noday <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = subset(counts, Region=="North Bay")) ##site as a random effect

##model growth at non-bridges
##based on data by sites
nonbridge.temp<-subset(counts, Region!="Bridge")
#nonbridge.temp<-counts
trFit.nonbridge <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(day), family = quasipoisson, data = nonbridge.temp)##site as a random effect

#trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + Colony, family = quasipoisson, data = nonbridge.temp)##site as a fixed effect

plot(trFit.nonbridge, main="All sites except bridges", ciBase=mean)#; mtext(text = str_c( round(change(trFit, 2006, 2016)$percentChange[1],1),"% change from 2006-2016"), side = 3)

##model growth at all regions together
trFit.all<-ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(day), family = quasipoisson, data = counts)##site as a random effect
plot(trFit.all, main="All sites except bridges", ciBase=mean)
checkFit(trFit.all)

##compare model fits
trFit.all2<-ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = counts)

##based on data by regions (Phil's data)
#nonbridge.temp<-subset(data, Region!="" & Region!="Bridges") ##phil's data subset
#nonbridge.temp<-subset(data, Region!="" ) ##phil's data subset
#trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + Region, data = nonbridge.temp)##site as a fixed effect
#plot(trFit, main="All sites except bridges (regional data)"); mtext(text = str_c( round(change(trFit, 2006, 2016)$percentChange[1],1),"% change from 2006-2016"), side = 3)

##based on regional data, but processed after having site counts
#nonbridge.temp<-subset(regional.counts, Region!="" & Region!="Bridges") ##site data processed
#trFit <- ptrend(total ~ trend(Year, tempRE = TRUE, type = "smooth") + Region, data = nonbridge.temp)##site as a fixed effect
#plot(trFit, main="All sites except bridges (regional data)"); mtext(text = str_c( round(change(trFit, 2006, 2016)$percentChange[1],1),"% change from 2006-2016"), side = 3)

##compare phil's processed data to site counts
#sub.temp<-subset(data, Region=="North Bay")
#all.temp<-subset(regional.counts, Region=="North Bay")

#plot(x = sub.temp$Year, y = sub.temp$Count) ##phil's totals in black
#points(x = all.temp$Year, y = all.temp$total, col="red") ##site totals in red

##generic count query
unique(subset(counts, Region=="Bridges", select = c(Colony, Year, Count)))

##explore north bay data
##sum of raw counts in x region
fig.test<-ggplot(data = subset(regional.counts, Region =="South Bay"), aes(x=Year, y=total)) + geom_point() +geom_smooth(method="lm")
fig.test <- fig.test + geom_text(aes(label=n.sites), nudge_x = 0.5)
fig.test

##model fit, taking site into account as random effect
regions<-unique(counts$Region)
j<-2
trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = subset(counts, Region==regions[j])) ##site as a random effect
plot(trFit, ciBase=mean, main=regions[j])

##trends in individual sites within north bay region
fig1n<-ggplot(data = subset(counts, Region=="North Bay"), aes(x = Year, y=Count, color=Colony)) + geom_point()
fig1n <- fig1n + geom_smooth(method="lm", se = F, aes(linetype = Region))
fig1n <- fig1n + ylab("Number of DCCO nests")
#fig1n <- fig1n + scale_y_continuous(limits=c(0,50))
fig1n

##make fake data with extra 0's to see effect on north bay model
nb.test<-subset(counts, Region=="North Bay")
##for each site
##find earliest date
##if it is before 1990, then add 0s from 1990 to first date

out<-nb.test
nb.sites<-unique(nb.test$Colony)
for (j in 1:length(nb.sites)) {
  site.temp<-nb.sites[j]
  data.temp<-subset(nb.test, Colony==site.temp)
  early.dat<-min(data.temp$Year)
  if (early.dat>1990) {
    anchor.temp<-data.frame(Colony=site.temp, Year=1990, Count=0, Survey.Date=NA, Organization=NA, Survey.type=NA, lat=data.temp$lat[1], long=data.temp$long[1], Region="North Bay", Count.type=NA, Exclude.comments=NA, Comments=NA, Incomplete.year=NA, day=NA, time.period="pre", bridge.dist=data.temp$bridge.dist[1])
    out<-rbind(out, anchor.temp)
  }
}

trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = out) ##site as a random effect
plot(trFit, ciBase=mean)

##BUFFER FROM BRIDGE
##close versus far sites
cut.off<-0.4
par(mfrow=c(2,1))
trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(day), family = quasipoisson, data = subset(counts, Region!="Bridges" & bridge.dist<cut.off)) ##site as a random effect
plot(trFit, ciBase=mean, main=str_c("Closer than ", cut.off, " dd to bridges"))

trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(day), family = quasipoisson, data = subset(counts, Region!="Bridges" & bridge.dist>=cut.off)) ##site as a random effect
plot(trFit, ciBase=mean, main=str_c("Farther than ", cut.off, " dd to bridges"))
par(mfrow=c(1,1))

##full model will effect of bridge dist
trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") + s(bridge.dist), family = quasipoisson, data = subset(counts, Region!="Bridges" & Year >2002)) ##site as a random effect
plot(trFit, ciBase=mean)
checkFit(trFit)
