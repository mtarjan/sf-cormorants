##DCCO analysis
##June 2, 2017
##Prepared by Max Tarjan
##mtarjan@sfbbo.org

####SEE LINE 276 FOR CURRENT WORK WITH GAMs


##load required packages
require(ggplot2)
require(dplyr)
require(tidyr)
library(stringr)
library(gdata) ##required for read.xls


##load DCCO nest counts
#counts<-read.csv("DCCO_counts_18Aug2017.csv")
counts<-read.xls("C:/Users/max/Desktop/Tarjan/Science/DCCO_counts_25Jan2018.xlsx")

##RESTRICTIONS
##exclude seasonal total count type
counts<-subset(counts, Count.type != "Seasonal total" & Region != "" & Region != "NA")

##EXPAND DATA

##format date
counts$Survey.Date<-as.Date(counts$Survey.Date, "%m/%d/%Y")

library(lubridate)
##add day of year
counts$day<-yday(counts$Survey.Date)

##add time periods (pre v post bridge construction)
counts$time.period<-counts$Year
counts$time.period[which(counts$time.period<=2002)]<-"pre"
counts$time.period[which(counts$time.period>2002 & counts$time.period != "pre")]<-"post"


##VIEW DATA

##TABLE OF SAMPLE SIZE
ms.table1<-data.frame(count(x=counts, Region, Colony, lat, long));head(ms.table1)

#background basemap...need W, N, E, S bounds
#mapImage <- get_map(location = c(-121.9361, 37.6285, -122.1849, 37.4078),
#color = "color",
#                    source = "google",
#                    maptype = "satellite",
#                    zoom=11)

fig1<-ggplot(data = counts, aes(x = Year, y=Count, color=Colony)) + geom_point()
fig1 <- fig1 + geom_smooth(method="lm", se = F, aes(linetype = Region))
fig1 <- fig1 + ylab("Number of DCCO nests")
fig1

##colors for bar plots
mycols<-c("#E7298A", "#771155", "#114477", "#771122", "#DDDD77", "#1B9E77", "#66A61E","#7570B3", "#A6761D", "#4477AA", "#D95F02", "#E6AB02", "#666666")

##summarize data by region
regional.counts<-counts %>% group_by(Region, Year) %>% summarise(total=sum(Count)) %>% data.frame()

regional.counts$time.period<-regional.counts$Year
regional.counts$time.period[which(regional.counts$time.period<=2002)]<-"pre"
regional.counts$time.period[which(regional.counts$time.period>2002 & regional.counts$time.period != "pre")]<-"post"

##counts number of colonies tracked in each region
n.colonies<-data.frame(table(counts$Region, counts$Year))
n.colonies[,2]<-as.numeric(as.character(n.colonies[,2]))
colnames(n.colonies)<-c("Region", "Year", "n.sites")

regional.counts$n.sites<-rep(0, nrow(regional.counts))
for (j in 1:nrow(regional.counts)) {
  regional.counts$n.sites[j]<-as.numeric(subset(n.colonies, subset = Region == regional.counts$Region[j] & Year == regional.counts$Year[j], select = n.sites))
}

##plot of nest counts by year and site
fig2 <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=total, colour=as.factor(time.period), fill=as.factor(time.period)))
#fig2 <- fig2 + geom_bar(stat="identity")
fig2 <- fig2 + geom_point(size=2, shape=21)
fig2 <- fig2 + geom_smooth(method = "lm", aes(linetype = as.factor(time.period)), fill="darkgrey")
fig2 <- fig2 + scale_color_manual(values= c("black", "black"), name = "Pre- or Post- \nBridge Construction") + scale_fill_manual(values= c("black", "white"), name = "Pre- or Post- \nBridge Construction")
fig2 <- fig2 + scale_linetype_manual(values=c("solid", "twodash"), name = "Pre- or Post- \nBridge Construction")
fig2 <- fig2 + facet_wrap(~Region, strip.position="top") ##split up sites with facets; choose this option or the one below
#fig2 <- fig2 + geom_bar(stat="identity", aes(fill=Region)) + scale_fill_manual(values=mycols, name="") ##stacked barplot with sites as the colors. can change the colors to region when have those assigned (but need to summarize data by region first)
fig2 <- fig2 + ylab("Number of DCCO nests")
fig2 <- fig2 + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
fig2 <- fig2 + scale_y_continuous(breaks=seq(0, 2500, 200), expand=c(0,0), limits = c(0, NA))
fig2 <- fig2 + theme_bw()
fig2 <- fig2 + theme(panel.spacing = unit(0.25, "in"))
fig2 <- fig2 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig2 <- fig2 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig2

png(filename = "fig2.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig2; dev.off()


##FIGURE 3
##plot of raw nest counts by year and region (loess smoother)
fig3 <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=total))
#fig2 <- fig2 + geom_bar(stat="identity")
fig3 <- fig3 + geom_point(size=2)
fig3 <- fig3 + geom_smooth(method = "loess")
fig3 <- fig3 + facet_wrap(~Region, strip.position="top", scales="free") ##split up sites with facets; choose this option or the one below
#fig2 <- fig2 + geom_bar(stat="identity", aes(fill=Region)) + scale_fill_manual(values=mycols, name="") ##stacked barplot with sites as the colors. can change the colors to region when have those assigned (but need to summarize data by region first)
fig3 <- fig3 + ylab("Number of DCCO nests")
fig3 <- fig3 + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0), limits=c(1985,2016))
fig3 <- fig3 + scale_y_continuous(breaks=seq(0, 2500, 200), expand=c(0,0), limits = c(0, NA))
fig3 <- fig3 + theme_classic()
fig3 <- fig3 + theme()
#fig3 <- fig3 + theme(panel.border = element_rect(color="black", fill=NA))
fig3 <- fig3 + theme(panel.spacing = unit(0.25, "in"))
fig3 <- fig3 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3 <- fig3 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3 <- fig3 + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
fig3

png(filename = "fig3.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig3; dev.off()

##FIGURE 3B
##number of sites included in regional estimates each year (indicates survey effort??)
fig3b <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=n.sites))
fig3b <- fig3b + geom_point(size=2)
fig3b <- fig3b + geom_smooth(method = "loess")
fig3b <- fig3b + facet_wrap(~Region, strip.position="top") ##split up sites with facets; choose this option or the one below
fig3b <- fig3b + ylab("Number of DCCO colonies counted")
fig3b <- fig3b + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
fig3b <- fig3b + scale_y_continuous(breaks=seq(0, 100, 1), expand=c(0,0), limits = c(0, NA))
fig3b <- fig3b + theme_bw()
fig3b <- fig3b + theme(panel.spacing = unit(0.25, "in"))
fig3b <- fig3b + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3b <- fig3b + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3b

##FIGURE 3C
##number of birds counted per colony
fig3c <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=total/n.sites))
fig3c <- fig3c + geom_point(size=2)
fig3c <- fig3c + geom_smooth(method = "loess")
fig3c <- fig3c + facet_wrap(~Region, strip.position="top") ##split up sites with facets; choose this option or the one below
fig3c <- fig3c + ylab("Number of DCCO counted per colony")
fig3c <- fig3c + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
fig3c <- fig3c + scale_y_continuous(breaks=seq(0, 2000,100), expand=c(0,0), limits = c(0, NA))
fig3c <- fig3c + theme_bw()
fig3c <- fig3c + theme(panel.spacing = unit(0.25, "in"))
fig3c <- fig3c + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3c <- fig3c + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3c

## FIGURE 4
##plot change in dcco numbers for all sites minus bridge
annual.counts<-subset(counts, Region !="" & Region !="Bridges") %>% group_by(Year) %>% summarise(total=sum(Count)) %>% data.frame()
#annual.counts<-subset(counts, Region !="" ) %>% group_by(Year) %>% summarise(total=sum(Count)) %>% data.frame()

fig4 <- ggplot(data = annual.counts, aes(x = Year, y=total))
fig4 <- fig4 + geom_point(size=2)
fig4 <- fig4 + geom_smooth(method = "loess")
fig4 <- fig4 + ylab("Number of DCCO nests at non-bridge sites")
fig4 <- fig4 + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
fig4 <- fig4 + scale_y_continuous(breaks=seq(0, 5500, 200), expand=c(0,0), limits = c(0, NA))
fig4 <- fig4 + theme_bw()
fig4 <- fig4 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig4 <- fig4 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig4

png(filename = "fig4.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig4; dev.off()


##plot of change in dcco numbers (slope following bridge construction) with distance from bridge

##add mean distance to the two bridges
bridge.dist<-as.matrix(dist(subset(counts, select = c("lat", "long")), method = "euclidean"))
bb.dist<-bridge.dist[,which(counts$Colony=="S.F.-Oakland Bay Br. East")[1]]
rb.dist<-bridge.dist[,which(counts$Colony=="Richmond-San Rafael Bridge")[1]]
counts$bridge.dist<-apply(X = cbind(bb.dist, rb.dist), MARGIN = 1, FUN = mean)

##growth rate of colony post 2002
out<-data.frame(site = rep(NA, length(unique(counts$Colony))), Region = rep(NA, length(unique(counts$Colony))), bridge.dist = rep(NA, length(unique(counts$Colony))), slope = rep(NA, length(unique(counts$Colony))))
for (j in 1:length(unique(counts$Colony))) {
  row.temp<-subset(counts, subset= Colony == as.character(unique(counts$Colony)[j]))
  site<-as.character(unique(counts$Colony)[j])
  if (nrow(subset(counts, subset = Colony==site & Year >2002))>1) {
    slope<-lm(formula = Count ~ Year, data = subset(counts, subset = Colony==site & Year >2002))$coefficients[2]
    out$site[j]<-site
    out$Region[j]<-as.character(row.temp$Region[1])
    out$bridge.dist[j]<-row.temp$bridge.dist[1]
    out$slope[j]<-slope
  }
}

out<-out[-which(is.na(out$slope)),] ##exclude sites for which slope could not be calculated (too few sites)

ggplot(data=out, aes(x = Region, y = slope)) + geom_boxplot() + ylab("Change in DCCO counts 2002-2016")

##plot sites on a map
#map1<-ggplot(data = counts, aes(x = UTM.Easting, y = UTM.Northing)) + geom_point(aes(size=nests))
#map1 <- map1 + facet_wrap(facets = ~time.period)
#map1

#par(mfrow=c(1,length(unique(counts$time.period))))

##LOAD PHIL'S REGIONAL COUNTS
#data<-read.csv("DCCO_regional_counts_Phil_12Jul2017.csv")
#data.org<-data
#data<-tidyr::gather(data, "Year", "Count", 2:ncol(data)) ##rearrange data
#data$Year<-as.numeric(str_sub(data$Year, 2, 5)) ##format year (remove space)
#data<-subset(data, subset = Count!='NA')

#fig3 <- ggplot(data = subset(data, subset = Region !="All Colonies"), aes(x = Year, y=Count))
#fig3 <- fig3 + geom_point(size=2)
#fig3 <- fig3 + geom_smooth(method = "loess")
#fig3 <- fig3 + facet_wrap(~Region, strip.position="top", scales="free_y") ##split up sites with facets; choose this option or the one below
#fig2 <- fig2 + geom_bar(stat="identity", aes(fill=Region)) + scale_fill_manual(values=mycols, name="") ##stacked barplot with sites as the colors. can change the colors to region when have those assigned (but need to summarize data by region first)
#fig3 <- fig3 + ylab("Number of DCCO nests")
#fig3 <- fig3 + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
#fig3 <- fig3 + scale_y_continuous(breaks=seq(0, 3500, 200), expand=c(0,0), limits = c(0, NA))
#fig3 <- fig3 + theme_bw()
#fig3 <- fig3 + theme(panel.spacing = unit(0.25, "in"))
#fig3 <- fig3 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#fig3 <- fig3 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
#fig3

#png(filename = "fig3a.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig3; dev.off()

##PLOT EFFECT OF BRIDGE POP ON NON-BRIDGE POP
#data.test<-tidyr::spread(data, value = Count, key = Region)
##data.test<-subset(data.test, Year > 2000)

##get predicted population sizes based on loess curves
#out<-data.frame(Year=unique(data.test$Year))
#for (j in 1:length(unique(data$Region))) {
#  dat.temp<-subset(data, Region==unique(data$Region)[j])
#  pred.temp<-predict(loess(dat.temp$Count~dat.temp$Year), unique(data.test$Year))
#  pred.temp<-data.frame(pred.temp)
#  colnames(pred.temp)<-unique(data$Region)[j]
#  out<-cbind(out, pred.temp)
#}

#data.test<-out

##calculate annual growth rate
#gr<-dim(0)
#for (j in 2:nrow(data.test)) {
#  gr.temp<-cbind(data.test$Year[j],(data.test[j,2:ncol(data.test)]-data.test[j-1,2:ncol(data.test)])/(data.test[j,2:ncol(data.test)]+data.test[j-1,2:ncol(data.test)])*100)
#  gr<-rbind(gr, gr.temp)
#}



#plot(data.test$Bridge, data.test$NonBridge)
#plot(gr$Bridges, gr$"Outer Coast"); abline(a = 0, b = -1); text(x=gr$Bridges, y=gr$"Outer Coast", label = as.character(gr$`data.test$Year[j]`), pos=4)


#fig5 <- ggplot(data = gr, aes(x = Bridges, y=NonBridge))
#fig5 <- fig5 + geom_point(size=2)
#fig5 <- fig5 + geom_smooth(method = "loess")
#fig5 <- fig5 + facet_wrap(~Region, strip.position="top")
#fig5 <- fig5 + xlab(label = "DCCO annual growth rate at Bridges")
#fig5 <- fig5 + ylab(label = "DCCO annual growth rate at all other sites")
#fig5 <- fig5 + xlim(c(-10,7))
#fig5 <- fig5 + annotate(geom = 'text', x = gr$Bridges+0.25, y= gr$NonBridge, label = gr$`data.test$Year[j]`)
#fig5

#ggplot(data = subset(data, Region=="NonBridge"), aes(x = Year, y = Count))+ geom_point() + geom_line(data = data.test, aes(x = Year, y = NonBridge), color="blue") ##check that loess function is assigning functions well

##ANALYZE DATA

##this section goes through:
##1) fit a basic model
##2) see if assumptions are met (about error distrib, autocorrelation, etc)
##3) test more complex models
##4) select a model based on AIC (or other nonparametric method)
##5) interpret model

##generalized linear model
#model1<-glm(data = subset(counts, Region=="North Bay"), formula = Count ~ Year)
#summary(model1)

##LINEAR MODELS WITH TIME BREAKS

##GAM
library(mgcv)
library(lattice) ##for plotting

M0<-gam(Count ~ Year + Colony,
        data = counts,
        family = poisson)

M1<-gam(Count ~ s(Year) + Colony,
        data = counts,
        family = poisson)

M2<-gam(Count ~ s(Year) + Colony + day,
        data = counts,
        family = poisson)

M3<-gam(Count ~ s(Year) + Colony + Survey.type,
        data = counts,
        family = poisson)

M4<-gam(Count ~ s(Year) + Colony + day + Survey.type,
        data = counts,
        family = poisson)

M5<-gam(Count ~ s(Year) + Colony + day + time.period,
        data = counts,
        family = poisson)

##interactions; allow for separate trends for each site
M6<-gam(Count ~ s(Year, by=Colony) + Colony,
        data = counts,
        family = poisson)

#M7<-gam(Count ~ s(Year, by=Colony) + Colony + day,
#        data = counts,
#        family = poisson)

#M8<-gam(Count ~ s(Year, by=Colony) + Colony + day + time.period,
#        data = counts,
#        family = poisson)

#M9<-gam(Count ~ s(Year, by=Colony) + Colony + day + Survey.type,
#        data = counts,
#        family = poisson)

#M10<-gam(Count ~ s(Year, by=Colony) + Colony + day + Survey.type,
#        data = subset(counts, Region=="North Bay"),
#        family = poisson)

#AIC(M0, M1, M2, M3, M4, M5, M6, M7, M8, M9)
AIC(M0, M1, M2, M3)

##quasi models to deal with overdispersion
M9q<-gam(Count ~ s(Year, by=Colony) + Colony + day + Survey.type,
         data = counts,
         family = quasipoisson)

##plot model M9
#P9<-predict(M9, se.fit = T)
#plot(M9$model$Year, M9$fitted.values)

##diagnostic plots
E9<-resid(M9, type="pearson")
F9<-fitted(M9)
plot(x=F9, y=E9); abline(h=0)

##residuals for every colony
xyplot(E9~F9 | M9$model$Colony)

##histogram of residuals
hist(E9)

##plot residuals against covariates
plot(x=M9$model$day, y=E9)

xyplot(E9~M9$model$day | M9$model$Colony)

##compute autocorrelation
ac<-tapply(E9, FUN = acf, INDEX = M9$model$Colony, plot=F, lag.max=5)

for (j in 1:length(ac)) {
  if (j ==1) {
    ac.all<-ac[[j]]$acf
    K<-c(1:length(ac.all))
    ID<-rep(j, length(ac.all))
  } else {
    ac.all<-c(ac.all, ac[[j]]$acf)
    K<-c(K, 1:length(ac[[j]]$acf))
    ID<-c(ID, rep(j, length(ac[[j]]$acf)))
  }
}

#K<-rep(0:5, length(ac))
#ID<-rep(1:length(ac), each=6)
cbind(ac.all,K,ID)

##create lattice plot to look at time lag
xyplot(ac.all ~ K | factor(ID),
       ylim=c(-1,1),
       ylab= "acf",
       xlab = "Time lag",
       panel = function (x,y) {
         panel.lines(x,y,type="h", col=1)
         panel.abline(h = 0)
         panel.abline(h = 1.96 / sqrt(31), lty=2)
         panel.abline(h = -1.96/ sqrt(31), lty=2)
       })

#Examine overdispersion 
sum(E9^2) / (M9$df.res)

##GAMM (poptrend)
##poptrend
library(poptrend)


## Fit a smooth trend with fixed site effects, random time effects,
## and automatic selection of degrees of freedom

regions<-unique(counts$Region)

j<-2
trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re") ##site as a random effect
                #+ s(day)
                #+ s(bridge.dist)
                #+ Region
                + Survey.type
                , family = quasipoisson, data = subset(counts, Region==regions[j])) ##subset by region
#, family = quasipoisson, data = counts) ##all regions

## Check the model fit
checkFit(trFit)
## Plot the trend
plot(trFit, ciBase=mean, main=regions[j])
#summary(trFit)
## Check the estimated percent change from year 8 to 25
change(trFit, 2002, 2016)

##plot predicted pop change for each region
par(mfrow = c(2,2), bty="L", mar=c(4,4,4,2))
counts.temp<-subset(counts, Region!="" & Region!="South Farallon Islands")

trend.estimates<-dim(0)
regional.models<-list()
for (j in 1:length(unique(counts.temp$Region))) {
  reg.temp<-unique(counts.temp$Region)[order(unique(counts.temp$Region))][j]
  data.temp<-subset(counts.temp, Region==reg.temp)
  
  if (reg.temp=="Bridges") { ##if the region is bridges, don't include day. else include day as a fixed effect
    trFit <- ptrend(Count ~ trend(Year, tempRE = TRUE, type = "smooth") + s(Colony, bs="re"), family = quasipoisson, data = data.temp) ##site as a random effect
  } else {
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
##sum of raw counts in the north bay region
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
    anchor.temp<-data.frame(Colony=site.temp, Year=1990, Count=0, Survey.Date=NA, Organization=NA, Survey.type=NA, lat=data.temp$lat[1], long=data.temp$long[1], Region="North Bay", Count.type=NA, Exclude.comments=NA, Comments=NA, day=NA, time.period="pre", bridge.dist=data.temp$bridge.dist[1])
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
