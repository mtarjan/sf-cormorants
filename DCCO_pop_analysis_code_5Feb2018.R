##DCCO analysis
##June 2, 2017
##Prepared by Max Tarjan
##mtarjan@sfbbo.org

####SEE LINE 585 FOR CURRENT WORK WITH PLOTTING GAMs


##load required packages
require(ggplot2)
require(dplyr)
require(tidyr)
library(stringr)
library(gdata) ##required for read.xls
library(BBmisc) ##required for normalize function


##load DCCO nest counts
#counts<-read.csv("DCCO_counts_18Aug2017.csv")
counts<-read.xls("C:/Users/max/Desktop/Tarjan/Science/DCCO_counts_20Feb2018.xlsx")

##make edits to add incomplete year info
counts$Incomplete.year[which(counts$Region=="North Bay" & counts$Year %in% c(1991, 1992, 1995:2002))]<-"yes" ##north bay region missing knight island counts 1995-2002
counts$Incomplete.year[which(counts$Region=="Outer Coast" & counts$Year ==2000)]<-"yes" ##Outer Coast: For 2000, we have a 0 for Hog Island, but Lake Merced is ND. So I think the 0 in 2000 in this Chart should be removed
counts$Incomplete.year[which(counts$Region=="South Bay" & counts$Year %in% c(1992, 1997, 1998, 2000:2002, 2004, 2006:2013, 2016:2017))]<-"yes"##South Bay: Most importantly, San Mateo Bridge (~ 100 nests) is ND for several years, especially after 2005. We know it’s been active
counts$Incomplete.year[which(counts$Region=="South Farallon Islands" & counts$Year %in% c(1985, 1986, 1991, 1992, 2009, 2010, 2012))]<-"yes" ##eliminate years with ground data only


##RESTRICTIONS
##exclude seasonal total count type
#counts<-subset(counts, Count.type != "Seasonal total" & Region != "" & Region != "NA" & Exclude.comments=="")
counts<-subset(counts, Count.type != "Seasonal total" & Region != "" & Region != "NA")

##EXPAND DATA

##format date
counts$Survey.Date<-as.Date(counts$Survey.Date, "%Y-%m-%d")

library(lubridate)
##add day of year
counts$day<-yday(counts$Survey.Date)

##add time periods (pre v post bridge construction)
counts$time.period<-counts$Year
counts$time.period[which(counts$time.period<=2002)]<-"pre"
counts$time.period[which(counts$time.period>2002 & counts$time.period != "pre")]<-"post"

##get data for years with nearly complete data only, given Phil's designations
counts.complete<-subset(counts, is.na(Incomplete.year))

##VIEW DATA

##TABLE OF SAMPLE SIZE
ms.table1<-data.frame(count(x=unique(subset(counts, select=c(Region, Colony, lat, long, Year))), Region, Colony, lat, long));head(ms.table1)

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

##first summarize by colony and year to get average if there are multiple counts from different agencies for one year
##can also alter this code to subset by survey type
##decide if using counts or counts complete for the regional counts
mean.colony.counts<-counts %>% group_by(Colony, Year, Region) %>% summarise(mean.count=round(mean(Count),0)) %>% data.frame()

max.colony.counts<-counts %>% group_by(Colony, Year, Region) %>% summarise(max.count=round(max(Count),0)) %>% data.frame()

##add survey type to max colony count
max.colony.counts$Survey.type<-NA
for (j in 1:nrow(max.colony.counts)) {
  type.temp<-counts$Survey.type[which(counts$Colony==max.colony.counts$Colony[j] & counts$Year==max.colony.counts$Year[j] & counts$Count==max.colony.counts$max.count[j])]
  #print(j); print(type.temp)
  if (length(type.temp)>0){
    max.colony.counts$Survey.type[j]<-as.character(type.temp)
  }
}

##summarize data by region
#regional.counts<-counts %>% group_by(Region, Year) %>% summarise(total=sum(Count)) %>% data.frame()
regional.counts<-max.colony.counts %>% group_by(Region, Year) %>% summarise(total=sum(max.count)) %>% data.frame()

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
fig2 <- fig2 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
fig2 <- fig2 + scale_y_continuous(breaks=seq(0, 2500, 200), expand=c(0,0), limits = c(0, NA))
fig2 <- fig2 + theme_bw()
fig2 <- fig2 + theme(panel.spacing = unit(0.25, "in"))
fig2 <- fig2 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig2 <- fig2 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig2

#png(filename = "fig2.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig2; dev.off()


##FIGURE 3
##plot of raw nest counts by year and region (loess smoother)
fig3 <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=total))
#fig2 <- fig2 + geom_bar(stat="identity")
fig3 <- fig3 + geom_point(size=2)
fig3 <- fig3 + geom_smooth(method = "loess")
fig3 <- fig3 + facet_wrap(~Region, strip.position="top", scales="free", ncol = 2) ##split up sites with facets; choose this option or the one below
#fig2 <- fig2 + geom_bar(stat="identity", aes(fill=Region)) + scale_fill_manual(values=mycols, name="") ##stacked barplot with sites as the colors. can change the colors to region when have those assigned (but need to summarize data by region first)
fig3 <- fig3 + ylab("Number of DCCO nests")
fig3 <- fig3 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
fig3 <- fig3 + scale_y_continuous(breaks=seq(0, 2500, 100), expand=c(0,0), limits = c(0, NA))
fig3 <- fig3 + theme_classic()
fig3 <- fig3 + theme()
#fig3 <- fig3 + theme(panel.border = element_rect(color="black", fill=NA))
fig3 <- fig3 + theme(panel.spacing = unit(0.25, "in"))
fig3 <- fig3 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3 <- fig3 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3 <- fig3 + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
fig3

png(filename = "fig3.png", units="in", width=6.5, height=6.5,  res=200);fig3; dev.off()

##FIGURE 3A
##loess on log(count)
fig3a <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=log(total)))
fig3a <- fig3a + geom_point(size=2)
fig3a <- fig3a + geom_smooth(method = "loess")
fig3a <- fig3a + facet_wrap(~Region, strip.position="top", scales="free") 
fig3a <- fig3a + ylab("ln(Number of DCCO nests)")
fig3a <- fig3a + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
#fig3 <- fig3 + scale_y_continuous(breaks=seq(0, 2500, 100), expand=c(0,0), limits = c(0, NA))
fig3a <- fig3a + theme_classic()
fig3a <- fig3a + theme()
#fig3 <- fig3 + theme(panel.border = element_rect(color="black", fill=NA))
fig3a <- fig3a + theme(panel.spacing = unit(0.25, "in"))
fig3a <- fig3a + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3a <- fig3a + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3a <- fig3a + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
fig3a

##find breaks according to loess on log(counts)
out<-dim(0)
for (j in 1:length(unique(regional.counts$Region))) {
  dat.temp<-subset(regional.counts, Region==unique(regional.counts$Region)[j] & total >0)
  loess.temp<-loess(formula = log(total) ~ Year, data = dat.temp)
  predict.temp<-predict(loess.temp)
  #curve.pts<-cbind(dat.temp$Year, predict.temp)
  
  dy <- diff(predict.temp) 
  
  cutoff<-dim(0)
  for (i in 6:(length(dy)-5)) {
    if (t.test(x=dy[i:(i-4)], y=dy[(i+1):(i+5)])$p.value<0.1) {
      cutoff<-c(cutoff, dat.temp$Year[i])
    }
  }
  cutoff<-round(median(cutoff),0)
  #print(cutoff)
  out<-rbind(out, c(as.character(unique(regional.counts$Region)[j]), cutoff))
}
cutoff<-data.frame(out)
cutoff$X2<-as.numeric(as.character(cutoff$X2))

##FIGURE 3B
##number of sites included in regional estimates each year (indicates survey effort??)
fig3b <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=n.sites))
fig3b <- fig3b + geom_point(size=2)
fig3b <- fig3b + geom_smooth(method = "loess")
fig3b <- fig3b + facet_wrap(~Region, strip.position="top") ##split up sites with facets; choose this option or the one below
fig3b <- fig3b + ylab("Number of DCCO colonies counted")
fig3b <- fig3b + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
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
fig3c <- fig3c + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
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
fig4 <- fig4 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
fig4 <- fig4 + scale_y_continuous(breaks=seq(0, 5500, 200), expand=c(0,0), limits = c(0, NA))
fig4 <- fig4 + theme_bw()
fig4 <- fig4 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig4 <- fig4 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig4

#png(filename = "fig4.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig4; dev.off()


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

ggplot(data=out, aes(x = Region, y = slope)) + geom_boxplot() + ylab("Change in DCCO counts 2002-2017")

##plot sites on a map
#map1<-ggplot(data = counts, aes(x = UTM.Easting, y = UTM.Northing)) + geom_point(aes(size=nests))
#map1 <- map1 + facet_wrap(facets = ~time.period)
#map1

#par(mfrow=c(1,length(unique(counts$time.period))))

##LOAD PHIL'S REGIONAL COUNTS
phil.data<-read.csv("C:/Users/max/Desktop/Tarjan/Science/DCCO/DCCO_regional_counts_Phil_12Jul2017.csv")
phil.data<-tidyr::gather(phil.data, "Year", "Count", 2:ncol(phil.data)) ##rearrange data
phil.data$Year<-as.numeric(str_sub(phil.data$Year, 2, 5)) ##format year (remove space)
phil.data<-subset(phil.data, subset = Count!='NA')
colnames(phil.data)<-c("Region", "Year", "total")

#data<-read.csv("DCCO_regional_counts_Phil_12Jul2017.csv")
#data.org<-data
#data<-tidyr::gather(data, "Year", "Count", 2:ncol(data)) ##rearrange data
#data$Year<-as.numeric(str_sub(data$Year, 2, 5)) ##format year (remove space)
#data<-subset(data, subset = Count!='NA')

fig3d <- ggplot(data = subset(phil.data, subset = Region !="All Colonies"), aes(x = Year, y=total))
fig3d <- fig3d + geom_point(size=2)
fig3d <- fig3d + geom_smooth(method = "loess")
fig3d <- fig3d + facet_wrap(~Region, strip.position="top", scales="free_y", ncol = 2) ##split up sites with facets; choose this option or the one below
fig3d <- fig3d + ylab("Number of DCCO nests")
fig3d <- fig3d + scale_x_continuous(breaks=seq(1980, 2016, 2), expand=c(0,0))
fig3d <- fig3d + scale_y_continuous(breaks=seq(0, 3500, 200), expand=c(0,0), limits = c(0, NA))
fig3d <- fig3d + theme_bw()
fig3d <- fig3d + theme(panel.spacing = unit(0.25, "in"))
fig3d <- fig3d + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig3d <- fig3d + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig3d

#png(filename = "fig3.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig3; dev.off()

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

##POISSON REGRESSION: Gerry's suggested analysis; Manuwal 2001. Biology and Conservation of the Common Murre. USGS
##1) poisson regression of count sums (ie regional counts)
##2) average poisson regression; averaged from multiple regressions on counts of individual colony counts. The average regression can have weights for each colony regression (weighted by population size and survey effort)
##3) compare these two to see if they tell the same story

##table with slope of ln(N)- estimate, SE, lower 95% CI, upper, p-value, % annual increase (estimate, upper and lower CI). for regressions across different time periods by region/site

##FIGURE 6
##linear regression of log count sums (ie regional counts)

##use my regional counts
#regional.counts<-regional.counts.org

out<-dim(0) ##output for table
for (j in 1:length(unique(regional.counts$Region))) {
  region.temp<-unique(regional.counts$Region)[j]
  dat.temp<-subset(regional.counts, subset = Region ==region.temp) ##take regional data only
  
  ##get equations for two separate pieces
  cutoff.temp<-cutoff$X2[which(as.character(cutoff$X1)==as.character(region.temp))]
  #lm1<-lm(formula = log(total)~Year, data=subset(dat.temp, Year < cutoff.temp & total>0))
  #lm2<-lm(formula = log(total)~Year, data=subset(dat.temp, Year >= cutoff.temp & total >0))
  
  lm1<-lm(formula = log(total+0.01)~Year, data=subset(dat.temp, Year < cutoff.temp))
  lm2<-lm(formula = log(total+0.01)~Year, data=subset(dat.temp, Year >= cutoff.temp))
  
  ##make linear model for all data
  lm.all<-lm(formula = log(total+0.01)~Year, data=dat.temp)
  
  ##make an equation for logistic growth
  
  if (region.temp=="North Bay") {
    model.log<-nls(total~K/(1+((K-No)/No)*exp(-r*(Year-1986))),
                   start=list(K=300,No=20,r=0.5),data=dat.temp,trace=F)
  } 
  if (region.temp!="North Bay" & region.temp!="South Bay") {
    model.log<-nls(total~K/(1+((K-No)/No)*exp(-r*(Year-1986))),
                   start=list(K=800,No=1,r=0.5),data=dat.temp,trace=F)
  }
  
  if (region.temp=="South Bay") {
    model.log<-nls(total~K/(1+((K-No)/No)*exp(-r*(Year-1986))),
                   start=list(K=800,No=1,r=0.5),data=dat.temp,trace=F)
  }
  
  
  ##get functions from equations
  fun1<-function(x) exp(coefficients(lm1)[1])*exp(coefficients(lm1)[2]*x)
  fun2<-function(x) exp(coefficients(lm2)[1])*exp(coefficients(lm2)[2]*x)
  fun.all<-function(x) exp(coefficients(lm.all)[1])*exp(coefficients(lm.all)[2]*x)
  
  fun1.lm<-function(x) coefficients(lm1)[2]*x+coefficients(lm1)[1]
  fun2.lm<-function(x) coefficients(lm2)[2]*x+coefficients(lm2)[1]
  
  fun.lm.all<-function(x) coefficients(lm.all)[2]*x+coefficients(lm.all)[1]
  
  No<-coefficients(model.log)[2]
  K<-coefficients(model.log)[1]
  r<-coefficients(model.log)[3]
  
  #No<-1
  #K<-750
  #r<-0.4
  
  fun.log<- function(x) K/(1+((K-No)/No)*exp(-r*(x-1986)))
  
  #fig6 <- ggplot(data = dat.temp, aes(x = Year, y=total))
  #fig6 <- fig6 + geom_point(size=2)
  #fig6 <- fig6 + stat_function(fun=fun.log, size=1.25)
  #fig6
  
  ##calculate r-squared
  RSS.p <- sum(residuals(model.log)^2) ##residual sum of squares
  TSS <- sum((dat.temp$total - mean(dat.temp$total))^2)  # Total sum of squares
  log.rsq<-round(1 - (RSS.p/TSS),2)  # R-squared measure
  
  ##make plot of counts and both equations; list r and p values on plot
  fig6 <- ggplot(data = dat.temp, aes(x = Year, y=total))
  fig6 <- fig6 + geom_point(size=2)
  fig6 <- fig6 + facet_wrap(~Region, strip.position="top", scales="free") ##split up sites with facets
  fig6 <- fig6 + ylab("Number of DCCO nests")
  fig6 <- fig6 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
  fig6 <- fig6 + scale_y_continuous(breaks=seq(0, 2500, 100), expand=c(0,0), limits = c(0, NA))
  
  if (region.temp %in% c("Outer Coast", "South Bay", "North Bay")) {
    fig6 <- fig6 + stat_function(fun=fun.log, size=1.25)
    fig6 <- fig6 + geom_text(aes(x=2008, y=max(dat.temp$total)/10, label=str_c("r-squared = ",log.rsq,  "; p = ", round(coefficients(summary(model.log))[3,4], 3))))
    
  } else {
    fig6 <- fig6 + stat_function(fun=fun1, xlim=c(1985, cutoff.temp-1), size=1.25)
    fig6 <- fig6 + stat_function(fun=fun2, xlim=c(cutoff.temp, 2017), size=1.25)
    
    fig6 <- fig6 + geom_text(aes(x=2008, y=max(dat.temp$total)/7, label="time period     r-squared     p-value"))
    fig6 <- fig6 + geom_text(aes(x=2008, y=max(dat.temp$total)/10, label=str_c("1985-", cutoff.temp-1, "          ", round(summary(lm1)$r.squared,2),  "          ", round(coefficients(summary(lm1))[2,4], 3))))
    fig6 <- fig6 + geom_text(aes(x=2008, y=(max(dat.temp$total)/17), label=str_c(cutoff.temp, "-2017          ", round(summary(lm2)$r.squared,2),  "          ", round(coefficients(summary(lm2))[2,4], 3))))
  }
  
  fig6 <- fig6 + theme_classic()
  fig6 <- fig6 + theme(panel.spacing = unit(0.25, "in"))
  fig6 <- fig6 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  fig6 <- fig6 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
  fig6 <- fig6 + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
  fig6 <- fig6 + theme(text = element_text(size=16))
  fig6
  
  ##PLOT 6a. on a ln scale with linear functions
  
  fig6a <- ggplot(data = dat.temp, aes(x = Year, y=log(total)))
  fig6a <- fig6a + geom_point(size=2)
  fig6a <- fig6a + facet_wrap(~Region, strip.position="top", scales="free") ##split up sites with facets
  fig6a <- fig6a + ylab("ln(Number of DCCO nests)")
  fig6a <- fig6a + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
  #fig6a <- fig6a + scale_y_continuous(breaks=seq(0, 2500, 100), expand=c(0,0), limits = c(0, NA))
  
  fig6a <- fig6a + stat_function(fun=fun1.lm, xlim=c(1985, cutoff.temp-1), size=1.25)
  fig6a <- fig6a + stat_function(fun=fun2.lm, xlim=c(cutoff.temp, 2017), size=1.25)
  
  fig6a <- fig6a + geom_text(aes(x=2008, y=min(log(dat.temp$total+0.01))+(max(log(dat.temp$total+0.01))-min(log(dat.temp$total+0.01)))/7, label="time period     r-squared     p-value"))
  fig6a <- fig6a + geom_text(aes(x=2008, y=min(log(dat.temp$total+0.01))+(max(log(dat.temp$total+0.01))-min(log(dat.temp$total+0.01)))/10, label=str_c("1985-", cutoff.temp-1, "          ", round(summary(lm1)$r.squared,2),  "          ", round(coefficients(summary(lm1))[2,4], 3))))
  fig6a <- fig6a + geom_text(aes(x=2008, y=min(log(dat.temp$total+0.01))+(max(log(dat.temp$total+0.01))-min(log(dat.temp$total+0.01)))/17, label=str_c(cutoff.temp, "-2017          ", round(summary(lm2)$r.squared,2),  "          ", round(coefficients(summary(lm2))[2,4], 3))))
  
  fig6a <- fig6a + theme_classic()
  fig6a <- fig6a + theme(panel.spacing = unit(0.25, "in"))
  fig6a <- fig6a + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  fig6a <- fig6a + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
  fig6a <- fig6a + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
  fig6a <- fig6a + theme(text = element_text(size=16))
  fig6a
  
  ##PLOT 6b. linear function for all data. regular scale
  
  fig6b <- ggplot(data = dat.temp, aes(x = Year, y=total))
  fig6b <- fig6b + geom_point(size=2)
  fig6b <- fig6b + facet_wrap(~Region, strip.position="top", scales="free") ##split up sites with facets
  fig6b <- fig6b + ylab("Number of DCCO nests")
  fig6b <- fig6b + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
  
  fig6b <- fig6b + stat_function(fun=fun.all, xlim=c(1985, 2017), size=1.25)
  
  fig6b <- fig6b + geom_text(aes(x=2008, y=min(dat.temp$total)+(max(dat.temp$total)-min(dat.temp$total))/7, label="time period     r-squared     p-value"))
  fig6b <- fig6b + geom_text(aes(x=2008, y=min(dat.temp$total)+(max(dat.temp$total)-min(dat.temp$total))/10, label=str_c("1985-2017          ", round(summary(lm.all)$r.squared,2),  "          ", round(coefficients(summary(lm.all))[2,4], 3))))
  
  fig6b <- fig6b + theme_classic()
  fig6b <- fig6b + theme(panel.spacing = unit(0.25, "in"))
  fig6b <- fig6b + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  fig6b <- fig6b + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
  fig6b <- fig6b + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
  fig6b <- fig6b + theme(text = element_text(size=16))
  fig6b
  
  ##save the plot
  #assign(str_c("fig6.", as.character(region.temp)), fig6) ##save plot for that region ##this approach doesn't work because the variables (eg lm1) get updated so the plot call with plot the most recent one, not the variable that existed when it was saved
  png(filename = str_c("fig6.",region.temp, ".cutoff.png"), units="in", width=6*1.5, height=4*1.5,  res=200);print(fig6); dev.off()
  
  png(filename = str_c("fig6.",region.temp, ".ln.png"), units="in", width=6*1.5, height=4*1.5,  res=200);print(fig6a); dev.off()
  
  png(filename = str_c("fig6.",region.temp, ".png"), units="in", width=6*1.5, height=4*1.5,  res=200);print(fig6b); dev.off()
  
  out.temp<-c(as.character(region.temp), "1985-2017", round(coefficients(lm.all)[2],2), round(coefficients(summary(lm.all))[2,2], 3), round(coefficients(summary(lm.all))[2,4], 3), round(exp(coefficients(lm.all)[2])*100-100,2))
  out.temp1<-c(as.character(region.temp), str_c("1985-", cutoff.temp), round(coefficients(lm1)[2],2), round(coefficients(summary(lm1))[2,2], 3), round(coefficients(summary(lm1))[2,4], 3), round(exp(coefficients(lm1)[2])*100-100,2))
  ##percent annual increase is exp(slope)
  out.temp2<-c(as.character(region.temp), str_c(cutoff.temp, "-2017"), round(coefficients(lm2)[2],2), round(coefficients(summary(lm2))[2,2], 3), round(coefficients(summary(lm2))[2,4], 3), round(exp(coefficients(lm2)[2])*100-100,2))
  
  if (region.temp %in% c("Bridges", "South Farallon Islands")) {
    out<-rbind(out, out.temp1, out.temp2)
  } else {
    out<-rbind(out, out.temp)
  }
}

#out<-data.frame(out); colnames(out)<-c("Region", "start", "end", "slope", "se", "lower95CI", "upper95CI", "pvalue", "percent.annual.increase", "percent.upper95CI", "percent.lower95CI")

slope.table<-data.frame(out); colnames(slope.table)<-c("Region", "Time period", "slope", "se", "pvalue", "percent.annual.increase")

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
##code from Shadish et al. 2014. Using generalized additive (mixed) models to analyze single case designs. (in Mendeley)

library(mgcv)
library(lattice) ##for plotting

#M0<-gam(Count ~ Year + Colony,
#        data = counts,
#        family = poisson)

#M1<-gam(Count ~ s(Year) + Colony,
#        data = counts,
#        family = poisson)

#M2<-gam(Count ~ s(Year) + Colony + day,
#        data = counts,
#        family = poisson)

#M3<-gam(Count ~ s(Year) + Colony + Survey.type,
#        data = counts,
#        family = poisson)

#M4<-gam(Count ~ s(Year) + Colony + day + Survey.type,
#        data = counts,
#        family = poisson)

#M5<-gam(Count ~ s(Year) + Colony + day + time.period,
#        data = counts,
#        family = poisson)

##interactions; allow for separate trends for each site
#M6<-gam(Count ~ s(Year, by=Colony) + Colony,
#        data = counts,
#        family = poisson)

#M7<-gam(Count ~ s(Year, by=Colony) + Colony + day,
#        data = counts,
#        family = poisson)

#M8<-gam(Count ~ s(Year, by=Colony) + Colony + Survey.type,
#        data = counts,
#        family = poisson)

M9<-gam(Count ~ s(Year, by=Colony) + day + Survey.type,
        data = counts,
        family = poisson)

#M9<-gam(Count ~ s(Year, by=Colony) + Colony + day + time.period,
#        data = counts,
#        family = poisson)

#M10<-gam(Count ~ s(Year, by=Colony) + Colony + day + Survey.type,
#        data = counts,
#        family = poisson)

#aic.results<-AIC(M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12)
#round(aic.results, 0)
#AIC(M0, M1, M2, M3, M4, M5, M6, M7, M9)
##look for lowest AIC

##quasi models to deal with overdispersion
#M9q<-gam(Count ~ s(Year, by=Colony) + day + Survey.type,
#         data = counts,
#         family = quasipoisson)

##SELECT MODEL TO PLOT
model.plot<-M9

##plot model M8
#P8<-predict(M8, se.fit = T)
#plot(M8$model$Year, M8$fitted.values)

##diagnostic plots
#E8<-resid(M8, type="pearson")
#F8<-fitted(M8)
#plot(x=F8, y=E8); abline(h=0)

##residuals for every colony
#xyplot(E8~F8 | M8$model$Colony)

##histogram of residuals
#hist(E8)

##plot residuals against covariates
#plot(x=M8$model$Survey.type, y=E8)

#xyplot(E8~M8$model$Survey.type | M8$model$Colony)

##compute autocorrelation
#ac<-tapply(E8, FUN = acf, INDEX = M8$model$Colony, plot=F, lag.max=5)

#for (j in 1:length(ac)) {
#  if (j ==1) {
#    ac.all<-ac[[j]]$acf
#    K<-c(1:length(ac.all))
#    ID<-rep(j, length(ac.all))
#  } else {
#    ac.all<-c(ac.all, ac[[j]]$acf)
#    K<-c(K, 1:length(ac[[j]]$acf))
#    ID<-c(ID, rep(j, length(ac[[j]]$acf)))
#  }
#}

##K<-rep(0:5, length(ac))
##ID<-rep(1:length(ac), each=6)
#cbind(ac.all,K,ID)

##create lattice plot to look at time lag
#xyplot(ac.all ~ K | factor(ID),
#       ylim=c(-1,1),
#       ylab= "acf",
#       xlab = "Time lag",
#       panel = function (x,y) {
#         panel.lines(x,y,type="h", col=1)
#         panel.abline(h = 0)
#         panel.abline(h = 1.96 / sqrt(31), lty=2)
#         panel.abline(h = -1.96/ sqrt(31), lty=2)
#       })

#Examine overdispersion 
#sum(E8^2) / (M8$df.res)

##make plots of trends generated by GAM
#colony.temp<-"South Farallon Islands"
#survey.temp<-"Ground"

#region.temp<-unique(regional.counts$Region)[j]
#plot(subset(regional.counts, Region==region.temp, select=c(Year, total)), pch=20, main=region.temp)

#lines(M13$model$Year[which(regional.counts$Region==region.temp)], M13$fitted.values[which(regional.counts$Region==region.temp)])

#gam.pred <- predict(gam.mod, newdata = new.dat, type = "response", se.fit = TRUE)
# so we have to use pam.pred$fit for the predicted values
#plot(new.dat, gam.pred$fit, type = "l")
# and +/- the se.fit for the standard errors
#lines(new.dat, gam.pred$fit + gam.pred$se.fit, lty = "dotted")
#lines(new.dat, gam.pred$fit, type = "l", lty = "dotted")

##GENERATE CONFIDENCE INTERVALS FOR GAM MODEL USING SIMULATION
##https://stat.ethz.ch/pipermail/r-help/2011-April/275632.html

##plot sum of site counts for a given region
new.dat<-table(counts$Year, as.character(counts$Colony)) %>% data.frame()
colnames(new.dat)<-c("Year", "Colony", "Freq")
new.dat<-dplyr::left_join(new.dat[,1:2], y=subset(counts, select=c(Region,Colony)), by = c("Colony","Colony"))
new.dat<-unique(new.dat)
new.dat<-subset(new.dat, Region!="")
new.dat$Survey.type<-as.factor(rep("Ground", nrow(new.dat)))
new.dat$day<-round(mean(counts$day, na.rm=T),0)
new.dat$Year<-as.numeric(as.character(new.dat$Year))
new.dat$Colony<-as.factor(new.dat$Colony)
##use Model.plot
##PREDICT ACROSS FULL RANGE OF YEARS FOR EACH SITE; THEN CALCUATE REGIONAL PREDICTED COUNTS
pred<-predict(model.plot, newdata = new.dat, se.fit=T)$fit %>% as.numeric()
pred.se<-predict(model.plot, newdata = new.dat, se.fit=T)$se.fit %>% as.numeric()
counts.m8<-cbind(new.dat, pred, pred.se)

##add field counts to predicted counts.m8
#counts.temp<-counts; counts.temp$colony.year<-str_c(as.character(counts$Colony), counts$Year)
#counts.m8.temp<-counts.m8; counts.m8.temp$colony.year<-str_c(as.character(counts.m8$Colony), counts.m8$Year)
#counts.m8<-dplyr::left_join(counts.m8.temp, y=subset(counts.temp, select=c(colony.year, Count)), by = c("colony.year","colony.year")) ##THIS INTRODUCES DUPLICATE DATE/COLONY ROWS

##use mean or max colony counts instead
counts.temp<-max.colony.counts; counts.temp$colony.year<-str_c(as.character(counts.temp$Colony), counts.temp$Year)
counts.m8.temp<-counts.m8; counts.m8.temp$colony.year<-str_c(as.character(counts.m8$Colony), counts.m8$Year)
counts.m8<-dplyr::left_join(counts.m8.temp, y=subset(counts.temp, select=c(colony.year, Survey.type, max.count)), by = c("colony.year","colony.year"))
counts.m8<-subset(counts.m8, select=-colony.year)
colnames(counts.m8)[ncol(counts.m8)]<-"Count"
head(counts.m8)

#plot(pred~Count, data=counts.m8)

##get a weight for the predictions based on the average size of the colony / the size of the regional counts
out<-dim(0)
for (j in 1:nrow(counts.m8)){
  colony.temp<-counts.m8$Colony[j]
  region.temp<-counts.m8$Region[j]
  regional.counts.temp<-subset(counts.m8, Region==region.temp) %>% group_by(Region, Year) %>% summarise(total=sum(Count, na.rm=T)) %>% data.frame
  out.temp<-mean(subset(counts.m8, Colony==colony.temp)$Count, na.rm=T)/mean(regional.counts.temp$total, na.rm=T) ##weight is the mean colony count / mean count of the region
  out<-c(out, out.temp)
}
counts.m8$weight<-out ##mean colony size as a fraction of mean regional size

##divide the weight by the standard error (less weight given to estimates with more se)
counts.m8$weight2<-ifelse(counts.m8$pred.se==0, counts.m8$weight, counts.m8$weight/counts.m8$pred.se)

##subset counts.m8 to years when there is known info about each colony
counts.m8.sub<-dim(0)
for (j in 1:length(unique(counts.m8$Colony))) {
  colony.temp<-unique(counts.m8$Colony)[j]
  min.year<-min(subset(counts.m8, Colony==colony.temp & is.na(Count)==F)$Year, na.rm=T)
  max.year<-max(subset(counts.m8, Colony==colony.temp & is.na(Count)==F)$Year, na.rm=T)
  
  counts.m8.sub<-rbind(counts.m8.sub, subset(counts.m8, Colony==colony.temp & Year >= min.year & Year <= max.year))
}

head(counts.m8.sub)

##add normalized predictor by colony
counts.m8$pred.norm<-rep(NA, nrow(counts.m8))
for (j in 1:length(unique(counts.m8$Colony))) {
  colony.temp<-unique(counts.m8$Colony)[j]
  dat.temp<-subset(counts.m8, Colony==colony.temp)
  range.min<-min(dat.temp$Count, na.rm=T)
  range.max<-max(dat.temp$Count, na.rm=T)
  norm.temp<-normalize(dat.temp$pred, range=c(range.min, range.max), method="range")
  counts.m8$pred.norm[which(counts.m8$Colony==colony.temp)]<-norm.temp
}

##WEIGHT THE predictions BY POPULATION SIZE (and variance)
##alternative is to use predictor that is already scaled to pop size
regional.pred<-counts.m8 %>% group_by(Year, Region) %>% summarise(total=sum(Count, na.rm=T), pred.regional=sum(pred*weight2)) %>% data.frame()

##replace missing years of counts with NA
for (j in 1:nrow(regional.pred)) {
  dat.temp<-subset(regional.counts, Year==regional.pred$Year[j] & Region==regional.pred$Region[j])
  if (nrow(dat.temp)==0) {
    regional.pred$total[j]<-NA
  }
}

##plot trends for each colony
for (j in 1:length(unique(regional.pred$Region))) {
  region.temp<-unique(regional.pred$Region)[j]
  
  ##plot trends by colony
  data.plot<-subset(counts.m8, Region==region.temp)
  fig <- ggplot(data = data.plot, aes(x=Year))
  fig <- fig + geom_point(aes(y=Count, color=Survey.type.y)) #+ geom_path(aes(y=pred.norm))
  fig <- fig + scale_colour_discrete(name="Survey type")
  fig <- fig + ggtitle(region.temp)
  fig <- fig + facet_wrap(~Colony, scales = "free_y")
  fig
  
  if (length(unique(data.plot$Colony))==2) {
    png(filename = str_c("fig.",region.temp, ".counts.colonies.png"), units="in", width=6.5, height=3.5,  res=200);print(fig); dev.off()
  } else {
    png(filename = str_c("fig.",region.temp, ".counts.colonies.png"), units="in", width=6.5, height=5.5,  res=200);print(fig); dev.off()
  }
  
  ##plot the predictions with SE
  data.plot<-subset(counts.m8.sub, Region==region.temp)
  fig <- ggplot(data = data.plot, aes(x=Year))
  fig <- fig + geom_path(aes(y=pred)) + geom_path(aes(y=pred+pred.se), lty="dashed") + geom_path(aes(y=pred-pred.se), lty="dashed")
  fig <- fig + ylab("Trend")
  fig <- fig + ggtitle(region.temp)
  fig <- fig + facet_wrap(~Colony, scales = "free_y")
  fig
  
  png(filename = str_c("fig.",region.temp, ".gam.colonies.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()
  
  ##plot trends by region
  data.plot<-subset(regional.pred, Region==region.temp)
  range<-c(min(data.plot$total, na.rm=T), max(data.plot$total, na.rm=T))
  range.pred<-round(c(min(data.plot$pred.regional, na.rm=T), max(data.plot$pred.regional, na.rm=T)),0)
  fig <- ggplot(data = data.plot, aes(x=Year))
  fig <- fig + geom_point(aes(y=total))
  fig <- fig + geom_path(aes(y=normalize(pred.regional, range=range, method="range")))
  fig <- fig + ylab("Total regional count")
  fig <- fig + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Regional trend", breaks = seq(range[1], range[2], (range[2]-range[1])/10), labels = seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10)))
  fig <- fig + ggtitle(region.temp)
  fig
  
  png(filename = str_c("fig.",region.temp, ".gam.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()
}

##PLOT EFFECTS OF COVARIATES
##plot effect of day
##simulate new data
day<-seq(min(counts$day, na.rm=T), max(counts$day, na.rm=T), 1)
day.sim<-data.frame(Year= rep(median(counts$Year), length(day)), Colony=rep("Bair Island Power Towers", length(day)), Region=rep("South Bay", length(day)), Survey.type=rep("Ground", length(day)), day=day)
##get predications for new data
pred<-predict(model.plot, newdata = day.sim, se.fit=T)$fit %>% as.numeric()
pred.se<-predict(model.plot, newdata = day.sim, se.fit=T)$se.fit %>% as.numeric()
counts.day<-cbind(day.sim, pred, pred.se)
##plot effect of day on prediction
fig <- ggplot(data = counts.day, aes(x=day))
#fig <- fig + geom_point(aes(y=pred))
fig <- fig + geom_path(aes(y=pred))
fig <- fig + geom_path(aes(y=pred+pred.se), lty="dashed")
fig <- fig + geom_path(aes(y=pred-pred.se), lty="dashed")
fig <- fig + ylab("Predicted trend value")
fig <- fig + xlab("Count date (i.e. day of year)")
fig

png(filename = str_c("fig.day.effect.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()

##effect of count date on counts
#plot(Count~day, data=counts); abline(a= coefficients(lm(Count~day, data=counts))[1], b= coefficients(lm(Count~day, data=counts))[2])

##effect of count date on counts by colony
#fig <- ggplot(data = subset(counts, is.na(day)==F), aes(x=day, y= Count))
#fig <- fig + geom_point(aes())
#fig <- fig + geom_smooth(method="lm")
#fig <- fig + facet_wrap(~Colony, scales="free")
#fig

##effect of survey type on predicted trend value
type<-as.character(unique(counts$Survey.type))
type.sim<-data.frame(Year= rep(median(counts$Year), length(type)), Colony=rep("Bair Island Power Towers", length(type)), Region=rep("South Bay", length(type)), Survey.type=type, day=rep(round(mean(day, na.rm = T), 0), length(type)))
##get predications for new data
pred<-predict(model.plot, newdata = type.sim, se.fit=T)$fit %>% as.numeric()
pred.se<-predict(model.plot, newdata = type.sim, se.fit=T)$se.fit %>% as.numeric()
counts.type<-cbind(type.sim, pred, pred.se)

##plot effect of survey type on prediction
fig <- ggplot(data = counts.type, aes(x=type, y=pred))
fig <- fig + geom_point(size=2)
fig <- fig + geom_errorbar(aes(ymin=pred-pred.se, ymax=pred+pred.se, width=0.25))
fig <- fig + ylab("Predicted trend value")
fig <- fig + xlab("Survey type")
fig <- fig + scale_y_continuous(limits = c(min(pred-pred.se-0.5), max(pred+pred.se+0.5)), breaks = seq(1,10, 0.25))
fig <- fig + theme(text = element_text(size=16))
fig

png(filename = str_c("fig.type.effect.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()

##plot effect of survey type
data.temp<-subset(counts, select=c(Colony, Year, Count, Survey.type))
data.temp<- data.temp %>% spread(key = Survey.type, value = Count) %>% data.frame()

data.temp<-subset(data.temp, select=c(Aerial, Boat, Boat.Ground, Ground))

plot(Ground~Aerial, data=data.temp); abline(a = 0, b=1, lty="dashed"); abline(a= coefficients(lm(Ground~Aerial, data=data.temp))[1], b= coefficients(lm(Ground~Aerial, data=data.temp))[2])

##calculate % smaller
type.model<-lm(Ground~Aerial, data=data.temp)
type.fun<-function(x) {coefficients(type.model)[2]*x+ coefficients(type.model)[1]}

(200-type.fun(200))/(200+type.fun(200))*100

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
