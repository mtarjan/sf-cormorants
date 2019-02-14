##DCCO analysis
##January 23, 2019
##Prepared by Max Tarjan
##mtarjan@sfbbo.org


##load required packages
require(ggplot2)
require(dplyr)
require(tidyr)
library(stringr)
library(gdata) ##required for read.xls
library(BBmisc) ##required for normalize function
library(lubridate)
library(MuMIn) ##needed for AICc


##load DCCO nest counts
#counts<-read.csv("DCCO_counts_18Aug2017.csv")
counts<-read.xls("C:/Users/max/Desktop/Tarjan/Science/DCCO_counts_22Jan2019.xlsx")

##add zeros to the first years in counts
#counts.zero<-counts
#for (j in 1:length(unique(counts$Colony))) {
#  dat.temp<-subset(counts, Colony==unique(counts$Colony)[j])
#  min.yr.temp<-min(dat.temp$Year)
#  if(min.yr.temp==1984) {next}
#  yrs.temp<-1984:(min.yr.temp-1)
#  counts.zero<-rbind(counts.zero, data.frame(Colony=rep(dat.temp$Colony[1],length(yrs.temp)), Year=yrs.temp, Count=rep(0,length(yrs.temp)), Survey.Date=NA, Organization=NA, Survey.type=NA, lat=rep(dat.temp$lat[1], length(yrs.temp)), long=rep(dat.temp$long[1], length(yrs.temp)), Region=rep(dat.temp$Region[1], length(yrs.temp)), Count.type=rep("", length(yrs.temp)), Exclude.comments=rep("", length(yrs.temp)), Comments=NA, Incomplete.year=NA))
#}
#counts.zero<-counts.zero[order(counts.zero$Colony,counts.zero$Year),]

#counts<-counts.zero

counts$Colony<-as.character(counts$Colony)
translator <- c('Alviso Plant, Pond Nos. A9 and A10' = 'Alviso Ponds A9/A10', 
                'Alviso Plant, Pond No. A18' = 'Alviso Pond A18',
                'Dumbarton Bridge Power Towers' = 'Dumbarton Towers',
                'North San Pablo Bay Radar Target' = 'N. SP Bay Target',
                'Northeast San Pablo Bay Beacon' = 'NE SP Bay Beacon',
                'Petaluma Wastewater Treatment Plant' = 'Petaluma Plant',
                'Plant No. 1, Pond No. 3A' = 'Plant 1/Pond N3A',
                'San Mateo Bridge and PG&E Towers' = 'San Mateo Towers')
acronyms<-translator[counts$Colony]
counts$Colony[which(acronyms!='NA')]<-acronyms[which(acronyms!='NA')]

counts$Colony<-factor(counts$Colony)

##make blank survey.type NA
counts$Survey.type[which(counts$Survey.type=="")]<-NA
counts$Survey.type<-drop.levels(x = counts$Survey.type) ##drop unused level

##make edits to add incomplete year info
#counts$Incomplete.year[which(counts$Region=="North Bay" & counts$Year %in% c(1991, 1992, 1995:2002))]<-"yes" ##north bay region missing knight island counts 1995-2002
#counts$Incomplete.year[which(counts$Region=="Outer Coast" & counts$Year ==2000)]<-"yes" ##Outer Coast: For 2000, we have a 0 for Hog Island, but Lake Merced is ND. So I think the 0 in 2000 in this Chart should be removed
#counts$Incomplete.year[which(counts$Region=="South Bay" & counts$Year %in% c(1992, 1997, 1998, 2000:2002, 2004, 2006:2013, 2016:2017))]<-"yes"##South Bay: Most importantly, San Mateo Bridge (~ 100 nests) is ND for several years, especially after 2005. We know it’s been active
#counts$Incomplete.year[which(counts$Region=="South Farallon Islands" & counts$Year %in% c(1985, 1986, 1991, 1992, 2009, 2010, 2012))]<-"yes" ##eliminate years with ground data only

##EXPAND DATA

##format date
counts$Survey.Date<-as.Date(counts$Survey.Date, "%Y-%m-%d")

##add day of year
counts$day<-yday(counts$Survey.Date)

##add time periods (pre v post bridge construction)
#counts$time.period<-counts$Year
#counts$time.period[which(counts$time.period<=2002)]<-"pre"
#counts$time.period[which(counts$time.period>2002 & counts$time.period != "pre")]<-"post"

##get data for years with nearly complete data only, given Phil's designations
#counts.complete<-subset(counts, is.na(Incomplete.year))

counts.raw<-counts

##RESTRICTIONS
##exclude seasonal total count type
counts<-subset(counts, Count.type != "Seasonal total" & Region != "" & Region != "NA" & Exclude.comments=="" & is.na(Count)==F)

##add average count day if the day is missing
counts$day[which(is.na(counts$day))]<-round(mean(counts$day, na.rm=T), 0)

##add counts for sf bay region
sf.counts<-counts; sf.counts$Region<-"San Francisco Bay"
counts<-rbind(counts, sf.counts)

##VIEW DATA

##TABLE OF SAMPLE SIZE
ms.table1<-data.frame(count(x=unique(subset(counts, select=c(Region, Colony, lat, long, Year))), Region, Colony, lat, long));head(ms.table1)

#background basemap...need W, N, E, S bounds
#mapImage <- get_map(location = c(-121.9361, 37.6285, -122.1849, 37.4078),
#color = "color",
#                    source = "google",
#                    maptype = "satellite",
#                    zoom=11)

#fig1<-ggplot(data = counts, aes(x = Year, y=Count, color=Colony)) + geom_point()
#fig1 <- fig1 + geom_smooth(method="lm", se = F, aes(linetype = Region))
#fig1 <- fig1 + ylab("Number of DCCO nests")
#fig1

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
    max.colony.counts$Survey.type[j]<-as.character(type.temp[1])
  }
}

##summarize data by region
#regional.counts<-counts %>% group_by(Region, Year) %>% summarise(total=sum(Count)) %>% data.frame()
regional.counts<-max.colony.counts %>% group_by(Region, Year) %>% summarise(total=sum(max.count)) %>% data.frame()

#regional.counts$time.period<-regional.counts$Year
#regional.counts$time.period[which(regional.counts$time.period<=2002)]<-"pre"
#regional.counts$time.period[which(regional.counts$time.period>2002 & regional.counts$time.period != "pre")]<-"post"

##counts number of colonies tracked in each region
#n.colonies<-data.frame(table(counts$Region, counts$Year))
#n.colonies[,2]<-as.numeric(as.character(n.colonies[,2]))
#colnames(n.colonies)<-c("Region", "Year", "n.sites")

##add the number of sites
regional.counts$n.sites<-rep(0, nrow(regional.counts))
for (j in 1:nrow(regional.counts)) {
  region.temp<-regional.counts$Region[j]
  year.temp<-regional.counts$Year[j]
  regional.counts$n.sites[j]<-length(unique(subset(counts, Region==region.temp & Year==year.temp)$Colony))
}

##plot of nest counts by year and site
#fig2 <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=total, colour=as.factor(time.period), fill=as.factor(time.period)))
#fig2 <- fig2 + geom_bar(stat="identity")
#fig2 <- fig2 + geom_point(size=2, shape=21)
#fig2 <- fig2 + geom_smooth(method = "lm", aes(linetype = as.factor(time.period)), fill="darkgrey")
#fig2 <- fig2 + scale_color_manual(values= c("black", "black"), name = "Pre- or Post- \nBridge Construction") + scale_fill_manual(values= c("black", "white"), name = "Pre- or Post- \nBridge Construction")
#fig2 <- fig2 + scale_linetype_manual(values=c("solid", "twodash"), name = "Pre- or Post- \nBridge Construction")
#fig2 <- fig2 + facet_wrap(~Region, strip.position="top") ##split up sites with facets; choose this option or the one below
#fig2 <- fig2 + geom_bar(stat="identity", aes(fill=Region)) + scale_fill_manual(values=mycols, name="") ##stacked barplot with sites as the colors. can change the colors to region when have those assigned (but need to summarize data by region first)
#fig2 <- fig2 + ylab("Number of DCCO nests")
#fig2 <- fig2 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
#fig2 <- fig2 + scale_y_continuous(breaks=seq(0, 2500, 200), expand=c(0,0), limits = c(0, NA))
#fig2 <- fig2 + theme_bw()
#fig2 <- fig2 + theme(panel.spacing = unit(0.25, "in"))
#fig2 <- fig2 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#fig2 <- fig2 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
#fig2

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
fig3 <- fig3 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1984,2017))
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
#fig3a <- ggplot(data = subset(regional.counts, subset = Region !=""), aes(x = Year, y=log(total)))
#fig3a <- fig3a + geom_point(size=2)
#fig3a <- fig3a + geom_smooth(method = "loess")
#fig3a <- fig3a + facet_wrap(~Region, strip.position="top", scales="free") 
#fig3a <- fig3a + ylab("ln(Number of DCCO nests)")
#fig3a <- fig3a + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1985,2017))
##fig3 <- fig3 + scale_y_continuous(breaks=seq(0, 2500, 100), expand=c(0,0), limits = c(0, NA))
#fig3a <- fig3a + theme_classic()
#fig3a <- fig3a + theme()
##fig3 <- fig3 + theme(panel.border = element_rect(color="black", fill=NA))
#fig3a <- fig3a + theme(panel.spacing = unit(0.25, "in"))
#fig3a <- fig3a + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#fig3a <- fig3a + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
#fig3a <- fig3a + theme(strip.background = element_rect(fill=NULL, linetype = "blank"))
#fig3a

##find breaks according to loess on log(counts)
#out<-dim(0)
#for (j in 1:length(unique(regional.counts$Region))) {
#  dat.temp<-subset(regional.counts, Region==unique(regional.counts$Region)[j] & total >0)
#  loess.temp<-loess(formula = log(total) ~ Year, data = dat.temp)
#  predict.temp<-predict(loess.temp)
  ##curve.pts<-cbind(dat.temp$Year, predict.temp)
  
#  dy <- diff(predict.temp) 
  
#  cutoff<-dim(0)
#  for (i in 6:(length(dy)-5)) {
#    if (t.test(x=dy[i:(i-4)], y=dy[(i+1):(i+5)])$p.value<0.1) {
#      cutoff<-c(cutoff, dat.temp$Year[i])
#    }
#  }
#  cutoff<-round(median(cutoff),0)
  ##print(cutoff)
#  out<-rbind(out, c(as.character(unique(regional.counts$Region)[j]), cutoff))
#}
#cutoff<-data.frame(out)
#cutoff$X2<-as.numeric(as.character(cutoff$X2))

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
#annual.counts<-subset(counts, Region !="" & Region !="Bridges") %>% group_by(Year) %>% summarise(total=sum(Count)) %>% data.frame()
##annual.counts<-subset(counts, Region !="" ) %>% group_by(Year) %>% summarise(total=sum(Count)) %>% data.frame()

#fig4 <- ggplot(data = annual.counts, aes(x = Year, y=total))
#fig4 <- fig4 + geom_point(size=2)
#fig4 <- fig4 + geom_smooth(method = "loess")
#fig4 <- fig4 + ylab("Number of DCCO nests at non-bridge sites")
#fig4 <- fig4 + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0))
#fig4 <- fig4 + scale_y_continuous(breaks=seq(0, 5500, 200), expand=c(0,0), limits = c(0, NA))
#fig4 <- fig4 + theme_bw()
#fig4 <- fig4 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#fig4 <- fig4 + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
#fig4

#png(filename = "fig4.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig4; dev.off()


##plot of change in dcco numbers (slope following bridge construction) with distance from bridge

##add mean distance to the two bridges
#bridge.dist<-as.matrix(dist(subset(counts, select = c("lat", "long")), method = "euclidean"))
#bb.dist<-bridge.dist[,which(counts$Colony=="S.F.-Oakland Bay Br. East")[1]]
#rb.dist<-bridge.dist[,which(counts$Colony=="Richmond-San Rafael Bridge")[1]]
#counts$bridge.dist<-apply(X = cbind(bb.dist, rb.dist), MARGIN = 1, FUN = mean)

##growth rate of colony post 2002
#out<-data.frame(site = rep(NA, length(unique(counts$Colony))), Region = rep(NA, length(unique(counts$Colony))), bridge.dist = rep(NA, length(unique(counts$Colony))), slope = rep(NA, length(unique(counts$Colony))))
#for (j in 1:length(unique(counts$Colony))) {
#  row.temp<-subset(counts, subset= Colony == as.character(unique(counts$Colony)[j]))
#  site<-as.character(unique(counts$Colony)[j])
#  if (nrow(subset(counts, subset = Colony==site & Year >2002))>1) {
#    slope<-lm(formula = Count ~ Year, data = subset(counts, subset = Colony==site & Year >2002))$coefficients[2]
#    out$site[j]<-site
#    out$Region[j]<-as.character(row.temp$Region[1])
#    out$bridge.dist[j]<-row.temp$bridge.dist[1]
#    out$slope[j]<-slope
#  }
#}

#out<-out[-which(is.na(out$slope)),] ##exclude sites for which slope could not be calculated (too few sites)

#ggplot(data=out, aes(x = Region, y = slope)) + geom_boxplot() + ylab("Change in DCCO counts 2002-2017")

##plot sites on a map
#map1<-ggplot(data = counts, aes(x = UTM.Easting, y = UTM.Northing)) + geom_point(aes(size=nests))
#map1 <- map1 + facet_wrap(facets = ~time.period)
#map1

#par(mfrow=c(1,length(unique(counts$time.period))))

##plot summary counts for sf bay region
fig <- ggplot(data = subset(regional.counts, subset = Region =="San Francisco Bay"), aes(x = Year, y=total))
fig <- fig + geom_point(size=2)
fig <- fig + geom_path(size=1.1)
fig <- fig + ylab("Number of DCCO nests")
fig <- fig + scale_x_continuous(breaks=seq(1980, 2017, 2), expand=c(0,0), limits=c(1984,2018))
fig <- fig + scale_y_continuous(breaks=seq(0, max(regional.counts$total)+200, 200), expand=c(0,0), limits = c(0, max(regional.counts$total)+200))
fig <- fig + theme_classic()
fig <- fig + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
fig <- fig + theme(axis.title.y = element_text(margin = margin(r=1, unit="line")))
fig

png(filename = "SFBay.counts.fig.png", units="in", width=5, height=4,  res=200);fig; dev.off()

##plot raw counts for all regions
for (j in 1:length(unique(counts$Region))) {
  region.temp<-unique(counts$Region)[j]
  if (region.temp=="San Francisco Bay") {next}
  data.plot<-subset(counts, Region==region.temp)
  range<-c(min(data.plot$Count, na.rm=T), max(data.plot$Count, na.rm=T))
  
  fig <- ggplot(data = data.plot, aes(x=Year))
  fig <- fig + geom_point(aes(y=Count))
  #fig <- fig + geom_point(aes(y=Count, color=Survey.type.y))
  #fig <- fig + scale_colour_discrete(name="Survey type")
  fig <- fig + theme_classic()
  fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
  fig <- fig + ggtitle(region.temp) + ylab(label = "Number of DCCO nests")
  fig <- fig + facet_wrap(~Colony, scales="free")
  fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 5), lim=c(1985,2017)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
  fig <- fig + scale_y_continuous(breaks = seq(range[1], range[2],round((range[2]-range[1])/10, 0)), lim=c(range[1], range[2]))
  fig
  
  #fig.heights<-c(3, 7.5, 5, 6, 5)
  fig.height<-sqrt(length(unique(data.plot$Colony)))*2
  png(filename = str_c("figs.raw.counts/fig.",region.temp, ".counts.all.colonies.png"), units="in", width=6.5, height=fig.height,  res=200);print(fig); dev.off()
}

##LOAD PHIL'S REGIONAL COUNTS
phil.data<-read.csv("C:/Users/max/Desktop/Tarjan/Science/DCCO/DCCO_regional_counts_Phil_12Jul2017.csv")
phil.data<-tidyr::gather(phil.data, "Year", "Count", 2:ncol(phil.data)) ##rearrange data
phil.data$Year<-as.numeric(str_sub(phil.data$Year, 2, 5)) ##format year (remove space)
phil.data<-subset(phil.data, subset = Count!='NA')
colnames(phil.data)<-c("Region", "Year", "total")

##data<-read.csv("DCCO_regional_counts_Phil_12Jul2017.csv")
##data.org<-data
##data<-tidyr::gather(data, "Year", "Count", 2:ncol(data)) ##rearrange data
##data$Year<-as.numeric(str_sub(data$Year, 2, 5)) ##format year (remove space)
##data<-subset(data, subset = Count!='NA')

fig3d <- ggplot(data = subset(phil.data, subset = Region !="NonBridge"), aes(x = Year, y=total))
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

png(filename = "fig3.complete.years.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig3d; dev.off()

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

#source("DCCO_lm_analysis_07Mar2018.R")

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

##SUBSET COUNTS TO BIG SITES
large.sites<-counts %>% group_by(Region, Colony) %>% summarise(max.count=max(Count), mean.count=mean(Count), n=length(Count)) %>% data.frame() %>% subset(subset = max.count>=10 & n>=10)
as.character(sort(large.sites$Colony))
large.sites<-large.sites$Colony

##LOESS PREDICTIONS FOR EACH COLONY
#counts.loess<-dim(0)
#for (j in 1:length(unique(counts$Colony))) {
  #j<-j+1
  #col.temp<-unique(counts$Colony)[j]
  #dat.temp<-subset(counts, Colony==col.temp)
  #if (nrow(subset(dat.temp, Count>0))<3) {next}
  
  #fit.loess<-loess(formula = Count~Year, data = dat.temp)
  #pred.l<-predict(object = fit.loess, newdata = data.frame(Year=min(dat.temp$Year):max(dat.temp$Year)), se = T)
  #counts.loess<-rbind(counts.loess, data.frame(Colony=col.temp, Region = dat.temp$Region[1], Year=min(dat.temp$Year):max(dat.temp$Year), pred=pred.l$fit, se.upper=pred.l$fit +pred.l$se.fit, se.lower=pred.l$fit-pred.l$se))
  
  ##replace values below 0 with 0
  #if (length(which(counts.loess$pred<0))>0) {counts.loess[which(counts.loess$pred<0),]$pred<-0}
  #if (length(which(counts.loess$se.lower<0))>0) {counts.loess[which(counts.loess$se.lower<0),]$se.lower<-0}
  #if (length(which(counts.loess$se.upper<0))>0) {counts.loess[which(counts.loess$se.upper<0),]$se.upper<-0}
  
  #dat.plot<-subset(counts.loess, Colony==col.temp)
  #fig <- ggplot(data = dat.plot, aes(x = Year))
  #fig <- fig + geom_path(aes(y = pred))
  #fig <- fig + geom_path(aes(y = se.upper), lty="dashed") + geom_path(aes(y = se.lower), lty="dashed")
  #fig <- fig + geom_point(data = dat.temp, aes(x = Year, y = Count))
  #fig <- fig + facet_grid(.~Colony)
  #fig
#}

#counts.loess<-dplyr::left_join(counts.loess, y=subset(counts, select=c(Colony, Year, Count)))

##GAM
##code from Shadish et al. 2014. Using generalized additive (mixed) models to analyze single case designs. (in Mendeley)

library(mgcv)
library(lattice) ##for plotting

M0<-gam(Count ~ s(Year, by=Colony) + Colony,
        data = counts,
        family = poisson)

M1<-gam(Count ~ s(Year, by=Colony) + Colony + s(day),
        data = counts,
        family = poisson)

M2<-gam(Count ~ s(Year, by=Colony) + Colony + Survey.type,
        data = counts,
        family = poisson)

M3<-gam(Count ~ s(Year, by=Colony) + Colony + s(day) + Survey.type,
        data = counts,
        family = poisson)



MuMIn::AICc(M0, M1, M2, M3)
##look for lowest AIC


##TABLE TO COMPARE MODEL FIT
#model.fit<-round(AIC(M0, M1, M2, M3),0)
##do delta AICc instead
##AIC scores are often shown as ∆AIC scores, or difference between the best model (smallest AIC) and each model (so the best model has a ∆AIC of zero).
model.fit<-MuMIn::AICc(M0, M1, M2, M3)
model.fit$deltaAICc<-round(model.fit$AICc-min(model.fit$AICc),0)
model.fit$Model<-c(M0$formula, M1$formula, M2$formula, M3$formula)
model.fit$UBRE<-round(c(summary(M0)$sp.criterion, summary(M1)$sp.criterion, summary(M2)$sp.criterion, summary(M3)$sp.criterion),3)
model.fit$deviance.explained<-round(c(summary(M0)$dev.expl, summary(M1)$dev.expl, summary(M2)$dev.expl, summary(M3)$dev.expl),3)
model.fit<-subset(model.fit, select=c(Model, df, deltaAICc, UBRE, deviance.explained))
model.fit<-model.fit[order(model.fit$deltaAICc),]

##SELECT MODEL TO PLOT
model.plot<-M3

##plot model
#P<-predict(model.plot, se.fit = T)
#plot(model.plot$model$Year, model.plot$fitted.values)

##diagnostic plots
#E1<-resid(model.plot, type="pearson")
#F1<-fitted(model.plot)
#plot(x=F1, y=E1); abline(h=0)

##residuals for every colony
#xyplot(E8~F8 | M8$model$Colony)

##histogram of residuals
#hist(E1)

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

##make new.dat that includes entire range of years for every colony
new.dat<-data.frame(Year=rep(min(counts$Year):max(counts$Year), length(unique(counts$Colony))), Colony=rep(unique(counts$Colony), length(min(counts$Year):max(counts$Year)))%>% sort())
##make new.dat that includes range of years after the first year that each colony has data
#new.dat<-dim(0)
#max.year<-max(counts$Year)
#for (j in 1:length(unique(counts$Colony))) {
#  colony.temp<-unique(counts$Colony)[j]
#  min.year<-min(subset(counts, Colony==colony.temp)$Year)
#  max.year<-max(subset(counts, Colony==colony.temp)$Year)
#  new.dat<-rbind(new.dat, data.frame(Colony=colony.temp, Year=min.year:max.year))
#}
##make new.dat that only has years with known counts
#new.dat<-subset(counts, select=c(Colony, Year))

new.dat<-dplyr::left_join(new.dat, y=subset(counts, Region!="San Francisco Bay", select=c(Region,Colony)), by = c("Colony","Colony"))
new.dat<-unique(new.dat)
new.dat<-subset(new.dat, Region!="")
new.dat$Survey.type<-as.factor(rep("Ground", nrow(new.dat)))
new.dat$day<-round(mean(counts$day, na.rm=T),0)
new.dat$Year<-as.numeric(as.character(new.dat$Year))
new.dat$Colony<-as.factor(new.dat$Colony)

##duplicate new.dat so have SF Bay as a region
new.dat<-rbind(new.dat, data.frame(Year=new.dat$Year, Colony=new.dat$Colony, Region="San Francisco Bay", Survey.type=new.dat$Survey.type, day=new.dat$day))

##use Model.plot
##PREDICT ACROSS FULL RANGE OF YEARS FOR EACH SITE; THEN CALCUATE REGIONAL PREDICTED COUNTS
predictions<-predict.gam(object = model.plot, newdata = new.dat, type = "link", se.fit = T) ##TYPE CAN BE RESPONSE OR LINK
pred.link<-as.numeric(predictions$fit)
pred.se.link<-as.numeric(predictions$se.fit)

##response predictions
predictions<-predict.gam(object = model.plot, newdata = new.dat, type = "response", se.fit = T) ##TYPE CAN BE RESPONSE OR LINK
pred<-as.numeric(predictions$fit)
pred.se<-as.numeric(predictions$se.fit)

##former version that got predictions on the linear predictor scale
#pred<-predict(model.plot, newdata = new.dat, se.fit=T)$fit %>% as.numeric()
#pred.se<-predict(model.plot, newdata = new.dat, se.fit=T)$se.fit %>% as.numeric()
##confidence intervals
#ci<-MASS::confint(model.plot, level=0.95, trace=F)
counts.m8<-cbind(new.dat, pred, pred.se, pred.link, pred.se.link)
counts.m8<-subset(counts.m8, pred !="Inf" & pred.link!="Inf")

##add field counts to predicted counts.m8
#counts.temp<-counts; counts.temp$colony.year<-str_c(as.character(counts$Colony), counts$Year)
#counts.m8.temp<-counts.m8; counts.m8.temp$colony.year<-str_c(as.character(counts.m8$Colony), counts.m8$Year)
#counts.m8<-dplyr::left_join(counts.m8.temp, y=subset(counts.temp, select=c(colony.year, Count)), by = c("colony.year","colony.year")) ##THIS INTRODUCES DUPLICATE DATE/COLONY ROWS

##use mean or max colony counts instead
counts.temp<-max.colony.counts; counts.temp$region.colony.year<-str_c(as.character(counts.temp$Region),as.character(counts.temp$Colony), counts.temp$Year)
counts.m8.temp<-counts.m8; counts.m8.temp$region.colony.year<-str_c(as.character(counts.m8$Region), as.character(counts.m8$Colony), counts.m8$Year)
counts.m8<-dplyr::left_join(counts.m8.temp, y=subset(counts.temp, select=c(region.colony.year, Survey.type, max.count)), by = c("region.colony.year","region.colony.year"))
counts.m8<-subset(counts.m8, select=-region.colony.year)
colnames(counts.m8)[ncol(counts.m8)]<-"Count"
head(counts.m8)

#plot(pred~Count, data=counts.m8)
##subset counts.m8 to years when there is known info about each colony
counts.m8.sub<-dim(0)
for (j in 1:length(unique(counts.m8$Colony))) {
  colony.temp<-unique(counts.m8$Colony)[j]
  min.year<-min(subset(counts.m8, Colony==colony.temp & is.na(Count)==F)$Year, na.rm=T)
  max.year<-max(subset(counts.m8, Colony==colony.temp & is.na(Count)==F)$Year, na.rm=T)
  
  counts.m8.sub<-rbind(counts.m8.sub, subset(counts.m8, Colony==colony.temp & Year >= min.year & Year <= max.year))
}

head(counts.m8.sub)

##overwrite with initial counts.m8
#counts.m8.sub<-counts.m8

#counts.m8<-counts.m8.sub

##add normalized predictor by colony
#counts.m8.sub$pred.norm<-rep(NA, nrow(counts.m8.sub))
#for (j in 1:length(unique(counts.m8.sub$Colony))) {
#  colony.temp<-unique(counts.m8.sub$Colony)[j]
#  dat.temp<-subset(counts.m8.sub, Colony==colony.temp)
#  range.min<-min(dat.temp$Count, na.rm=T)
#  range.max<-max(dat.temp$Count, na.rm=T)
#  norm.temp<-normalize(dat.temp$pred, range=c(range.min, range.max), method="range")
#  counts.m8.sub$pred.norm[which(counts.m8.sub$Colony==colony.temp)]<-norm.temp
#}

##subset counts m8 to get large colonies only
counts.m8<-subset(counts.m8, Colony %in% large.sites)

##subset counts.m8 to get preds with error less than 2* max count
#counts.m8<-subset(counts.m8, pred.se<max(counts$Count, na.rm=T)*2)

##get a weight for the predictions based on relative colony size (the average size of the colony / the size of the regional counts) and relative error (estimated erro / sum of error for all sites in the region in that year). relative colony size and relative error should each sum to one across colonies in year x for each region
out<-dim(0)
for (j in 1:nrow(counts.m8)){
  colony.temp<-counts.m8$Colony[j]
  region.temp<-counts.m8$Region[j]
  colony.mean<-mean(subset(counts.m8, Colony==colony.temp)$Count, na.rm=T) ##mean colony size across years
  col.mean.sum<-subset(counts.m8, Region==region.temp) %>% group_by(Colony) %>% summarise(col.mean = mean(Count, na.rm=T)) %>% data.frame() %>% summarise(col.mean.sum=sum(col.mean)) %>% as.numeric() ##sum of mean colony sizes
  #region.n<-length(unique(subset(counts.m8, Region==region.temp)$Colony)) ##number of colonies in region
  size.weight<-colony.mean/col.mean.sum ##colony size/sum of all colony sizes
  
  #regional.counts.temp<-subset(counts.m8, Region==region.temp) %>% group_by(Region, Year) %>% summarise(total=sum(Count, na.rm=T)) %>% data.frame
  #regional.mean<-mean(regional.counts.temp$total, na.rm=T)
  
  ##error weight
  ##error at that colony in year x divided by sum of regional error in year x
  year.temp<-counts.m8$Year[j]
  error.year<-subset(counts.m8, Year==year.temp & Region==region.temp) %>% summarise(error.year=sum(pred.se)) %>% as.numeric()
  n.temp<-length(unique(subset(counts.m8, Region==region.temp & Year==year.temp)$Colony))
  error.weight<-(1-counts.m8$pred.se[j]/error.year)/(n.temp-1)
  
  if (length(unique(subset(counts.m8, Region==region.temp & Year==year.temp)$Colony))==1) {error.weight<-1} ##if there is only one colony in the region, then the error weight is 1
  
  out<-rbind(out, c(size.weight, error.weight))
}
counts.m8$weight<-out[,1] ##mean colony size as a fraction of mean regional size
counts.m8$error.weight<-out[,2]

##check that weights within each year sum to 1
counts.m8 %>% group_by(Year, Region) %>% summarize(weights=sum(error.weight))

##divide the weight by the standard error (less weight given to estimates with more se)
#counts.m8$weight2<-ifelse(counts.m8$pred.se==0, counts.m8$weight, counts.m8$weight/counts.m8$pred.se)

##GET REGIONAL PREDICTIONS
##WEIGHT THE predictions BY POPULATION SIZE (and variance)
regional.pred<-counts.m8 %>% group_by(Year, Region) %>% summarise(total=sum(Count, na.rm=T), pred.regional=sum(pred*weight*error.weight)/sum(weight*error.weight), pred.regional.link=sum(pred.link*weight*error.weight)/sum(weight*error.weight)) %>% data.frame()

#regional.pred.loess<-counts.loess %>% group_by(Year, Region) %>% summarise(total=sum(Count, na.rm=T), pred.regional=sum(pred)) %>% data.frame()

##ITERATE TO GET MEAN REGIONAL PREDICTION AND CI
##replace pred by randomly selected pred within a range of values
##assume predictions come from a normal distribution with se==pred.se
##assume sd = se * sqrt(df+1)
##add df to counts.m8
edf.colony<-subset(model.plot$edf, str_detect(names(model.plot$edf), pattern="Colony"))
names(edf.colony)<-str_sub(names(edf.colony), 15, -3)
edf.df<-data.frame(Colony=names(edf.colony), edf=edf.colony)
edf.sum<-edf.df %>% group_by(Colony) %>% summarise(edf.sum=sum(edf)) %>% data.frame()

counts.m8<-dplyr::left_join(counts.m8, y=edf.sum, by = c("Colony","Colony"))
counts.m8$pred.sd<-counts.m8$pred.se*sqrt(counts.m8$edf.sum+1)
counts.m8$pred.sd.link<-counts.m8$pred.se.link*sqrt(counts.m8$edf.sum+1)
rep<-10000
pred.rep<-apply(X = subset(counts.m8, select=c(pred, pred.sd)), MARGIN = 1, FUN = function(x,y,z,n) rnorm(n = n, mean=x[y], sd=x[z]), n = rep, y=1, z=2) %>% data.frame() %>% t() ##alter this to change assumption about error around model predicted estimates
#pred.rep<-apply(X = subset(counts.m8, select=pred), MARGIN = 1, FUN = function(x,n) rpois(n = n, lambda =x[1]), n = rep) %>% data.frame() %>% t() ##assume poisson
pred.rep.link<-apply(X = subset(counts.m8, select=c(pred.link, pred.sd.link)), MARGIN = 1, FUN = function(x,y,z,n) rnorm(n = n, mean=x[y], sd=x[z]), n = rep, y=1, z=2) %>% data.frame() %>% t() ##alter this to change assumption about error around model predicted estimates

#pred.rep.loess<-apply(X = subset(counts.loess, select=c(pred, se.upper, se.lower)), MARGIN=1, FUN = function (x,n) runif(n = n, min = x[3], max=x[2]), n=rep)

##visualize replicate distributions
#boxplot(pred.rep[1,]); mean(pred.rep[1,]); counts.m8$pred[1]
#boxplot(rnorm(mean=counts.m8$pred[1], sd=counts.m8$pred.sd[1], n=100))


regional.pred.rep<-regional.pred
for (j in 1:ncol(pred.rep)) {
  counts.temp<-counts.m8
  counts.temp$pred<-pred.rep[,j] ##replace prediction with replicate prediction
  counts.temp$pred.link<-pred.rep.link[,j] ##replace link predictions with reps
  regional.pred.temp<-counts.temp %>% group_by(Year, Region) %>% summarise(pred.regional=sum(pred*weight*error.weight)/sum(weight*error.weight), pred.regional.link=sum(pred.link*weight*error.weight)/sum(weight*error.weight)) %>% data.frame()
  regional.pred.rep<-cbind(regional.pred.rep, r.pred.rep=regional.pred.temp$pred.regional, r.pred.link.rep=regional.pred.temp$pred.regional.link)
}

##do the same for loesss
#regional.pred.rep.loess<-regional.pred.loess
#for (j in 1:ncol(pred.rep.loess)) {
#  counts.temp<-counts.loess
#  counts.temp$pred<-pred.rep.loess[,j] ##replace prediction with replicate prediction
#  regional.pred.temp<-counts.temp %>% group_by(Year, Region) %>% summarise(pred.regional=sum(pred)) %>% data.frame()
#  regional.pred.rep.loess<-cbind(regional.pred.rep.loess, r.pred.rep=regional.pred.temp$pred.regional)
#}

##get mean regional prediction and CI
r.pred.mean<-subset(regional.pred.rep, select=str_detect(names(regional.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = mean) %>% as.numeric() ##take only replicates
r.pred.sd<-subset(regional.pred.rep, select=str_detect(names(regional.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = sd) %>% as.numeric()
r.pred.lower<-r.pred.mean - 1.96*r.pred.sd/sqrt(rep)
r.pred.upper<-r.pred.mean + 1.96*r.pred.sd/sqrt(rep)

regional.pred$r.pred.mean<-r.pred.mean
regional.pred$r.pred.sd<-r.pred.sd
regional.pred$r.ci.lower<-r.pred.lower
regional.pred$r.ci.upper<-r.pred.upper

##get mean regional prediction and CI for link
r.pred.mean<-subset(regional.pred.rep, select=str_detect(names(regional.pred.rep), pattern="r.pred.link.rep")) %>% apply(MARGIN = 1, FUN = mean) %>% as.numeric() ##take only replicates
r.pred.sd<-subset(regional.pred.rep, select=str_detect(names(regional.pred.rep), pattern="r.pred.link.rep")) %>% apply(MARGIN = 1, FUN = sd) %>% as.numeric()
r.pred.lower<-r.pred.mean - 1.96*r.pred.sd/sqrt(rep)
r.pred.upper<-r.pred.mean + 1.96*r.pred.sd/sqrt(rep)

regional.pred$r.pred.mean.link<-r.pred.mean
regional.pred$r.pred.sd.link<-r.pred.sd
regional.pred$r.ci.lower.link<-r.pred.lower
regional.pred$r.ci.upper.link<-r.pred.upper

##replace missing years of counts with NA
for (j in 1:nrow(regional.pred)) {
  dat.temp<-subset(regional.counts, Year==regional.pred$Year[j] & Region==regional.pred$Region[j])
  if (nrow(dat.temp)==0) {
    regional.pred$total[j]<-NA
  }
}

regional.pred.original<-regional.pred

##make all values positive by adding minimum value
#min.val<-min(subset(regional.pred, select=c(pred.regional,r.pred.sd,r.ci.lower,r.ci.upper)))
#regional.pred<-cbind(subset(regional.pred, select=c(Year, Region,total)), subset(regional.pred, select=c(pred.regional, r.pred.mean,r.pred.sd, r.ci.lower,r.ci.upper))+abs(min.val))

#min.year<-rep(1985, 5)
min.year<-subset(regional.counts, is.na(total)==F) %>% group_by(Region) %>% summarize(min.year=min(Year)) %>% data.frame()
min.year$min.year<-c(1984, 1990, 1997, 1990, 1987, 1990)

##TABLE OF PERCENT CHANGE
#Regions<-sort(as.character(unique(regional.pred$Region)))
Regions<-min.year$Region
dur<-c(str_c(min.year$min.year, "-2002"), rep("2003-2017",length(Regions)), str_c(min.year$min.year, "-2017"))
change.dat<-data.frame(Region=rep(Regions,3), Years=dur, start=c(min.year$min.year, rep(2003, length(Regions)), min.year$min.year), end=c(rep(2002, length(Regions)), rep(2017, length(Regions)*2)), percent.change=NA, percent.change.rep=NA, lower95=NA, upper95=NA, q1=NA, q3=NA, growth=NA, growth.rep=NA, growth.lower95=NA, growth.upper95=NA, growth.q1=NA, growth.q3=NA)

per.change.func<-function(x,y) {
  x[which(round(x,0)<=0)]<-1
  y[which(round(y,0)<=0)]<-1
  #ifelse(y>x & ((y-x)/x)<0,-round((y-x)/x*100,2), ifelse(y<x & ((y-x)/x)>0,-round((y-x)/x*100,2),round((y-x)/x*100,2)))
  round((y-x)/x*100,2)
  }

growth.func<- function(x,y, year1, year2) {(y-x)/(year2-year1)} ##function to calculate growth rate

for (j in 1:nrow(change.dat)) {
  region.temp<-as.character(change.dat$Region[j])
  start.year.temp<-change.dat$start[j]
  end.year.temp<-change.dat$end[j]
  
  initial.temp<-subset(regional.pred, as.character(Region)==region.temp & Year==start.year.temp)
  final.temp<-subset(regional.pred, as.character(Region)==region.temp & Year==end.year.temp)
  
  ##use function
  change.dat$percent.change[j]<-per.change.func(initial.temp$pred.regional, final.temp$pred.regional)
  
  ##get distribution of % change and take mean and 95% CI
  initial.rep<-as.numeric(subset(regional.pred.rep, Region==region.temp & Year==start.year.temp, select= str_detect(names(regional.pred.rep), pattern="r.pred.rep")))
  final.rep<-as.numeric(subset(regional.pred.rep, Region==region.temp & Year==end.year.temp, select= str_detect(names(regional.pred.rep), pattern="r.pred.rep")))
  
  ##get values for link reps
  initial.rep.link<-as.numeric(subset(regional.pred.rep, Region==region.temp & Year==start.year.temp, select= str_detect(names(regional.pred.rep), pattern="r.pred.link.rep")))
  final.rep.link<-as.numeric(subset(regional.pred.rep, Region==region.temp & Year==end.year.temp, select= str_detect(names(regional.pred.rep), pattern="r.pred.link.rep")))

  ##use percent change function
  percent.change.rep<-per.change.func(initial.rep, final.rep)
  
  mean.change<-round(mean(percent.change.rep, na.rm = T),2)
  med.change<-round(median(percent.change.rep, na.rm = T),2)
  sd.change<-round(sd(percent.change.rep, na.rm=T),2)
  
  change.dat$percent.change.rep[j]<-mean.change
  
  ##calc confidence intervals
  ##order the values to get CI
  rep.ord<-percent.change.rep[order(percent.change.rep)]
  l95.temp<-rep.ord[round(0.025*length(percent.change.rep),0)]
  u95.temp<-rep.ord[round(0.975*length(percent.change.rep),0)]
  
  ##assign upper and lower CI
  change.dat$lower95[j]<-l95.temp
  change.dat$upper95[j]<-u95.temp
  change.dat$q1[j]<-as.numeric(summary(rep.ord)[2])
  change.dat$q3[j]<-as.numeric(summary(rep.ord)[5])
  
  #hist(percent.change.rep); abline(v=change.dat$percent.change[j])
  ##ADD PERCENT CHANGE IN RAW COUNTS FITTED TO LOESS
  #raw.loess<-loess(formula = total~Year, data = subset(regional.pred, Region==region.temp))
  
  #raw.start<-predict(object = raw.loess, newdata = data.frame(Year=start.year.temp))
  #raw.end<-predict(object = raw.loess, newdata = data.frame(Year=end.year.temp))
  #change.dat$raw.change[j]<-per.change.func(raw.start, raw.end)
  
  ##calc GROWTH RATE
  change.dat$growth[j]<-growth.func(x = initial.temp$pred.regional.link, y = final.temp$pred.regional.link, year1 = start.year.temp, year2 = end.year.temp)
  ##get it for the reps
  growth.rep<-growth.func(x=initial.rep.link, y=final.rep.link, year1 = start.year.temp, year2 = end.year.temp)
  ##med and CI for growth
  change.dat$growth.rep[j]<-round(mean(growth.rep, na.rm = T),2) ##median growth rate
  rep.growth.ord<-growth.rep[order(growth.rep)] ##order the reps
  change.dat$growth.lower95[j]<-round(rep.growth.ord[round(0.025*length(rep.growth.ord),0)],2)
  change.dat$growth.upper95[j]<-round(rep.growth.ord[round(0.975*length(rep.growth.ord),0)],2)
  change.dat$growth.q1[j]<-round(as.numeric(summary(rep.growth.ord)[2]),2)
  change.dat$growth.q3[j]<-round(as.numeric(summary(rep.growth.ord)[5]),2)
}

change.dat<-change.dat[order(change.dat$Region),]

##plot trends for each colony
for (j in 1:length(unique(regional.pred$Region))) {
  region.temp<-unique(regional.pred$Region)[j]
  
  if (region.temp!="San Francisco Bay") {
    ##plot trends by colony
    data.plot<-subset(counts.m8.sub, Region==region.temp)
    
    range<-c(min(data.plot$Count, na.rm=T), max(data.plot$Count, na.rm=T))
    range.pred<-round(c(min(data.plot$pred-data.plot$pred.se, na.rm=T), max(data.plot$pred+data.plot$pred.se, na.rm=T)),1) 
    
    fig <- ggplot(data = data.plot, aes(x=Year))
    fig <- fig + geom_point(aes(y=Count))
    #fig <- fig + geom_point(aes(y=Count, color=Survey.type.y))
    #fig <- fig + geom_path(aes(y=pred.norm))
    #fig <- fig + scale_colour_discrete(name="Survey type")
    fig <- fig + theme_classic()
    fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
    fig <- fig + ggtitle(region.temp) + ylab(label = "Number of DCCO nests")
    fig <- fig + facet_wrap(~Colony, scales="free")
    fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 5), lim=c(1985,2017)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
    fig <- fig + scale_y_continuous(breaks = seq(range[1], range[2],round((range[2]-range[1])/10, 0)), lim=c(range[1], range[2]))
    fig
    
    #fig.heights<-c(3, 7.5, 5, 6, 5)
    fig.height<-sqrt(length(unique(data.plot$Colony)))*2
    png(filename = str_c("fig.",region.temp, ".counts.colonies.png"), units="in", width=6.5, height=fig.height,  res=200);print(fig); dev.off()
    
    ##plot the colony predictions with SE
    data.plot<-subset(counts.m8.sub, Region==region.temp)
    fig <- ggplot(data = data.plot, aes(x=Year))
    #fig <- fig + geom_path(aes(y=pred.link)) + geom_path(aes(y=pred.link+pred.se.link), lty="dashed") + geom_path(aes(y=pred.link-pred.se.link), lty="dashed")
    fig <- fig + geom_path(aes(y=pred)) + geom_path(aes(y=pred+pred.se), lty="dashed") + geom_path(aes(y=pred-pred.se), lty="dashed")
    fig <- fig + ylab("Trend")
    fig <- fig + ggtitle(region.temp)
    fig <- fig + facet_wrap(~Colony, scales = "free_y")
    fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 3), labels=seq(1985, 2017, 3)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
    fig
    
    png(filename = str_c("fig.",region.temp, ".gam.colonies.png"), units="in", width=6.5, height=fig.height,  res=200);print(fig); dev.off()
  }
  
  ##plot trends by region
  data.plot.region<-subset(regional.pred, Region==region.temp & Year >= min.year$min.year[j] & Year <= max(data.plot$Year))
  range<-c(min(data.plot.region$total, na.rm=T), max(data.plot.region$total, na.rm=T))
  range.pred<-round(c(min(data.plot.region$pred.regional, na.rm=T), max(data.plot.region$pred.regional, na.rm=T)),0)
  range.pred<-round(c(min(data.plot.region$r.pred.mean-data.plot.region$r.pred.sd, na.rm=T), max(data.plot.region$r.pred.mean+data.plot.region$r.pred.sd, na.rm=T)),1) ##switch to mean from replicated
  
  fig <- ggplot(data = data.plot.region, aes(x=Year))
  fig <- fig + geom_path(aes(y=r.pred.mean))
  fig <- fig + geom_path(aes(y=pred.regional), color="blue")
  fig <- fig + geom_path(aes(y=r.pred.mean+r.pred.sd), lty="dashed")
  fig <- fig + geom_path(aes(y=r.pred.mean-r.pred.sd), lty="dashed")
  fig <- fig + ylab("Regional count trend")
  fig <- fig + geom_point(aes(y=normalize(x = total, range=range.pred, method="range")))
  fig <- fig + scale_y_continuous(breaks=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), sec.axis = sec_axis(~ ., name = "Total regional count", breaks = seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels = round(seq(range[1], range[2], (range[2]-range[1])/10), 0)))
  #fig <- fig + scale_y_continuous(breaks= seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10))
  fig <- fig + ggtitle(region.temp)
  fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 2), labels=seq(1985, 2017, 2)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
  fig
  
  png(filename = str_c("fig.",region.temp, ".error.gam.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()
  
  #if (region.temp=="North Bay") {
  #  range.pred<-round(c(min(data.plot.region$pred.regional), max(data.plot.region$pred.regional)),1)
  #  fig <- ggplot(data = data.plot.region, aes(x=Year))
  #  fig <- fig + geom_path(aes(y=pred.regional))
  #  fig <- fig + geom_point(aes(y=normalize(x = total, range=range.pred, method="range")))
  #  fig <- fig + ylab("Regional trend")
  #  fig <- fig + scale_y_continuous(breaks=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), sec.axis = sec_axis(~ ., name = "Total regional count", breaks = seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels = round(seq(range[1], range[2], (range[2]-range[1])/10), 0)))
  #  fig <- fig + ggtitle(region.temp)
  #  fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 2), labels=seq(1985, 2017, 2)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
  #  fig
    
  #  png(filename = str_c("fig.",region.temp, ".gam.NoError.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()
  #}
}

##PLOT REGIONAL TRENDS
##subset by min.year & ##scale the regional trend values so can match up magnitudes
dat.plot<-dim(0)
for (j in 1:length(unique(regional.pred$Region))) {
  dat.temp<-subset(regional.pred, Region==unique(regional.pred$Region)[j] & Year >= min.year$min.year[j])
  #dat.temp$trend.norm<-normalize(dat.temp$pred.regional, range=c(0,100), method="range")
  dat.plot<-rbind(dat.plot, dat.temp)
}

region_names <- c(
  `North Bay` = "North Bay",
  `South Bay` = "South Bay",
  `South Farallon Islands` = "South Farallon Islands",
  `Bridges` = "Bridges",
  `Outer Coast` = "Outer Coast",
  `San Francisco Bay` = "San Francisco \n Bay Area Total"
)

#dat.plot<-subset(regional.pred.loess, Region=="Outer Coast")

##plot as facets
fig <- ggplot(dat.plot, aes(x=Year, y=pred.regional))
fig <- fig + geom_path(size=1.1)
fig <- fig + geom_path(aes(y=r.pred.mean+r.pred.sd), lty="dashed") + geom_path(aes(y=r.pred.mean-r.pred.sd), lty="dashed") ##add error
fig <- fig + facet_wrap(~Region, scale="free_y")
fig <- fig + theme_classic() 
fig <- fig + ylab(label = "Count Trend")
fig <- fig + scale_x_continuous(breaks=seq(1980, 2017, 5), limits = c(1982,2017))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1))
#fig <- fig + scale_y_continuous(breaks= round(seq(min(dat.plot$pred.regional),max(dat.plot$pred.regional),max(dat.plot$pred.regional)/10),0))
fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

png(filename = str_c("fig.regional.trends.facet.png"), units="in", width=6.5, height=5,  res=200);print(fig); dev.off()

##plot with free scales
fig <- ggplot(dat.plot, aes(x=Year, y=pred.regional.link))
fig <- fig + geom_path(size=1.1)
fig <- fig + geom_path(aes(y=r.pred.mean.link+r.pred.sd.link), lty="dashed") + geom_path(aes(y=r.pred.mean.link-r.pred.sd.link), lty="dashed") ##add error
fig <- fig + facet_wrap(~Region, scale="free", labeller = as_labeller(region_names))
fig <- fig + theme_classic() 
fig <- fig + ylab(label = "Trend")
fig <- fig + scale_x_continuous(breaks=seq(1980, 2017, 5), limits = c(1982,2017))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black", face = "bold"), axis.text.y = element_text(color="black", face="bold"))
fig <- fig + theme(axis.title = element_text(color="black", face="bold"), strip.text = element_text(face="bold"))
#fig <- fig + scale_y_continuous(breaks= round(seq(min(dat.plot$pred.regional),max(dat.plot$pred.regional),max(dat.plot$pred.regional)/10),0))
fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = x[1],to = x[2],by = (x[2]-x[1])/10),2))
fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

png(filename = str_c("fig8.regional.trends.facet.png"), units="in", width=6.5, height=5,  res=200);print(fig); dev.off()

##plot with overlap
#fig <- ggplot(dat.plot, aes(x=Year, y=pred.regional, color=Region))
#fig <- fig + geom_path(size=1.1)
#fig <- fig + theme_bw() 
#fig <- fig + ylab(label = "Trend")
#fig <- fig + scale_x_continuous(breaks=seq(1980, 2017, 3))
#fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1))
#fig

#png(filename = str_c("fig.regional.trends.overlap.png"), units="in", width=6.5, height=6.5,  res=200);print(fig); dev.off()

##PLOT REGIONAL COUNTS
#fig <- ggplot(regional.counts, aes(x=Year, y=total))
#fig <- ggplot(regional.pred, aes(x=Year, y=total))
fig <- ggplot(subset(counts.m8, is.na(Count)==F) %>% group_by(Region, Year) %>% summarise(total=sum(Count)) %>% data.frame(), aes(x=Year, y=total))
fig <- fig + geom_point(size=1.1)
fig <- fig + facet_wrap(~Region, scale="free", labeller = as_labeller(region_names))
fig <- fig + theme_classic() 
fig <- fig + ylab(label = "Total Regional Nest Count")
fig <- fig + scale_x_continuous(breaks=seq(1980, 2017, 5), limits = c(1982,2017))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black", face="bold"), axis.text.y = element_text(color="black", face="bold"))
fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = 0,to = x[2],by = (x[2]-0)/10),0))
fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig <- fig + theme(axis.title = element_text(color="black", face="bold"), strip.text = element_text(face="bold"))
fig

png(filename = str_c("fig7.regional.counts.facet.png"), units="in", width=6.5, height=5,  res=200);print(fig); dev.off()

##PLOT EFFECTS OF COVARIATES
##plot effect of day
##simulate new data
day<-seq(min(counts$day, na.rm=T), max(counts$day, na.rm=T), 1)
day.sim<-data.frame(Year= rep(median(counts$Year), length(day)), Colony=rep("Steinburger Slough", length(day)), Region=rep("South Bay", length(day)), Survey.type=rep("Ground", length(day)), day=day)
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
fig <- fig + theme_classic()
fig <- fig + scale_x_continuous(breaks=seq(0,300, 10))
fig <- fig + scale_y_continuous(breaks=seq(0,6,0.25))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black", face="bold"), axis.text.y = element_text(color="black", face="bold"))
fig <- fig + theme(axis.title = element_text(color="black", face="bold"))
fig

png(filename = str_c("fig6.day.effect.png"), units="in", width=4, height=3.5,  res=200);print(fig); dev.off()

##effect of count date on counts
#plot(Count~day, data=counts); abline(a= coefficients(lm(Count~day, data=counts))[1], b= coefficients(lm(Count~day, data=counts))[2])

##effect of count date on counts by colony
#fig <- ggplot(data = subset(counts, is.na(day)==F), aes(x=day, y= Count))
#fig <- fig + geom_point(aes())
#fig <- fig + geom_smooth(method="lm")
#fig <- fig + facet_wrap(~Colony, scales="free")
#fig

##effect of survey type on predicted trend value
type<-as.character(unique(subset(counts, is.na(Survey.type)==F)$Survey.type))
type.sim<-data.frame(Year= rep(median(counts$Year), length(type)), Colony=rep("Steinburger Slough", length(type)), Region=rep("South Bay", length(type)), Survey.type=type, day=rep(round(mean(day, na.rm = T), 0), length(type)))
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
fig <- fig + theme_classic()
fig <- fig #+ scale_y_continuous(breaks = seq(1,10, 0.25))
fig <- fig + theme(text = element_text(size=14))
fig

png(filename = str_c("fig.type.effect.png"), units="in", width=4, height=3.5,  res=200);print(fig); dev.off()

##plot effect of survey type
data.temp<-subset(counts.raw, select=c(Colony, Year, Count, Survey.type), subset= Colony=="South Farallon Islands")
data.temp<- data.temp %>% spread(key = Survey.type, value = Count) %>% data.frame()

data.temp<-subset(data.temp, select=c(Aerial, Ground))

plot(Ground~Aerial, data=data.temp); abline(a = 0, b=1, lty="dashed"); abline(a= coefficients(lm(Ground~Aerial, data=data.temp))[1], b= coefficients(lm(Ground~Aerial, data=data.temp))[2])

slope<-round(coefficients(lm(Ground~Aerial, data=data.temp))[2], 1)
intercept<-round(coefficients(lm(Ground~Aerial, data=data.temp))[1], 1)
r2<-round(summary(lm(Ground~Aerial, data=data.temp))$r.squared,2)
equation<-str_c(" y = ", slope, "x + ", intercept, "\n ", "R2", " = ", r2)

fig <- ggplot(data=data.temp, aes(x=Aerial, y=Ground))
fig <- fig + geom_point(size=2)
fig <- fig + geom_smooth(method = "lm", se = F, color="black")
fig <- fig + geom_abline(slope = 1, intercept = 0, lty="dashed")
fig <- fig + theme_classic() + xlab("Aerial count") + ylab("Ground count")
fig <- fig + geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=equation))
fig <- fig + scale_x_continuous(breaks = function(x) round(seq(from = x[1], to = x[2], by = (x[2]-x[1])/10),0))
fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = x[1], to = x[2], by = (x[2]-x[1])/10),0))
fig <- fig + theme(text = element_text(size=12), axis.text = element_text(color="black", face="bold"))
fig <- fig + theme(plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"))
fig <- fig + theme(axis.title = element_text(color="black", face="bold"))
fig
SFI.methods.fig<-fig

png(filename = str_c("fig9.SFI.methods.png"), units="in", width=5, height=4,  res=200);print(fig); dev.off()

##GET REGIONAL TREND FOR ALL REGIONS
#counts.sf<-subset(counts.m8, select=c(Colony, Year, Region, pred, pred.se, Count)) ##decide if want to include bridges or not at this step

###ADD WEIGHTS
#out<-dim(0)
#for (j in 1:nrow(counts.sf)) {
#  col.mean<-mean(subset(counts.sf, Colony==counts.sf$Colony[j])$Count, na.rm = T)
#  region.mean<-counts.sf %>% group_by(Colony) %>% summarise(col.mean = mean(Count, na.rm=T)) %>% data.frame() %>% summarise(region.mean=sum(col.mean)) %>% as.numeric() ##sum of mean colony sizes
#  weight<-col.mean/region.mean
  
#  year.temp<-counts.sf$Year[j]
#  error.year<-subset(counts.sf, Year==year.temp) %>% summarise(error.year=sum(pred.se)) %>% as.numeric()
#  n.temp<-length(unique(subset(counts.sf, Year==year.temp)$Colony))
#  error.weight<-(1-counts.sf$pred.se[j]/error.year)/(n.temp-1)
  
#  out<-rbind(out, c(weight, error.weight))
#}

#counts.sf$weight<-out[,1] ##mean colony size as a fraction of mean regional size
#counts.sf$error.weight<-out[,2]

##check that weights sum to one
#counts.sf %>% group_by(Year) %>% summarise(sum(error.weight))

###GET SF TREND
#sf.pred<-counts.sf %>% group_by(Year) %>% summarise(total=sum(Count, na.rm=T), pred.regional=sum(pred*weight*error.weight)) %>% data.frame()
#sf.pred$Region<-"San Francisco Bay"
#sf.pred<-subset(sf.pred, select=c(Year, Region, total, pred.regional))

###CALC SF TREND WITH ERROR
#edf.colony<-subset(model.plot$edf, str_detect(names(model.plot$edf), pattern="Colony"))
#names(edf.colony)<-str_sub(names(edf.colony), 15, -3)
#edf.df<-data.frame(Colony=names(edf.colony), edf=edf.colony)
#edf.sum<-edf.df %>% group_by(Colony) %>% summarise(edf.sum=sum(edf)) %>% data.frame()

#counts.sf<-dplyr::left_join(counts.sf, y=edf.sum, by = c("Colony","Colony"))
#counts.sf$pred.sd<-counts.sf$pred.se*sqrt(counts.sf$edf.sum+1)
#rep<-10000
#pred.rep<-apply(X = subset(counts.sf, select=c(pred, pred.sd)), MARGIN = 1, FUN = function(x,y,z,n) rnorm(n = n, mean=x[y], sd=x[z]), n = rep, y=1, z=2) %>% data.frame() %>% t() 

#sf.pred.rep<-sf.pred
#for (j in 1:ncol(pred.rep)) {
#  counts.temp<-counts.sf
#  counts.temp$pred<-pred.rep[,j] ##replace prediction with replicate prediction
#  sf.pred.temp<-counts.temp %>% group_by(Year) %>% summarise(pred.sf=sum(pred*weight*error.weight)) %>% data.frame()
#  sf.pred.rep<-cbind(sf.pred.rep, r.pred.rep=sf.pred.temp$pred.sf)
#}

##get mean regional prediction and CI
#sf.pred.mean<-subset(sf.pred.rep, select=str_detect(names(sf.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = mean) %>% as.numeric() ##take only replicates
#sf.pred.sd<-subset(sf.pred.rep, select=str_detect(names(sf.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = sd) %>% as.numeric()
#sf.pred.lower<-sf.pred.mean - 1.96*sf.pred.sd/sqrt(rep)
#sf.pred.upper<-sf.pred.mean + 1.96*sf.pred.sd/sqrt(rep)

##instead, order the values and use the actual values for the CI
#sf.pred.med<-subset(sf.pred.rep, select=str_detect(names(sf.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = median) %>% as.numeric()
#sf.pred.ord<-subset(sf.pred.rep, select=str_detect(names(sf.pred.rep), pattern="r.pred.rep")) %>% apply(MARGIN = 1, FUN = sort) 
#sf.pred.lower<-sf.pred.ord[round(0.025*(ncol(sf.pred.rep)-3),0),] %>% as.numeric()
#sf.pred.upper<-sf.pred.ord[round(0.975*(ncol(sf.pred.rep)-3),0),] %>% as.numeric()

#sf.pred$sf.pred.mean<-sf.pred.mean
#sf.pred$sf.pred.sd<-sf.pred.sd
#sf.pred$sf.pred.med<-sf.pred.med
#sf.pred$sf.ci.lower<-sf.pred.lower
#sf.pred$sf.ci.upper<-sf.pred.upper
#sf.pred$q1<-as.numeric(apply(sf.pred.ord, FUN=summary, MARGIN = 2)[2,])
#sf.pred$q3<-as.numeric(apply(sf.pred.ord, FUN=summary, MARGIN = 2)[5,])

##replace missing years of counts with NA
#for (j in 1:nrow(sf.pred)) {
#  dat.temp<-subset(counts.sf, Year==sf.pred$Year[j] & is.na(Count)==F)
#  if (nrow(dat.temp)==0) {
#    sf.pred$total[j]<-NA
#  }
#}

##PLOT SF TREND
#range.pred<-round(c(min(sf.pred$pred.sf), max(sf.pred$pred.sf)),1)
#range<-c(min(sf.pred$total), max(sf.pred$total))
#fig <- ggplot(data = subset(sf.pred, Year>=1990), aes(x=Year))
#fig <- fig + geom_path(aes(y = pred.sf))
#fig <- fig + geom_point(aes(y=normalize(total, range=range.pred, method="range")))
#fig <- fig + geom_path(aes(y = sf.pred.med), size = 1.1)
#fig <- fig + geom_path(aes(y = sf.ci.lower), lty="dashed") + geom_path(aes(y = sf.ci.upper), lty="dashed")
#fig <- fig + ylab("Trend")
#fig <- fig + theme_classic()
#fig <- fig + scale_y_continuous(breaks=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), sec.axis = sec_axis(~ ., name = "Total regional count", breaks = seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels = round(seq(range[1], range[2], (range[2]-range[1])/10), 0)))
#fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 2), labels=seq(1985, 2017, 2)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
#fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = x[1], to = x[2], by = (x[2]-x[1])/10),2))
#fig

#png(filename = str_c("fig.SF.trend.png"), units="in", width=4, height=3.5,  res=200);print(fig); dev.off()

##add to percent change table
#change.sf<-data.frame(Region=rep("San Francisco Bay", 3), Years=NA, start=c(1990, 2003, 1990), end=c(2003, 2017, 2017), percent.change=NA, percent.change.rep=NA, lower95=NA, upper95=NA, q1=NA, q3=NA, growth=NA, growth.rep=NA, growth.lower95=NA, growth.upper95=NA)
#change.sf$Years<-str_c(change.sf$start, "-", change.sf$end)

#for (j in 1:nrow(change.sf)) {
#  initial.rep<-subset(sf.pred.rep, Year==change.sf$start[j], select= str_detect(string = names(sf.pred.rep), pattern = "rep")) %>% as.numeric()
#  final.rep<-subset(sf.pred.rep, Year==change.sf$end[j], select= str_detect(string = names(sf.pred.rep), pattern = "rep")) %>% as.numeric()
  
  ##use percent change function
#  percent.change.rep<-per.change.func(initial.rep, final.rep)
  
#  med.change<-round(median(percent.change.rep, na.rm = T),2)
#  change.sf$percent.change.rep[j]<-med.change
  
  ##instead, order the values and use the actual values for the CI
#  rep.ord<-percent.change.rep[order(percent.change.rep)]
#  change.sf$lower95[j]<-rep.ord[round(0.025*length(percent.change.rep),0)]
#  change.sf$upper95[j]<-rep.ord[round(0.975*length(percent.change.rep),0)]
  
#  change.sf$q1[j]<-as.numeric(summary(rep.ord)[2])
#  change.sf$q3[j]<-as.numeric(summary(rep.ord)[5])
  
#  change.sf$percent.change[j]<-per.change.func(subset(sf.pred.rep, Year==change.sf$start[j])$pred.regional, subset(sf.pred.rep, Year==change.sf$end[j])$pred.regional)
  
  #hist(percent.change.rep); abline(v=change.sf$percent.change.rep[j]); abline(v=change.sf$lower95[j], lty="dashed"); abline(v=change.sf$upper95[j], lty="dashed")
  
  ##calc GROWTH RATE
#  change.sf$growth[j]<-growth.func(x = subset(sf.pred.rep, Year==change.sf$start[j])$pred.regional, y = subset(sf.pred.rep, Year==change.sf$end[j])$pred.regional, year1 = change.sf$start[j], year2 = change.sf$end[j])
  ##get it for the reps
#  growth.rep<-growth.func(x=initial.rep, y=final.rep, year1 = change.sf$start[j], year2 = change.sf$end[j])
  ##med and CI for growth
#  change.sf$growth.rep[j]<-round(median(growth.rep, na.rm = T),2) ##median growth rate
#  rep.growth.ord<-growth.rep[order(growth.rep)] ##order the reps
#  change.sf$growth.lower95[j]<-rep.growth.ord[round(0.025*length(rep.growth.ord),0)]
#  change.sf$growth.upper95[j]<-rep.growth.ord[round(0.975*length(rep.growth.ord),0)]
  
#}

#change.tab<-rbind(change.dat, change.sf)
change.tab<-change.dat
change.tab$"Percent change"<-str_c(round(change.tab$percent.change.rep,0), "% (", round(change.tab$lower95,0), ", ", round(change.tab$upper95,0), ")")
#change.tab<-subset(change.tab, select=c(Region, Years, `Percent change`))
##use growth instead
change.tab$"Growth rate"<-str_c(round(change.tab$growth.rep,1), " (", round(change.tab$growth.lower95,1), ", ", round(change.tab$growth.upper95,1), ")")
change.tab<-subset(change.tab, select=c(Region, Years, `Growth rate`, `Percent change`))

##calculate % smaller
#type.model<-lm(Ground~Aerial, data=data.temp)
#type.fun<-function(x) {coefficients(type.model)[2]*x+ coefficients(type.model)[1]}

#(200-type.fun(200))/(200+type.fun(200))*100

#source("DCCO_poptrend_code_07Mar2018.R")

##TABLE OF PERCENT ANNUAL CHANGE
#per.change.func<-function(x,y) {ifelse(y>x & ((y-x)/x)<0,-round((y-x)/x*100,2), ifelse(y<x & ((y-x)/x)>0,-round((y-x)/x*100,2),round((y-x)/x*100,2)))}

##combine predictions for regions and sf bay
#regional.sf.pred.rep<-rbind(regional.pred.rep, sf.pred.rep)
regional.sf.pred.rep<-regional.pred.rep

##order by region and year
regional.sf.pred.rep<-regional.sf.pred.rep[order(regional.sf.pred.rep$Region, regional.sf.pred.rep$Year),]

##calc percent annual change for one column
#change.annual<-per.change.func(lag(regional.sf.pred.rep$pred.regional,1), regional.sf.pred.rep$pred.regional)

##calc percent annual change for all columns
reps<-subset(regional.sf.pred.rep, select=str_detect(names(regional.sf.pred.rep), "pred.rep"))
change.annual.rep<-apply(X = reps, MARGIN = 2, FUN = function(x) {per.change.func(lag(x,1), x)})

##replace values with NA for first year for each region
change.annual.rep[which(regional.sf.pred.rep$Year==1984),]<-NA

##calc mean annual change
mean.annual.change<-round(as.numeric(apply(X = change.annual.rep, MARGIN = 1, FUN = mean)),2)
u95<-round(as.numeric(apply(X = change.annual.rep, MARGIN = 1, FUN = function(x){sort(x)[length(x)*0.975]})),2)
l95<-round(as.numeric(apply(X = change.annual.rep, MARGIN = 1, FUN = function(x){sort(x)[length(x)*0.025]})),2)

##combine labels with percent annual change values
change.annual<-cbind(subset(regional.sf.pred.rep, select=c(Year, Region)), per.change=mean.annual.change, u95=u95, l95=l95)
change.annual.rep<-cbind(change.annual, change.annual.rep)

##create appendix table of annual percent change
change.annual$`Percent annual change`<-str_c(round(change.annual$per.change,0), "% (", round(change.annual$l95,0), ", ", round(change.annual$u95,0), ")")
change.appendix<-subset(change.annual, select=c(Year, Region, `Percent annual change`), subset= per.change!="NA")
##replace years before data were available for a given region
#min.year.sf<-c(min.year, 1990)
for (j in 1:length(unique(change.appendix$Region))) {
  change.appendix<-subset(change.appendix, !(Region==unique(change.appendix$Region)[j] & Year<min.year$min.year[j]))
}
change.appendix<-spread(data = change.appendix, key = Region, value = `Percent annual change`, fill = "-")

##create change table from mean annual values
change.tab.annual<-data.frame(subset(change.dat, select=c(Region, Years, start, end)), per.change=NA, l95=NA, u95=NA, q1=NA, q3=NA)

for (j in 1:nrow(change.tab.annual)) {
  region.temp<-change.tab.annual$Region[j]
  start.temp<-change.tab.annual$start[j]
  end.temp<-change.tab.annual$end[j]
  
  dat.temp<-subset(change.annual.rep, Region==as.character(region.temp) & Year>=start.temp & Year <= end.temp, select=str_detect(names(change.annual.rep), "rep"))
  mean.rep.temp<-apply(dat.temp, MARGIN = 2, FUN = mean, na.rm=T)
  change.tab.annual$per.change[j]<-round(median(mean.rep.temp),0)
  change.tab.annual$u95[j]<-round(sort(mean.rep.temp)[length(mean.rep.temp)*0.975],0)
  change.tab.annual$l95[j]<-round(sort(mean.rep.temp)[length(mean.rep.temp)*0.025],0)
  change.tab.annual$q1[j]<-round(as.numeric(summary(mean.rep.temp)[2]),0)
  change.tab.annual$q3[j]<-round(as.numeric(summary(mean.rep.temp)[5]),0)
}

change.tab.annual$`Mean annual change`<-str_c(change.tab.annual$per.change, "% (", change.tab.annual$q1, ", ", change.tab.annual$q3, ")")
change.tab.annual<-subset(change.tab.annual, select=-c(start, end, per.change, l95, u95, q1, q3))

change.tab.combo<-cbind(subset(change.tab, select=-`Percent change`), subset(change.tab.annual, select=`Mean annual change`))
change.tab.combo

##make table with years as column headers
change.per<-change.tab
change.per$Years<-as.character(change.per$Years)
change.per$Years[which(str_detect(change.per$Years, "-2002"))]<-"pre-2003"
change.per$Years[which(str_detect(change.per$Years, "19..-2017"))]<-"All years"
change.per<-subset(change.per, select=-`Growth rate`) %>% spread(key = Years, value = `Percent change`)
change.per<-change.per[,c(1,4,2,3)]
change.per

##MODEL REGIONAL COUNTS
for (j in 1:length(unique(regional.counts$Region))) {
  region.temp<-unique(regional.counts$Region)[j]
  dat.temp<-subset(regional.counts, Region==region.temp)
  
  M.temp<-gam(total ~ s(Year),
          data = dat.temp,
          family = poisson)
  
  predictions<-predict.gam(object = M.temp, newdata = dat.temp, type = "response", se.fit = T)
  dat.temp$pred<-predictions$fit
  dat.temp$pred.se<-predictions$se.fit
  
  
}

fig <- ggplot(data = dat.temp, aes(x=Year))
fig <- fig + geom_path(aes(y=pred))
fig <- fig + geom_path(aes(y=pred+pred.se), lty="dashed")
fig <- fig + geom_path(aes(y=pred-pred.se), lty="dashed")
fig <- fig + ylab("Regional count trend")
fig <- fig + geom_point(aes(y=total))
#fig <- fig + scale_y_continuous(breaks=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels=seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), sec.axis = sec_axis(~ ., name = "Total regional count", breaks = seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10), labels = round(seq(range[1], range[2], (range[2]-range[1])/10), 0)))
#fig <- fig + scale_y_continuous(breaks= seq(range.pred[1], range.pred[2], (range.pred[2]-range.pred[1])/10))
fig <- fig + ggtitle(region.temp)
fig <- fig + scale_x_continuous(breaks = seq(1985, 2017, 2), labels=seq(1985, 2017, 2)) + theme(axis.text.x = element_text(angle = 45, hjust=1))
#fig <- fig + scale_y_continuous(trans="log")
fig

#png(filename = "fig.temp.png", units="in", width=6*1.5, height=4*1.5,  res=200);fig; dev.off()

change.tab
