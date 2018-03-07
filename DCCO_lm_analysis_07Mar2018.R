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