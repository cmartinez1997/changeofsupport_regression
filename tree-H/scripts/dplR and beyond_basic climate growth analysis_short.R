require(dplR)
require(lattice)
require(zoo)
require(pspline)
library(lattice)
library(latticeExtra)

##### required functions to run the script #####

####fy#####
fy <- function(object) {
  
  fyos<- function(x) {
    L <- !is.na(x)
    idx <- c(NA, which(L))[cumsum(L) + 1]
    fy <- min(idx,na.rm=TRUE)
    return(fy)
  }
  
  ifelse (is.mts(object),
          rel.years <- apply(object,2,fyos),
          rel.years <- fyos(object))
  
  calyears <- rel.years+start(object)[1]-1
  return(calyears)
}

####ly#####

ly <- function(object) {
  
  fyos<- function(x) {
    L <- !is.na(x)
    idx <- c(NA, which(L))[cumsum(L) + 1]
    fy <- max(idx,na.rm=TRUE)
  }
  
  ifelse (is.mts(object),
          rel.years <- apply(object,2,fyos),
          rel.years <- fyos(object))
  
  calyears <- rel.years+start(object)[1]-1
  return(calyears)
}

######spline.det#####
spline.det <- function(x,smoothing) {
  
  spline.p.for.smooth.Pspline<-function(year)  {
    p <- .5/((6*(cos(2*pi/year)-1)^2/(cos(2*pi/year)+2)))
    return(p)
  }
  
  
  ifelse ((is.null(ncol(x))),x2 <- ts.union(x,x), x2 <- x)
  
  fyarray <- start(x2)[1]
  lyarray <- end(x2)[1]
  begin <- fy(x2)
  end <- ly(x2)
  
  smoothedarray <- x2
  
  spline.p <- spline.p.for.smooth.Pspline(smoothing)
  
  
  for (i in 1:ncol(x2)) {
    temp <- smooth.Pspline((begin[i]:end[i]),x2[(begin[i]-fyarray+1):(end[i]-fyarray+1),i],spar=spline.p,method=1)
    smoothedarray[(begin[i]-fyarray+1):(end[i]-fyarray+1),i]  <- temp$ysmth
  }
  
  if (is.null(ncol(x))) {smoothedarray <- smoothedarray[,-1]}
  
  residualarray <- x-smoothedarray
  ratioarray <- x/smoothedarray
  
  output <- list(x=x,smoothedarray=smoothedarray,residualarray=residualarray,ratioarray=ratioarray)
  return(output)
}


####runningclimate aggregates monthly data up to 12 months####

runningclimate_new<-function(climate,stat=c("mean","sum"),length=12){
  require(zoo)
  x.ts<-ts(climate[,2:13],start=climate[1,1]) #transform CRU style climate data into time-series object
  x.lag<-stats::lag(x.ts,k=-1) #create lag1 series
  x1<-ts.union(x.lag,x.ts) #combine lagged and original series
  length<-12  #default to 12 months
  stat<-stat #no default is given
  seasons<-rep(list(x1),length)
  for(i in 2:length){
    seasons[[i]]<-ts(cbind(matrix(rep(rep(NA,nrow(x1)),i-1),ncol=i-1),t(rollapply(t(seasons[[i]]),i,stat,align='right'))),start=climate[1,1])
    seasons[[i]]<-ts.union(stats::lag(seasons[[i]][,13:24],k=-1),seasons[[i]][,13:24])
    colnames(seasons[[i]])<-c(paste0("p",month.abb),month.abb)
  }
  
  output<-seasons
}
####Climate PLot with correlations#####
#the detreding is for climate data only
#TRW=your RWI time series
#Climatesite= output from runningclimate_new
#fyr/lyr= first/last year of analysis
#detrended=should climate data be detrended with a spline, yes or no? 
#spline.length=same number as you would put in your TRW detrending spline

Climateplot_new<-function(TRW,Climatesite,fyr=tsp((Climatesite[[1]]))[1],lyr=tsp((Climatesite[[1]]))[2],detrended=c("Yes","No"),method=c("pearson","spearman"),spline.length){   ####Climatesite=your climate data run through runningclimate_new(),TRW=Tree ring chronology with time series class,fy=first year, ly=last year
  library(MASS)
  library(dyn)
  climatecor<-NULL
  METHOD<-method
  if(detrended=="Yes"){
    tsstart<-tsp((Climatesite[[1]]))[1]
    for (i in 1:length(Climatesite)){
      Climatesite[[i]]<-ts(apply(Climatesite[[i]],2,function(x)if(sum(complete.cases(x))>3){spline.det(ts(x),spline.length)$residualarray}else{rep(NA,length(x))}),start=tsstart)
      climatecor[[i]]<-cor(ts.union(Climatesite[[i]],window(TRW,fyr,lyr)),use="p",method=METHOD)
    }
    levelplotmatrix<-matrix(NA,length(Climatesite),24)
    for (i in 1:length(Climatesite)){
      levelplotmatrix[i,]<-climatecor[[i]][1:24,25]
    }
  }else{
    for (i in 1:length(Climatesite)){
      climatecor[[i]]<-cor(ts.union(Climatesite[[i]],window(TRW,fyr,lyr)),use="p",method=METHOD)
    }
    levelplotmatrix<-matrix(NA,length(Climatesite),24)
    for (i in 1:length(Climatesite)){
      levelplotmatrix[i,]<-climatecor[[i]][1:24,25]
    }
  }
  rownames(levelplotmatrix)<-names(1:length(Climatesite))
  colnames(levelplotmatrix)<-c(paste0("p",month.abb),month.abb[1:12])
  return(levelplotmatrix)
}



###first load your ring-width time series####
rwdata<-read.rwl("/Users/klesse_adobe/Dropbox/wsl/CLIMBEECH/Data/switzerland/fub.tuc") ### otherwise you just need a data.frame for all the following steps.

#detrend the data
rwdata.detrended<-detrend(rwdata,method="Spline",nyrs=30) ###30y-spline is kind of a "standard" flexible high frequency detrending, retaining the decadal variability, just play with the methods and see what changes ...

###rwi.stats: you can do that with the raw or detrended values, sometimes worth a check
rwi.stats.running(rwdata.detrended,window.length=30,window.overlap=15)  ### running eps and rbar statistics on your data.frame, here window of stats calculation is 31 years and overlap=30, arbitrarily chosen, default is 50 / 25 I think

####chronology building without variance stabilization
rwdata.chrono<-chron(rwdata.detrended, biweight=T)[,1] ### that's without variance stabilization #check the help function in dplR
ts.plot(rwdata.chrono)

####chronology building with variance stabilization
rwdata.chrono.stabilized<-chron.stabilized(rwdata.detrended, biweight=T,winLength=31)[,1] ### that's with variance stabilization #check the help function in dplR
ts.plot(rwdata.chrono.stabilized)

###for the correlations with my functions you have to make a time-series object out of rwdata.chrono.stabilized

rwdata.chrono.stabilized.ts<-ts(rwdata.chrono.stabilized,start=as.numeric(rownames(rwdata)[1])) #master chono tree_ref
ts.plot(rwdata.chrono.stabilized.ts)

###so now you have your final chronology, let's move towards climate correlation
###for this I created two nice functions over the last years to explore climate-growth relationships
###first one is to average monthly data to larger seasons up to 12 months
###therefore you need to load the climate dataset
###load however you want but it has to be in the format: 13 columns, first column=year,2-13=months)

gt.tmean<-read.table("/Users/klesse_adobe/Downloads/eobs_tmean.txt")
gt.ppt<-read.table("/Users/klesse_adobe/Downloads/eobs_precipitation.txt")

ts.plot(gt.tmean[,-1]) # plot without year column
ts.plot(gt.ppt[,-1]) # plot without year column



###runningclimate this returns a list of 12, where [[1]] is still the original monthly data, [[2]] two-months averages, 
#and so on! Columns range from pJan to current Dec. The alignment is "right", so your timeseries e.g. [[List3]][,column20] would be JuneJulyAugust of current year
###second is the nice correlation plot with end months in columns and window/season lengths in rows
gt.ppt.dataset<-runningclimate_new(gt.ppt,stat="mean")
gt.tmean.dataset<-runningclimate_new(gt.tmean,stat="mean")


#### Based on this nice graph you can decide which is the most meaningful (or at least the best correlating) response window for your model to use, or whatever you need the correlation for
###I changed the colorscale and some smaller things here

#fyr and lyr = first and last year that climate correlation should be computed
#P2 is the object containing the contours (like elevations on a map)#
#P1 contains the colors
#silly_cor is a matrix with the actual correlation values
#row 1 contains the correlations with monthly climate data, row 12 contains the correlations with annual climate data.

#use detrended="Yes" when you detrended your RW chronology with a spline. 

#TEMPERATURE:
silly_cor<-Climateplot_new(rwdata.chrono.stabilized.ts,gt.tmean.dataset,fyr = 1927,lyr=2024,detrended = "Yes",spline.length = 30,method="spearman")  #you need to set the first and last year fyr and lyr. If you detrend your tree-ring data with a spline, use the same spline also for the climate data
P1<-contourplot(t(silly_cor),region=T,contour=F,scales = list(tck = c(1,0)),main="tmean",par.settings=list(fontsize=list(text=8, points=10)),lwd=0.4,axis.text=list(cex=0.5),labels = list(cex = 0.5),col.regions=colorRampPalette(c("red","yellow","white","lightblue","blue")),at=c(seq(-0.825,0.825,0.05)),xlab="End months",ylab="Season length")
P2<-contourplot(t(silly_cor),col.regions=F,scales = list(tck = c(1,0)),par.settings=list(fontsize=list(text=8, points=10)),axis.text=list(cex=0.5),labels = list(cex = 0.5),cex=0.5,region=F,lwd=0.4,at=c(seq(-0.80,0.80,0.1)))
PP1<-P1+P2
PP1

#PRECIPITATION:
silly_cor<-Climateplot_new(rwdata.chrono.stabilized.ts,gt.ppt.dataset,fyr = 1920,lyr=2023,detrended = "Yes",spline.length = 30,method="spearman")  #you need to set the first and last year fyr and lyr. If you detrend your tree-ring data with a spline, use the same spline also for the climate data
P1<-contourplot(t(silly_cor),region=T,contour=F,scales = list(tck = c(1,0)),main="ppt",par.settings=list(fontsize=list(text=8, points=10)),lwd=0.4,axis.text=list(cex=0.5),labels = list(cex = 0.5),col.regions=colorRampPalette(c("red","yellow","white","lightblue","blue")),at=c(seq(-0.825,0.825,0.05)),xlab="End months",ylab="Season length")
P2<-contourplot(t(silly_cor),col.regions=F,scales = list(tck = c(1,0)),par.settings=list(fontsize=list(text=8, points=10)),axis.text=list(cex=0.5),labels = list(cex = 0.5),cex=0.5,region=F,lwd=0.4,at=c(seq(-0.80,0.80,0.1)))
PP1<-P1+P2
PP1


#####SPEI#####
#do the same correlations with SPEI
library(SPEI)


#as above Tmin, Tmax, Precip are your standard tables with 13 columns (1= year, 2-13= Jan - Dec)

PET2<-NULL
waterbalance<-NULL
YOURLATITUDE<-45
  PET2<-thornthwaite(ts(unlist(c(t(gt.tmean[,-1]))),frequency=12),lat=YOURLATITUDE)  #Tmin and Tmax are your temperature series, #latitude needed
waterbalance<-ts(unlist(c(t(gt.ppt[,-1]))),frequency=12)-PET2   #please check if waterbalance is the same order of magnitude as PET2. If not, there might be a unit issue with the precipitation data (add *10 after ',frequency=12)', if given in cm)

site_spei<-NULL
for(i in 1:12){
  silly<-ts(data.frame(matrix(spei(waterbalance,scale=i)$fitted,ncol=12,byrow=T)),start=1930) #check starting year
  site_spei[[i]]<-ts.union(stats::lag(silly,-1),silly)
}

for(i in 1:12){
  site_spei[[i]][ which(is.infinite(site_spei[[i]]))  ]<-min(site_spei[[i]][ which(!is.infinite(site_spei[[i]]))  ],na.rm=T)
}


#DROUGHT INDEX:
silly_cor<-Climateplot_new(rwdata.chrono.stabilized.ts,site_spei,fyr = 1927,lyr=2024,detrended = "Yes",spline.length = 30,method="spearman")
P1<-contourplot(t(silly_cor),region=T,contour=F,scales = list(tck = c(1,0)),main="SPEI",par.settings=list(fontsize=list(text=8, points=10)),lwd=0.4,axis.text=list(cex=0.5),labels = list(cex = 0.5),col.regions=colorRampPalette(c("red","yellow","white","lightblue","blue")),at=c(seq(-0.825,0.825,0.05)),xlab="End months",ylab="Season length")
P2<-contourplot(t(silly_cor),col.regions=F,scales = list(tck = c(1,0)),par.settings=list(fontsize=list(text=8, points=10)),axis.text=list(cex=0.5),labels = list(cex = 0.5),cex=0.5,region=F,lwd=0.4,at=c(seq(-0.80,0.80,0.1)))
PP1<-P1+P2
PP1


