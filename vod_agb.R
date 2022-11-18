library(raster)
library(rgdal)

setwd("D:\\MS SRM\\Master Thesis\\raster tutorial\\Tutorial_data")

list.files()

STACK.nc<- stack("cVeg_Lmon_UKESM1-0-LL_historical_r1i1p1f2_gn_195001-201412.nc")

STACK.nc


plot(STACK.nc[[1]])

STACK.nc<- rotate(STACK.nc)

plot(STACK.nc[[1]])

#croping region of interest from netcdf files
roi<- extent(-180,180,-23,23)

roi

lines(roi)

extent.roi<-extent(roi)

extent.roi<- extent.roi+c(-1,1,-1,1)

STACK.nc.crop<- crop(STACK.nc,extent.roi)

plot(STACK.nc.crop[[1]])

#original

SHAPES<- readOGR("D:\\MS SRM\\Master Thesis\\raster tutorial\\Tutorial_data\\Shapes\\ne_10m_admin_0_countries.shp")

AOI<- SHAPES[which(SHAPES$ADMIN=="Brazil"),]

lines(AOI)


extent.AOI<- extent(AOI)

extent.AOI<- extent.AOI+c(-1,1,-1,1)

STACK.nc.crop<- crop(STACK.nc,extent.AOI)


plot(STACK.nc.crop[[1]])

lines(AOI)


STACK.nc.disagg<- disaggregate(STACK.nc.crop, res(STACK.nc.crop)*10)

STACK.nc.mask<- mask(STACK.nc.disagg, AOI)

plot(STACK.nc.mask[[1]])

lines(AOI)

MEAN.C<- calc(STACK.nc.mask,mean)

plot(MEAN.C, main="mean vegetation carbon in Brazil 1950-2014")

MEAN.vegC<- cellStats(STACK.nc.mask,mean)

par(mai=c(0.5,0.85,0.5,0.5))

plot(MEAN.vegC,type="l",xlab="",ylab="vegC[kg/m2]",lwd=2,cex.axis=1.5,cex.lab=1.5,xaxt="n")
axis(side=1,at=seq(1,length(MEAN.vegC),120),labels=seq(1950,2014,10),cex.axis=1.5)

STACK.nc.decade<- aggregate(STACK.nc.mask,c(1,1,120))

STACK.nc.decade

plot(STACK.nc.decade[[c(1,6)]],main=c("1950-1960","2000-2010"))

plot(STACK.nc.decade[[6]]-STACK.nc.decade[[1]],main="difference 1950-1960 to 2000-2010")

COLS<- colorRampPalette(c("red","white","blue"))

plot(STACK.nc.decade[[6]]-STACK.nc.decade[[1]],breaks=seq(-9,9,0.5),col=COLS(37),main="difference of 1950-60 to 2000-2010")

MEAN.vod.am.p

library(ncdf4)
nc_carbon <-nc_open("Global_annual_mean_ABC_lc2001_1993_2012_20150331.nc")
nc_carbon
#extra
crop_roi<-crop(STACK.nc,roi)
plot(crop_roi[[1]])




#experimentation 



fc.nc<- stack(MEAN.WACCM.cLand.p+ MEAN.IPSL.cLand.p, var= varLists)

fc.nc

exp1<-cbind(MEAN.WACCM.cLand.p, MEAN.IPSL.cLand.p)
exp1<- data.frame(Time= c("layer.143","layer.144","layer.145","layer.146","layer.147", "layer.148", "layer.149", "layer.150","layer.151", "layer.152", "layer.153", "layer.154", "layer.155", "layer.156", "layer.157", "layer.158","layer.159", "layer.160", "layer.161", "layer.162", "layer.163", "layer.164", "layer.165"), y1=MEAN.WACCM.cLand.p, y2=MEAN.IPSL.cLand.p)

ggplot(exp1,aes(Time,y1,y2)+
         geom_line()
)

exp1

ggplot(exp2)

ggp1 <- ggplot(exp2, aes(Time)) +       # Create ggplot2 plot
  geom_line(aes(y = y1, color = "red")) +
  geom_line(aes(y = y2, color = "blue"))
ggp1                                 # Draw ggplot2 plot

exp2<- rbind(MEAN.WACCM.cveg.p, MEAN.IPSL.cveg.p)
plot(exp2, x= Time, y=y1,y2)
exp2<- data.frame(Time= c("layer.143","layer.144","layer.145","layer.146","layer.147", "layer.148", "layer.149", "layer.150","layer.151", "layer.152", "layer.153", "layer.154", "layer.155", "layer.156", "layer.157", "layer.158","layer.159", "layer.160", "layer.161", "layer.162"), y1=MEAN.WACCM.cveg.p, y2=MEAN.IPSL.cveg.p)
exp2
plot(exp2)

ggp1 <- ggplot(exp2, aes(Time)) +       # Create ggplot2 plot
  geom_line(aes(y = y1, color = "red")) +
  geom_line(aes(y = y2, color = "blue"))
ggp1 

plot()



exp3<-par(mai=c(0.5,0.85,0.5,0.5))
plot(MEAN.IPSL.cLand.p,main="Mean Total Carbon in tropical region",type="l",xlab="",ylab="cLand[kgm-2]",lwd=2,cex.axis=1.5,cex.lab=1.5,xaxt="n")
axis(side=1,at=seq(1,length(MEAN.IPSL.cLand.p),5),labels=seq(1993,2014,5),cex.axis=1.5, )


exp4<-par(mai=c(0.5,0.85,0.5,0.5))
plot(MEAN.WACCM.cLand.p,main="Mean Total Carbon in tropical region",type="l",xlab="",ylab="cLand[kgm-2]",lwd=2,cex.axis=1.5,cex.lab=1.5,xaxt="n")
axis(side=1,at=seq(1,length(MEAN.WACCM.cLand.p),5),labels=seq(1993,2014,5),cex.axis=1.5, )

install.packages("ggpubr")  
library(ggpubr)

figure <- ggarrange( (par(mai=c(0.5,0.85,0.5,0.5),
                          plot(MEAN.IPSL.cLand.p,main="Mean Total Carbon in tropical region",type="l",xlab="",ylab="cLand[kgm-2]",lwd=2,cex.axis=1.5,cex.lab=1.5,xaxt="n"),
                          axis(side=1,at=seq(1,length(MEAN.IPSL.cLand.p),5),labels=seq(1993,2014,5),cex.axis=1.5, )),
                      (par(mai=c(0.5,0.85,0.5,0.5)),
                       plot(MEAN.WACCM.cLand.p,main="Mean Total Carbon in tropical region",type="l",xlab="",ylab="cLand[kgm-2]",lwd=2,cex.axis=1.5,cex.lab=1.5,xaxt="n"),
                       axis(side=1,at=seq(1,length(MEAN.WACCM.cLand.p),5),labels=seq(1993,2014,5),cex.axis=1.5, ),
                       labels = c("A", "B"),
                       ncol = 1, nrow = 1),
                      
                      
                      
                      
                      
                      
                      warnings()
                      
                      rlang::last_error()
                      
                      MEAN.WACCM.cLand.p
                      
                      ggplot()+
                        geom_line(data = exp1, aes(Time, y1), color= 'blue') +
                        geom_point(data = exp1, aes(Time, y2), color = 'red')
                      