# Copyright (c) 2019 by Xiao Lei. 
#Permission to copy all or part of this work is granted, provided
# that the copies are not made or distributed for resale, and that
# the copyright notice and this notice are retained.
######################################
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)

#enter contry list
country=c('China','Australia','Nepal','USA','Spain','France','Italy','Switzerland','Netherlands',
          'Germany','Russia','Iceland','Bermuda','Cambodia','Thailand','Laos','Mexico','Singapore','For Future')

#small countries to emphasize
emphasis=c('Bermuda','Singapore')


#define colors for each
candidatecolor=c(brewer.pal(9, 'Set1'),brewer.pal(8, 'Set2'),brewer.pal(12, 'Set3'))
usedcolor=character()
usedcolor[1]=candidatecolor[1];
usedcolor[4]=candidatecolor[14]
usedcolor[11]=candidatecolor[15]
usedcolor[17]=candidatecolor[6]
usedcolor[18]=candidatecolor[7]
candidatecolor=candidatecolor[-c(1,5,14,15,7)]
usedcolor[length(country)]='midnightblue'
for (i in 1:length(country))(
  if (is.na(usedcolor[i]))
    usedcolor[i]=candidatecolor[i]
)

#plot the map
countrylabel=sort(country)
colorlabel=character()
for (i in 1:length(country)){
  colorlabel[i]=usedcolor[which(country==countrylabel[i])]
}
data<- map_data("world")
data[which(data['region']=='Taiwan'),5]='China'
data['color']=rep('For Future',nrow(data))
for (i in 1:(length(country)-1)){
  data[which(data['region']==country[i]),7]=country[i]
}
p=ggplot() + geom_polygon(data = data, aes(x=long, y = lat, group = group,fill=color)) + coord_fixed(1.3)+
  theme(panel.background = element_rect(fill = "lightskyblue1"))
p=p+scale_fill_manual(name='Countries Ordered by First Passage Time',values=colorlabel,breaks=country)
p=p+ guides(fill = guide_legend(title = "Countries Ordered by First Passage Time", title.position = "top"))
p=p+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = 'bottom',legend.text=element_text(size=12),legend.title =element_text(size=12) )
lat=numeric()
long=numeric()
pcolor=character()
for (i in 1:2){
  ind=which(data[,5]==emphasis[i])[1]
  lat=c(lat,data[ind,1])
  long=c(long,data[ind,2])
  pcolor=c(pcolor,usedcolor[which(country==emphasis[i])])
}
p=p+geom_point(aes(x=lat, y=long),size=3,color=pcolor,shape=18)
p
