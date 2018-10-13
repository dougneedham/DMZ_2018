library("rgl")
library(readr)
df <- read_csv('Example1.csv')
first.element <- function(my.string) { unlist(strsplit(my.string,"_"))[1]}
x <- as.integer(sapply(names(df)[2:length(df)],first.element))
y<- df$Increment
second.element <- function(my.string) { unlist(strsplit(my.string,"X"))[2]}
dfi <- expand.grid(x,y)
df <- df[,2:ncol(df)]
dfi$value <- 0		
columns <- ncol(df)

for(rownumber in 1:nrow(df)) {
  index2 <-1
  for(index in 1:(columns*rownumber)) {
    if(as.integer(row.names(dfi)[index]) <= (columns*rownumber)) { 
      #print(index)
      if(dfi[index,]$value == 0) {
        #print(as.integer(df[rownumber,index2]))
        print(index2)
        dfi[index,]$value<- as.integer(df[rownumber,index2])
        #print(index2)
        #print(cols)
        if(index2 >columns) { index2 <- 1}
        index2 <- index2 + 1
      }				
    }
    #print(index2)
  }
}
dfi$value2 <- dfi$value-min(dfi$value)/(max(dfi$value)-min(dfi$value))
dfi$value2 <- (dfi$value-min(dfi$value))/(max(dfi$value)-min(dfi$value))
plot_x <- seq(from = 0.01, to =1, by = 1/ncol(df))
plot_y <- seq(from=0.01,to=1,by=1/nrow(df))
surface3d(x=plot_x, y=plot_y, z=dfi$value2, col="red", back="lines")
title3d(xlab="Num Columns", zlab="Numer of records", ylab="Time ")
mtext3d(x,edge='x',at=plot_x)
mtext3d(y,edge='y++',at=plot_y)
axes3d(tick="TRUE",xlab=x)
plot3d(plot_x,plot_y,dfi$value2,xlab=x)


cs <- read_csv('covalent_schema2.csv')
cs$C_DATE <- as.Date(cs$C_DATE,'%m/%d/%Y')
cs$Increment <- as.integer(cs$C_DATE - min(cs$C_DATE))
for(cols in 1:ncol(cs)) {
  for(index in 2:nrow(cs)) { if(cs[index,cols] == 0 & cs[(index-1),cols] !=1) { cs[index,cols] <- cs[(index-1),cols]}}
}
for(index in 2:nrow(cs)) { if(cs[index,3] == 0 & cs[(index-1),3] !=1) { cs[index,3] <- cs[(index-1),3]}}

df <- cs[,c(2:7)]
df <- df[,order(as.integer(names(df)))]

x <- c(1:ncol(df))
y <-cs$Increment
second.element <- function(my.string) { unlist(strsplit(my.string,"X"))[2]}
#df <- df[order(as.integer(sapply(names(df),second.element))),]
#df <- df[order(names(df))]
#df <- data.frame(df[,3],df[,6],df[,5],df[,1],df[,2],df[,4])
dfi <- expand.grid(x,y)
dfi$value <- 0	

for(index in 1:nrow(dfi)) {dfi[index,]$value <- as.integer(df[(dfi[index,]$Var2+1),dfi[index,]$Var1])}

columns <- ncol(df)


#dfi$value2 <- dfi$value-min(dfi$value)/(max(dfi$value)-min(dfi$value))
dfi$value2 <- (dfi$value-min(dfi$value,na.rm=TRUE))/(max(dfi$value,na.rm=TRUE)-min(dfi$value,na.rm=TRUE))
plot_x <- seq(from = 1/ncol(df), to =1, by = 1/ncol(df))
plot_y <- seq(from=1/nrow(df),to=1,by=1/nrow(df))
surface3d(x=plot_x, y=plot_y, z=dfi$value2, col="red", back="lines")
title3d(xlab="Average Document Length", zlab="Numer of records", ylab="Time ")
mtext3d(names(df),edge='x',at=plot_x)
#text3d(y,edge='y++',at=plot_y)
axes3d(tick="TRUE",xlab=x)
#legend3d("right", c("  1,542 ActivityMetaData"
                  # ,"  2,305 ActivityAggregate"
                  # ,"  9,876 ActivityTake"
                  # ," 14,400 Activity"
                  # ," 61,926 Course"
                  # ,"101,462 CourseStructure"))

#plot3d(plot_x,plot_y,dfi$value2,xlab=x)



cs[cs$`1542` ==0,]$`1542` <- 1




library(gplots)
heatmap(t(as.matrix(df)),col=topo.colors(100))
heatmap.2(t(as.matrix(df)),dendrogram='none',Rowv=FALSE,Colv=FALSE,trace='none',col=topo.colors(100))



for(rownumber in 1:nrow(df)) {
  index2 <-1
  for(index in 1:(columns*rownumber)) {
    if(as.integer(row.names(dfi)[index]) <= (columns*rownumber)) { 
      #print(index)
      if(dfi[index,]$value == 0) {
        #print(as.integer(df[rownumber,index2]))
        print(index2)
        dfi[index,]$value<- as.integer(df[rownumber,index2])
        #print(index2)
        #print(cols)
        
        index2 <- index2 + 1
        if(index2 >columns) { index2 <- 1}        
      }				
    }
    #print(index2)
  }
}
#x <- seq(from = 0.01,to = 1, by = 0.01)
#y <- seq(from = 0.01,to = 1, by = 0.01)

##z <- (0.9-xAxis)^2 + (0.5-yAxis)^2

#dfe <- expand.grid(x,y)
#xAxis <- dfe$Var1/(1+dfe$Var1*dfe$Var2)
#yAxis <- dfe$Var2/(1+dfe$Var1*dfe$Var2)

#dfe$z <- (0.9-xAxis)^2 + (0.5-yAxis)^2

surface3d(x=x, y=y, z=dfe$z, col="blue", back="lines")
title3d(xlab="x", zlab="z", ylab="y")
axes3d(tick="FALSE")

#plot3d(x,y,dfe$z)

#im <- with(df, akima::interp(x, y, z, nx = 1000, ny = 1000))
#df2 <- data.frame(expand.grid(x = im$x, y = im$y), z = c(im$z))

#ggplot(df2, aes(x, y, fill = z)) + 
#  geom_raster() + 
#  viridis::scale_fill_viridis()