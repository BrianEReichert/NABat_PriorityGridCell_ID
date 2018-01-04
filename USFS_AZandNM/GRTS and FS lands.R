setwd("X:/Individual Shared Data/breichert/NABat/Master Sample/USFS_AZandNM")

################################################################
# Import shapefile of Attributed Master Sample                 #
################################################################
folder="X:/Individual Shared Data/breichert/NABat/Master Sample/CONUS_bystate_attributed/Arizona"
file="NABat_Arizona_attributed"
AZ=readOGR(dsn=folder, layer=file)
dim(AZ@data)
head(AZ@data)
names(AZ@data)
summary(AZ)
projection(AZ)

folder="X:/Individual Shared Data/breichert/NABat/Master Sample/CONUS_bystate_attributed/New Mexico"
file="NABat_New Mexico_attributed"
NM=readOGR(dsn=folder, layer=file)
dim(NM@data)
NM@data<-NM@data[,-(52)]
head(NM@data)
names(NM@data)
summary(NM)
projection(NM)


shp<-rbind(AZ,NM)
shp@data
#view class
class(shp)
#view features count
length(shp)
#spatial extent of the object
extent(shp)
#coodinate reference system
crs(shp)
#view metadata summary 
shp
plot(shp)
#header plus first 6 rows of attribute table
head(shp@data)
names(shp@data)
summary(shp)
projection(shp)

###Summarizing GRTS and BLM lands 
(tot.grts=length(unique(shp@data$GRTS_ID))) # 6304 
top5.grts=sort(unique(shp@data$GRTS_ID))[1:round(tot.grts*.05)] # top 5% GRTS priority cells in AZ and NM
top5_grts<-shp[shp@data$GRTS_ID %in% top5.grts,]
#plot(top5_grts)
#layer.name<-c("top5_grts")
#writeOGR(obj=top5_grts, dsn=folder,
#         layer=layer.name,driver="ESRI Shapefile")

(tot.FSgrts=length(unique(shp@data$GRTS_ID[shp@data$own_USFS>0]))) # 6304 
top5.FSgrts=sort(unique(shp@data$GRTS_ID[shp@data$own_USFS>0]))[1:round(tot.FSgrts*.05)] # top 5% GRTS priority cells in FS lands
top5_FSgrts<-shp[shp@data$GRTS_ID %in% top5.grts,]



fs.grts=shp[shp@data$own_USFS>0,]# 3146 NPS GRTS 
plot(fs.grts, col="green",add=TRUE)
fs.priority.grts=shp[shp@data$own_USFS>0&shp@data$GRTS_ID %in% top5.grts,]# 3146 NPS GRTS 
plot(fs.priority.grts, col="red",add=TRUE)

shp@data$FSpriority<-shp@data$own_USFS
shp@data$FSpriority[shp@data$FSpriority>0] <-1
shp@data$FSpriority[shp@data$GRTS_ID %in% top5.FSgrts]<-2
shp@data$FSpriority[shp@data$FSpriority>0&shp@data$GRTS_ID %in% top5.grts&shp@data$FSpriority!=2]<-3
plot(shp,col=shp@data$FSpriority)


#top5.BLM.grts<-shp@data[shp@data$own_BLM>0&shp@data$GRTS_ID==top5.grts,]  # BLM has 952 of the top 5 GRTS
#top5.BLM.grts<-shp@data$GRTS_ID[shp@data$own_BLM>0&shp@data$GRTS_ID==top5.grts]
#(top5.BLM.prop<-length(top5.BLM.grts)/length(top5.grts)) # 0.23 of top 5 GRTS are on BLM Lands (1904 grid cells)
#dim(shp@data[shp@data$own_BLM>0&shp@data$GRTS_ID==top5.grts,])

layer.name<-c("NABatprioritycells_FS_AZ_NM")
folder<-"X:/Individual Shared Data/breichert/NABat/Master Sample/USFS_AZandNM"
writeOGR(obj=shp, dsn=folder,
         layer=layer.name,driver="ESRI Shapefile")

#shp@data$GRTS_ID[shp@data$own_BLM>0]
#uniqe(shp@data$COMMON_NAM)

