#This is an example of how to identify NABat priority grid cells within a specified spatial domain using UC Natural Reserves in California. 
library(rgdal)
library(raster)
library(sp)
library(spdep)

################################################################
# Import shapefile of Attributed Master Sample                 #
################################################################
folder=getwd()
file="NABat_California_attributed"
X=readOGR(dsn=folder, layer=file)
dim(X@data)
head(X@data)
names(X@data)
summary(X)
projection(X)
#shp<-X

#extra code if you were to include a neighboring state (e.g., Nevada)
#folder=getwd()
#file="NABat_Nevada_attributed"
#X2=readOGR(dsn=folder, layer=file)
#dim(X2@data)
#X2@data<-X2@data[,-(52)]
#head(X2@data)
#names(X2@data)
#summary(X2)
#projection(X2)
#shp<-rbind(X,X2)
#shp@data
#view class
#class(shp)
#view features count
#length(shp)
#spatial extent of the object
#extent(shp)
#coodinate reference system
#crs(shp)
#view metadata summary 
#shp
#plot(shp)
#header plus first 6 rows of attribute table
#head(shp@data)
#names(shp@data)
#summary(shp)
#projection(shp)

################## Reading in kml files
#read.kml <- function(file, layers) {
#  require(sp)
#  require(rgdal)
#  read.layer <- function (layer_name) {
#    spobj <- rgdal::readOGR(dsn=file, layer=layer_name)
#    coords <- coordinates(spobj)
#    colnames(coords) <- c('x', 'y', 'z')[1:ncol(coords)]
#    df <- data.frame(coords, spobj@data)
#    transform(df, layer=layer_name)
#  }
#  Reduce(rbind, lapply(layers, read.layer))
#}

################### Reading in kml/kmz files
#ogrListLayers("doc.kml")
#Y=read.kml("doc.kml","California")
#Y2 <- rgdal::readOGR(dsn="doc.kml", layer="California")

################### Reading in geodatabase
ogrListLayers("UC_NRS.gdb")
Y <- rgdal::readOGR(dsn="UC_NRS.gdb", layer="NRS_Boundaries")

shp<-Y
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

############ Make sure that projections match between objects
Y <- spTransform(Y, crs(X))
############ extract results from spatial join as data frame
join=over(x=X, y=Y)
str(join)
class(join)

############ Create new spatialpolygonsDataFrame object 'Z'
Z=X
Z@data<-cbind(Z@data,join)  #append the data frame 'join' to the data table for Z
Z<-Z[!(is.na(Z@data$Name)),]   #remove rows of data (cells) that don't overlap are of interest (i.e. ='NA')
dim(Z@data)

plot(Y, col="green")
plot(Z, add=TRUE)

###Identify priority grid cells for Z based on lowest GRTS_ID 
(tot.grts=length(unique(Z@data$GRTS_ID))) # 125 
top25.grts=sort(unique(Z@data$GRTS_ID)) [1:25]  #[1:round(tot.grts*.05)] # or top 5% GRTS priority cells in X and X2
top25_grts<-Z[Z@data$GRTS_ID %in% top25.grts,]

plot(Y, col="green")
plot(Z, add=TRUE)
plot(top25_grts,add=TRUE, col = "red")

### Write shapefile of top priority grid cells only 
layer.name<-c("top25_grts_UCNRS")
writeOGR(obj=top25_grts, dsn=folder,
         layer=layer.name,driver="ESRI Shapefile")
#########################################################################

Z2<-Z
Z2@data$top25<-0
Z2@data$top25[Z2@data$GRTS_ID %in% top25.grts]<-1
plot(Y, col ="green")
col<-Z2@data$top25
col[which(col==1)]<-"red"
plot(Z2,col= col,add=TRUE)

### Write shapefile of just UCNRS grid cells with top priority grid cells indicated in data column "top25"
layer.name<-c("UCNRS_grts")
writeOGR(obj=Z2, dsn=folder,
         layer=layer.name,driver="ESRI Shapefile")
#########################################################################

X2<-X
X2@data$top25<-0
X2@data$top25[X2@data$GRTS_ID %in% top25.grts]<-1
plot(Y, col ="green")
col<-X2@data$top25
col[which(col==1)]<-"red"
col[which(join$Name!="<NA>"&col!="red")]<-"green"
plot(X2,col= col,add=TRUE)

### Write shapefile with all grid cells with top priority cells indicated in data column "top25" 
layer.name<-c("All_grts_UCNRS")
writeOGR(obj=X2, dsn=folder,
         layer=layer.name,driver="ESRI Shapefile")
#########################################################################









