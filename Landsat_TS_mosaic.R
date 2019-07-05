#example for using bfast spatial, taken from http://www.loicdutrieux.net/bfastSpatial/
#.libPaths("D:/R_library")
#.libPaths("P:/R/win-library/3.3")
#.libPaths("https://github.com")
#source("P:/Calltobegin.r")
#detach_package(raster)

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg)}; library(mypkg, character.only = T)}
#loadandinstall("installr")
#updateR()


# load the package
#devtools::install('loicdtx/bfastSpatial', ref = 'develop')
#devtools::install_github('loicdtx/bfastSpatial', ref='develop')

loadandinstall("rstudioapi")
loadandinstall("bfastSpatial")
loadandinstall("devtools")
loadandinstall("RStoolbox")
loadandinstall("raster")
loadandinstall("Hmisc") ###attention::Mask!!
loadandinstall("compiler")
loadandinstall("doParallel")
loadandinstall("bfast")


#set wd
setwd('E:/Mosaic')

####################################################################
####################################################################

# Get list of input data files. It is expected that you copy all .tar.gz archives to this folder:

#Running Landsat import in batch mode
# Get the directory where the Landsat archives are stored

###################################
#         batch process           #
###################################
dir_batch <- c("E:/Mosaic/Landsat_8","E:/Mosaic/ls_07_north","E:/Mosaic/ls_07_south","E:/Mosaic/ls_05_north","E:/Mosaic/ls_05_south")

#compile batch function!! (package compiler)    (#for LAndsat x (see LEDAPS product guide for details about the pixel_qa layer))
batch <- function(x, e, v){bfastSpatial::processLandsatBatch(x=x, pattern = NULL, outdir=dirout, srdir=NULL, vi='ndvi', e = e, mask = 'pixel_qa', keep = v, overwrite=TRUE)}
batch <- cmpfun(batch)
#############################
###doParallel###

no_cores<-detectCores()-1
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)
stopCluster(cl)


foreach (i=1:length(dir_batch))%dopar%{
  library(bfastSpatial)
  library(raster)
  
  #batch <- function(x, e, v){bfastSpatial::processLandsatBatch(x=x, pattern = NULL, outdir=dirout, srdir=NULL, vi='ndvi', e = e, mask = 'pixel_qa', keep = v, overwrite=TRUE)}
  #dir_batch <- c("E:/Mosaic/Landsat_8","E:/Mosaic/ls_07_north","E:/Mosaic/ls_07_south","E:/Mosaic/ls_05_north","E:/Mosaic/ls_05_south")
# Set the location of output and intermediary directories (everything in tmpdir in that case)
# We use dirname(rasterTmpFile()) instead of rasterOptions()$tmpdir to reduce verbose
rasterOptions(tmpdir = paste0("E:/Mosaic/tmp",i,sep=""))

        # Create a directory to store the output files.
        srdir <- dirout <- file.path(dirname(rasterTmpFile()), 'processed_batch')
        dir.create(dirout, showWarning=FALSE)

#Define extent within Landsat image to be processed (in UTM coordinates):
ex <- extent(c(598750.8 , 613857.2, -1542592, -1516472))
  
# keep values LS5-7 or LS8
#ls8 kls08 <- c(322, 386) & ls 5-7 kls_5_7 <- c(66, 130)
if(i==1){v <- c(322, 386)}else {v <-  c(66, 130)}

# batch process
batch(dir_batch[i], ex, v)
}
stopCluster(cl)

list <- list.files(path = dir_batch[i], full.names = TRUE)
list

##########################
#        time stack      #
#        by sensor       #
##########################

dir <- c("E:/Mosaic/tmp1/processed_batch/ndvi", "E:/Mosaic/tmp2/processed_batch/ndvi", "E:/Mosaic/tmp3/processed_batch/ndvi", "E:/Mosaic/tmp4/processed_batch/ndvi", "E:/Mosaic/tmp5/processed_batch/ndvi")

list_ls08 <- list.files('E:/Mosaic/tmp1/processed_batch/ndvi', pattern=glob2rx('*.grd'), full.names=TRUE)
# set reference extent
reference <- raster(list_ls08[70])
plot(reference)

###################

#test foreach

#i=5
#rm(i)
# List processed files and plot one of the files in the list:
for (i in 1:length(dir)){list = dir[i]

# fit extent, correct dates/names and stack data
list1 <- list.files(list, pattern=glob2rx('*.grd'), full.names=TRUE)
list1_names <- substring(list.files(list, pattern=glob2rx('*.grd')), 11,18)
list1_dates <- paste0(substring(list1_names,1,4),"/",substring(list1_names,5,6),"/",substring(list1_names,7,8))

for(k in 1:length(list1)){tmp = raster(list1[[k]])
e <-extend(tmp, reference, value = NA)
#writeRaster(e, filename = paste0(list1[i],sep=""), pattern=glob2rx('*.grd'), overwrite=TRUE)
if(!exists(paste0("output",i,sep=""))){assign(paste0("output",i,sep=""), e) }else{
  assign(paste0("output",i,sep=""),stack(eval(parse(text=paste0("output",i,sep=""))),e))}
}
# add names and dates
if(i==1){output1@z <- as.list(list1_dates)}else if(i==2) {output2@z <- as.list(list1_dates)}else if(i==3) {output3@z <- as.list(list1_dates)} else if(i==4) {output4@z <- as.list(list1_dates)} else if(i==5) {output5@z <- as.list(list1_dates)}else {print("no")}
if(i==1){names(output1) <- list1_names}else if(i==2) {names(output2) <- list1_names}else if(i==3) {names(output3) <- list1_names} else if(i==4) {names(output4) <- list1_names} else if(i==5) {names(output5) <- list1_names}else {print("no")}
}

##########################
#        mosiac          #
#        by sensor       #
##########################

#ls07
mLS7 <- stack()
for(i in 1:nlayers(output2)){
  print(i)
  aa <- subset(output2,i)
  id <- which(names(aa) == names(output3))
  if((length(id) == !0)){
    bb <- subset(output3,id)
    cc <- mosaic(aa,bb,fun=mean)
    cc@z <- bb@z
    names(cc) <- names(bb)
   mLS7 <- stack(mLS7,cc)
}}

# check weather SLC starts on correct date (Juni 2003)
plot(mLS7$X20030501, main = "Landsat 7 \n 01.05.2003")
plot(mLS7$X20030805, main = "Landsat 7 \n 05.08.2003")


#ls05
mLS5 <- stack()
for(i in 1:nlayers(output4)){
  print(i)
  aa <- subset(output4,i)
  id <- which(names(aa) == names(output5))
  if((length(id) == !0)){
    bb <- subset(output5,id)
    cc <- mosaic(aa,bb,fun=mean)
    cc@z <- bb@z
    names(cc) <- names(bb)
    mLS5 <- stack(mLS5,cc)
  }}

# combine stacks and built global timestack
giga <- stack(output1, mLS7, mLS5)
TrueOrder <- order(names(giga))

giga2 <- stack()
for(i in 1:length(TrueOrder)){tmp=TrueOrder[i]
  print(tmp)
  cc <- subset(giga, tmp)
  giga2 <- stack(giga2 ,cc)
}
#### set dates to @z
layNames <- names(giga2)
newdates <- paste0(substring(layNames,2,5),"/",substring(layNames,6,7),"/",substring(layNames,8,9))
giga2@z <- as.list(newdates)
names(giga2)
str(giga2$X20180705)

#### export layer to file
writeRaster(giga2, filename = "timestack.tif", overwrite=TRUE)
