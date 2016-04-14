################################################################################
#' this function is to classify ecoregion in to study region 
#' based on ecoregion map and study area map
#' 
#' @param filePath  Character string. The path lead to the fold that contains the data.
#'
#' 
#' @param pspType,  Character string. It tells which data source from,
#'                  ie., Mature or Juvenile. It must be one of these
#'        
#'
#' @return Two objects: studyareaecoregion is RasterLayer 
#'                      and attributesTable is data table
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname obtainTreeDataAB
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("obtainTreeDataAB", function(filePath, pspType) {
  standardGeneric("obtainTreeDataAB")
})

#' @export
#' @rdname obtainTreeDataAB
setMethod(
  "obtainTreeDataAB",
  signature = c(filePath = "character", 
                pspType = "character"),
  definition = function(filePath, 
                        pspType) {
    
    tepdir <- tempdir()
    filePath <- "C:/Users/yonluo/Documents/PSPs/AB/ESRDMaturePSPs2013"
    
    for(i in dir(filePath, pattern = ".DAT")[1:5]){
      if(file.exists(file.path(tepdir, "temp.csv"))){
        file.remove(file.path(tepdir, "temp.csv"))
      }
      file.copy(file.path(filePath, i),file.path(tepdir, "temp.csv"))
      mdata<-read.csv(file.path(tepdir, "temp.csv"),
                      header = FALSE,
                      stringsAsFactors = FALSE) 
      headdata1 <- headdata(mdata) %>%
        data.table
      headdata1[Record]
      write.csv(headdata1,
                file.path(tepdir,"headdatatemp.csv"),
                row.names = FALSE)
      headdata1 <- read.csv(file.path(tepdir, "headdatatemp.csv"),
                            header = TRUE,
                            stringsAsFactors = FALSE)
      headdata1 <- data.table(headdata1)[Recordtype==1,]
      totalheaddata <- rbindlist(list(totalheaddata, headdata1))
      
      treedata1 <- treedata(mdata)
      write.csv(treedata1,
                file.path(tepdir, "treedatatemp.csv"),
                row.names = FALSE)
      treedata1 <- read.csv(file.path(tepdir, "treedatatemp.csv"),
                            header = TRUE,
                            stringsAsFactors = FALSE)
      treedata1 <- data.table(treedata1)[Recordtype==2,]
      totaltreedata <- rbindlist(list(totaltreedata,treedata1))
      
      regendata1 <- regendata(mdata)
      write.csv(regendata1,
                file.path(tepdir, "regendatatemp.csv"),
                row.names = FALSE)
      regendata1 <- read.csv(file.path(tepdir, "regendatatemp.csv"),
                             header = TRUE,
                             stringsAsFactors = FALSE)
      regendata1 <- data.table(regendata1)[Recordtype==3, ]
      totalregendata <- rbindlist(list(totalregendata,regendata1))
      
      if(nrow(treedata1)+nrow(headdata1)+nrow(regendata1) != nrow(mdata)){
        
        cat("please check the file", i, "\n")
      }
    }
    
    
    
    
    
  })



headdata<-function(data)
{n<-nrow(data)
 output<-data.table(matrix(nrow=n,ncol=33))
 for(i in 1:n)
 { irow<-as.character(data[i,1])
   output[i,1]<-substr(irow,1,2)
   output[i,2]<-substr(irow,3,12)
   output[i,3]<-substr(irow,13,13)
   output[i,4]<-substr(irow,14,15)
   output[i,5]<-substr(irow,16,19)
   output[i,6]<-substr(irow,20,21)
   output[i,7]<-substr(irow,22,23)
   output[i,8]<-substr(irow,24,25)
   output[i,9]<-substr(irow,26,27)
   output[i,10]<-substr(irow,28,30)
   output[i,11]<-substr(irow,31,32)
   output[i,12]<-substr(irow,33,33)
   output[i,13]<-substr(irow,34,36)
   output[i,14]<-substr(irow,37,37)
   output[i,15]<-substr(irow,38,39)
   output[i,16]<-substr(irow,40,44)
   output[i,17]<-substr(irow,45,48)
   output[i,18]<-substr(irow,49,52)
   output[i,19]<-substr(irow,53,73)
   output[i,20]<-substr(irow,74,94)
   output[i,21]<-substr(irow,95,95)
   output[i,22]<-substr(irow,96,98)
   output[i,23]<-substr(irow,99,100)
   output[i,24]<-substr(irow,101,104)
   output[i,25]<-substr(irow,105,105)
   output[i,26]<-substr(irow,106,106)
   output[i,27]<-substr(irow,107,108)
   output[i,28]<-substr(irow,109,109)
   output[i,29]<-substr(irow,110,112)
   output[i,30]<-substr(irow,113,113)
   output[i,31]<-substr(irow,114,114)
   output[i,32]<-substr(irow,115,115)
   output[i,33]<-substr(irow,116,119)
 }
 names(output)<-c("Agency","Groupnumber","Plotnumber","Measurementnumber","MeasureYear","MeasureMonth","MeasureDay",
                  "Legalsubdivision","Section","Township","Range","Meridian","Plottreatment","Imperial","Recordtype",
                  "Treeplotsize","Saplingplotsize","Regenplotsize","AVIinterpretedoverstory","AVIinterpretedunderstory",
                  "Location","Slope percent","Aspect",
                  "Elevation","Erosionpotential","Drainage","Depth_to_mineral_soil","Surface_veg_type",
                  "Groud_coverage","Access","Plot_damage","Buffer_damage","Establish_year")
 return(output)
}


treedata<-function(data)
{n<-nrow(data)
 output<-data.frame(matrix(nrow=n,ncol=39))
 
 for(i in 1:n)
 { irow<-as.character(data[i,1])
   output[i,1]<-substr(irow,1,2)
   output[i,2]<-substr(irow,3,12)
   output[i,3]<-substr(irow,13,13)
   output[i,4]<-substr(irow,14,15)
   output[i,5]<-substr(irow,16,19)
   output[i,6]<-substr(irow,20,21)
   output[i,7]<-substr(irow,22,23)
   output[i,8]<-substr(irow,24,25)
   output[i,9]<-substr(irow,26,27)
   output[i,10]<-substr(irow,28,30)
   output[i,11]<-substr(irow,31,32)
   output[i,12]<-substr(irow,33,33)
   output[i,13]<-substr(irow,34,36)
   output[i,14]<-substr(irow,37,37)
   output[i,15]<-substr(irow,38,39)
   output[i,16]<-substr(irow,40,43)
   output[i,17]<-substr(irow,44,45)
   output[i,18]<-substr(irow,46,49)
   output[i,19]<-substr(irow,50,53)
   output[i,20]<-substr(irow,54,57)
   output[i,21]<-substr(irow,58,58)
   output[i,22]<-substr(irow,59,60)
   output[i,23]<-substr(irow,61,62)
   output[i,24]<-substr(irow,63,64)
   output[i,25]<-substr(irow,65,67)
   output[i,26]<-substr(irow,68,70)
   output[i,27]<-substr(irow,71,73)
   output[i,28]<-substr(irow,74,76)
   output[i,29]<-substr(irow,77,79)
   output[i,30]<-substr(irow,80,82)
   output[i,31]<-substr(irow,83,89)
   output[i,32]<-substr(irow,90,96)
   output[i,33]<-substr(irow,97,103)
   output[i,34]<-substr(irow,104,107)
   output[i,35]<-substr(irow,108,108)
   output[i,36]<-substr(irow,109,111)
   output[i,37]<-substr(irow,112,114)
   output[i,38]<-substr(irow,115,117)
   output[i,39]<-substr(irow,118,121)
 }
 names(output)<-c("Agency","Groupnumber","Plotnumber","Measurementnumber","MeasureYear","MeasureMonth","MeasureDay",
                  "Legalsubdivision","Section","Township","Range","Meridian","Plottreatment","Imperial","Recordtype",
                  "Treenumber","Species","DBH","Height","Heighttolivecrown","Crownclass","Conditioncode1","Conditioncode2",
                  "Conditioncode3","DBHage","Stumpage","Incrementprevioustenyears","Incrementpreviousyears",
                  "Azimuth","Distance","Treeplotsize","Saplingplotsize","Regenerationplotsize","Establishmentyear",
                  "Crownstatus","Crownwidthnorth","Crownwidthwest","Crownwidthsouth","Crownwidtheast")
 return(output)
}


regendata<-function(data)
{n<-nrow(data)
 output<-data.frame(matrix(nrow=n,ncol=26))
 
 for(i in 1:n)
 { irow<-as.character(data[i,1])
   output[i,1]<-substr(irow,1,2)
   output[i,2]<-substr(irow,3,12)
   output[i,3]<-substr(irow,13,13)
   output[i,4]<-substr(irow,14,15)
   output[i,5]<-substr(irow,16,19)
   output[i,6]<-substr(irow,20,21)
   output[i,7]<-substr(irow,22,23)
   output[i,8]<-substr(irow,24,25)
   output[i,9]<-substr(irow,26,27)
   output[i,10]<-substr(irow,28,30)
   output[i,11]<-substr(irow,31,32)
   output[i,12]<-substr(irow,33,33)
   output[i,13]<-substr(irow,34,36)
   output[i,14]<-substr(irow,37,37)
   output[i,15]<-substr(irow,38,39)
   output[i,16]<-substr(irow,40,43)
   output[i,17]<-substr(irow,44,45)
   output[i,18]<-substr(irow,46,48)
   output[i,19]<-substr(irow,49,51)
   output[i,20]<-substr(irow,52,54)
   output[i,21]<-substr(irow,55,57)
   output[i,22]<-substr(irow,58,60)
   output[i,23]<-substr(irow,61,67)
   output[i,24]<-substr(irow,68,74)
   output[i,25]<-substr(irow,75,81)
   output[i,26]<-substr(irow,82,85)
 }
 names(output)<-c("Agency","Groupnumber","Plotnumber","Measurementnumber","MeasureYear","MeasureMonth","MeasureDay",
                  "Legalsubdivision","Section","Township","Range","Meridian","Plottreatment","Imperial","Recordtype",
                  "Treenumber","Species","Regeneration_in_height_class1","Regeneration_in_height_class2",
                  "Regeneration_in_height_class3","Regeneration_in_height_class4","Regeneration_in_height_class5",
                  "Treeplotsize","Saplingplotsize","Regenerationplotsize","Establishmentyear")
 return(output)
}







