################################################################################
#' this function is to extract PSP inventory data from Alberta  
#' 
#' 
#' @param filePath  Character string. The path lead to the fold that contains the data.
#'
#' 
#' @param pspType,  Character string. It tells which data source from,
#'                  ie., Mature or Juvenile. It must be one of these
#'        
#' @importFrom data.table data.table
#' @return  
#'                      
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
    if(pspType == "Mature"){
      headData <- data.table(Agency = integer(), Groupnumber = integer(), Plotnumber = integer(),
                             Measurementnumber = integer(), MeasureYear = integer(), MeasureMonth = integer(),
                             MeasureDay = integer(), Legalsubdivision = integer(), Section = integer(),
                             Township = integer(), Range = integer(), Meridian = integer(),
                             Plottreatment = integer(), Imperial = logical(),
                             Recordtype = integer(), Treeplotsize = integer(),
                             Saplingplotsize = integer(), Regenplotsize = integer(),
                             AVIinterpretedoverstory = character(), AVIinterpretedunderstory = character(),
                             Location = integer(), Slope_percent = integer(),
                             Aspect = character(), Elevation = integer(),
                             Erosionpotential = integer(), Drainage = integer(),
                             Depth_to_mineral_soil = integer(), Surface_veg_type = integer(),
                             Groud_coverage = integer(), Access = integer(),
                             Plot_damage = integer(), Buffer_damage = integer(),
                             Establish_year = integer())
      treeData <- data.table(Agency = integer(), Groupnumber = integer(), 
                             Plotnumber = integer(), Measurementnumber = integer(), 
                             MeasureYear = integer(), MeasureMonth = integer(),
                             MeasureDay = integer(), Legalsubdivision = integer(),
                             Section = integer(), Township = integer(), Range = integer(), 
                             Meridian = integer(), Plottreatment = integer(), 
                             Imperial = logical(), Recordtype = integer(), 
                             Treenumber = integer(), Species = character(), 
                             DBH = integer(), Height = integer(), 
                             Heighttolivecrown = integer(), Crownclass = character(),
                             Conditioncode1 = integer(), Conditioncode2 = integer(), 
                             Conditioncode3 = integer(), DBHage = integer(), 
                             Stumpage = integer(), Incrementprevioustenyears = integer(), 
                             Incrementpreviousyears = integer(), 
                             Azimuth = integer(), Distance = integer(),
                             Treeplotsize = integer(), Saplingplotsize = integer(), 
                             Regenerationplotsize = integer(), Establishmentyear = integer(), 
                             Crownstatus = character(), Crownwidthnorth = integer(), 
                             Crownwidthwest = integer(), 
                             Crownwidthsouth = integer(), Crownwidtheast = integer())
      regenData <- data.table(Agency = integer(), Groupnumber = integer(),
                              Plotnumber = integer(), 
                              Measurementnumber = integer(), MeasureYear = integer(), 
                              MeasureMonth = integer(), MeasureDay = integer(), 
                              Legalsubdivision = integer(), Section = integer(), 
                              Township = integer(), Range = integer(), Meridian = integer(), 
                              Plottreatment = integer(), Imperial = logical(), Recordtype = integer(), 
                              Treenumber = integer(), Species = character(), 
                              Regeneration_in_height_class1 = integer(), 
                              Regeneration_in_height_class2 = integer(), 
                              Regeneration_in_height_class3 = integer(), 
                              Regeneration_in_height_class4 = integer(), 
                              Regeneration_in_height_class5 = integer(), 
                              Treeplotsize = integer(), Saplingplotsize = integer(), 
                              Regenerationplotsize = integer(), 
                              Establishmentyear = integer())
      for(i in dir(filePath, pattern = "*.DAT")){
        if(file.exists(file.path(tepdir, "temp.csv"))){
          file.remove(file.path(tepdir, "temp.csv"))
        }
        file.copy(file.path(filePath, i),file.path(tepdir, "temp.csv"))
        mdata<-read.csv(file.path(tepdir, "temp.csv"),
                        header = FALSE,
                        stringsAsFactors = FALSE) 
        headData <- rbindlist(list(headData, obtainHeadDataABMature(mdata)))
        
        treeData <- rbindlist(list(treeData,obtainTreeDataABMature(mdata)))
        
        regenData <- rbindlist(list(regenData,obtainRegenDataABMature(mdata)))
        
      }
    } else if (pspType == "Juvenile"){
      treeData <- data.table(Plotnumber = integer(), MeasureYear = integer(), 
                             MeasureMonth = integer(), Recordtype = integer(), 
                             Treenumber = integer(), RegenPlot = integer(), 
                             Species = character(),  Treetype = character(), 
                             Birthyear = integer(), Height = integer(), 
                             DBH = integer(), Conditioncode1 = integer(), 
                             Conditioncode2 = integer(), Conditioncode3 = integer(), 
                             LiveCrownHeight = integer(), CrownClass = character(), 
                             Azimuth = integer(), Distance = integer(), 
                             Age = integer(), competitionHeight = integer(), 
                             competitionType = integer(),  
                             RootCollarDiameter = integer(), 
                             CrownStat = character(), CrownN = integer(), 
                             CrownW = integer(), CrownS = integer(), 
                             CrownE = integer(), 
                             PlotTreatmentType = integer())
      filePath <- "C:/Users/Yong Luo/Documents/PSPs/AB_juveniles"
      for(i in dir(filePath, pattern = "*.DAT")){
        if(file.exists(file.path(tepdir, "temp.csv"))){
          file.remove(file.path(tepdir, "temp.csv"))
        }
        file.copy(file.path(filePath, i),file.path(tepdir, "temp.csv"))
        mdata<-read.csv(file.path(tepdir, "temp.csv"),
                        header = FALSE,
                        stringsAsFactors = FALSE) 
        treeData <- rbindlist(list(treeData, obtainTreeDataABJuvenile(mdata)))
      }
    } else {
      stop("please specify pspType correctly.")
    }
    if(pspType == "Mature"){
      return(list(headData = headData, treeData = treeData, regenData = regenData))
    } else if (pspType == "Juvenile"){
      return(list(headData = NA, treeData = treeData, regenData = NA))
    }
  })

setGeneric("obtainHeadDataABMature", function(rawdata) {
  standardGeneric("obtainHeadDataABMature")
})

#' @export
#' @rdname obtainHeadDataABMature
setMethod(
  "obtainHeadDataABMature",
  signature = c(rawdata = "data.frame"),
  definition = function(rawdata) {
    output <- data.table::data.table(matrix(nrow = nrow(rawdata), ncol = 33))
    rawdata <- as.character(rawdata[,1])
    output[ , 1] <- substr(rawdata, 1, 2)
    output[ , 2] <- substr(rawdata, 3, 12)
    output[ , 3] <- substr(rawdata, 13, 13)
    output[ , 4] <- substr(rawdata, 14, 15)
    output[ , 5] <- substr(rawdata, 16, 19)
    output[ , 6] <- substr(rawdata, 20, 21)
    output[ , 7] <- substr(rawdata, 22, 23)
    output[ , 8] <- substr(rawdata, 24, 25)
    output[ , 9] <- substr(rawdata, 26, 27)
    output[ , 10] <- substr(rawdata, 28, 30)
    output[ , 11] <- substr(rawdata, 31, 32)
    output[ , 12] <- substr(rawdata, 33, 33)
    output[ , 13] <- substr(rawdata, 34, 36)
    output[ , 14] <- substr(rawdata, 37, 37)
    output[ , 15] <- substr(rawdata, 38, 39)
    output[ , 16] <- substr(rawdata, 40, 44)
    output[ , 17] <- substr(rawdata, 45, 48)
    output[ , 18] <- substr(rawdata, 49, 52)
    output[ , 19] <- substr(rawdata, 53, 73)
    output[ , 20] <- substr(rawdata, 74, 94)
    output[ , 21] <- substr(rawdata, 95, 95)
    output[ , 22] <- substr(rawdata, 96, 98)
    output[ , 23] <- substr(rawdata, 99, 100)
    output[ , 24] <- substr(rawdata, 101, 104)
    output[ , 25] <- substr(rawdata, 105, 105)
    output[ , 26] <- substr(rawdata, 106, 106)
    output[ , 27] <- substr(rawdata, 107, 108)
    output[ , 28] <- substr(rawdata, 109, 109)
    output[ , 29] <- substr(rawdata, 110, 112)
    output[ , 30] <- substr(rawdata, 113, 113)
    output[ , 31] <- substr(rawdata, 114, 114)
    output[ , 32] <- substr(rawdata, 115, 115)
    output[ , 33] <- substr(rawdata, 116, 119)
    names(output) <- c("Agency","Groupnumber","Plotnumber","Measurementnumber","MeasureYear","MeasureMonth","MeasureDay",
                       "Legalsubdivision","Section","Township","Range","Meridian","Plottreatment","Imperial","Recordtype",
                       "Treeplotsize","Saplingplotsize","Regenplotsize","AVIinterpretedoverstory","AVIinterpretedunderstory",
                       "Location","Slope_percent","Aspect",
                       "Elevation","Erosionpotential","Drainage","Depth_to_mineral_soil","Surface_veg_type",
                       "Groud_coverage","Access","Plot_damage","Buffer_damage","Establish_year")
    output <- output[,Recordtype:=as.integer(Recordtype)][Recordtype == 1,]
    output[,':='(Agency = as.integer(Agency), Groupnumber = as.integer(Groupnumber),
                 Plotnumber = as.integer(Plotnumber), Measurementnumber = as.integer(Measurementnumber),
                 MeasureYear = as.integer(MeasureYear), MeasureMonth = as.integer(MeasureMonth),
                 MeasureDay = as.integer(MeasureDay), Legalsubdivision = as.integer(Legalsubdivision),
                 Section = as.integer(Section), Township = as.integer(Township),
                 Range = as.integer(Range), Meridian = as.integer(Meridian),
                 Plottreatment = as.integer(Plottreatment), Imperial = as.logical(Imperial),
                 Recordtype = as.integer(Recordtype), Treeplotsize = as.integer(Treeplotsize),
                 Saplingplotsize = as.integer(Saplingplotsize), Regenplotsize = as.integer(Regenplotsize),
                 AVIinterpretedoverstory = as.character(AVIinterpretedoverstory), 
                 AVIinterpretedunderstory = as.character(AVIinterpretedunderstory),
                 Location = as.integer(Location), Slope_percent = as.integer(Slope_percent),
                 Aspect = as.character(Aspect), Elevation = as.integer(Elevation),
                 Erosionpotential = as.integer(Erosionpotential), Drainage = as.integer(Drainage),
                 Depth_to_mineral_soil = as.numeric(Depth_to_mineral_soil),
                 Surface_veg_type = as.integer(Surface_veg_type),
                 Groud_coverage = as.integer(Groud_coverage), Access = as.integer(Access),
                 Plot_damage = as.integer(Plot_damage), Buffer_damage = as.integer(Buffer_damage),
                 Establish_year = as.integer(Establish_year))]
    
    return(output)
  })


setGeneric("obtainTreeDataABMature", function(rawdata) {
  standardGeneric("obtainTreeDataABMature")
})

#' @export
#' @rdname obtainTreeDataABMature
setMethod(
  "obtainTreeDataABMature",
  signature = c(rawdata = "data.frame"),
  definition = function(rawdata) {
    output <- data.table(matrix(nrow = nrow(rawdata), ncol = 39))
    rawdata <- as.character(rawdata[,1])
    output[ , 1]<-substr(rawdata, 1, 2)
    output[ , 2]<-substr(rawdata, 3, 12)
    output[ , 3]<-substr(rawdata, 13, 13)
    output[ , 4]<-substr(rawdata, 14, 15)
    output[ , 5]<-substr(rawdata, 16, 19)
    output[ , 6]<-substr(rawdata, 20, 21)
    output[ , 7]<-substr(rawdata, 22, 23)
    output[ , 8]<-substr(rawdata, 24, 25)
    output[ , 9]<-substr(rawdata, 26, 27)
    output[ , 10]<-substr(rawdata, 28, 30)
    output[ , 11]<-substr(rawdata, 31, 32)
    output[ , 12]<-substr(rawdata, 33, 33)
    output[ , 13]<-substr(rawdata, 34, 36)
    output[ , 14]<-substr(rawdata, 37, 37)
    output[ , 15]<-substr(rawdata, 38, 39)
    output[ , 16]<-substr(rawdata, 40, 43)
    output[ , 17]<-substr(rawdata, 44, 45)
    output[ , 18]<-substr(rawdata, 46, 49)
    output[ , 19]<-substr(rawdata, 50, 53)
    output[ , 20]<-substr(rawdata, 54, 57)
    output[ , 21]<-substr(rawdata, 58, 58)
    output[ , 22]<-substr(rawdata, 59, 60)
    output[ , 23]<-substr(rawdata, 61, 62)
    output[ , 24]<-substr(rawdata, 63, 64)
    output[ , 25]<-substr(rawdata, 65, 67)
    output[ , 26]<-substr(rawdata, 68, 70)
    output[ , 27]<-substr(rawdata, 71, 73)
    output[ , 28]<-substr(rawdata, 74, 76)
    output[ , 29]<-substr(rawdata, 77, 79)
    output[ , 30]<-substr(rawdata, 80, 82)
    output[ , 31]<-substr(rawdata, 83, 89)
    output[ , 32]<-substr(rawdata, 90, 96)
    output[ , 33]<-substr(rawdata, 97, 103)
    output[ , 34]<-substr(rawdata, 104, 107)
    output[ , 35]<-substr(rawdata, 108, 108)
    output[ , 36]<-substr(rawdata, 109, 111)
    output[ , 37]<-substr(rawdata, 112, 114)
    output[ , 38]<-substr(rawdata, 115, 117)
    output[ , 39]<-substr(rawdata, 118, 121)
    names(output)<-c("Agency", "Groupnumber", "Plotnumber", "Measurementnumber", 
                     "MeasureYear", "MeasureMonth", "MeasureDay", 
                     "Legalsubdivision", "Section", "Township", "Range", 
                     "Meridian", "Plottreatment", "Imperial", "Recordtype", 
                     "Treenumber", "Species", "DBH", "Height", "Heighttolivecrown", 
                     "Crownclass", "Conditioncode1", "Conditioncode2", 
                     "Conditioncode3", "DBHage", "Stumpage",
                     "Incrementprevioustenyears", "Incrementpreviousyears", 
                     "Azimuth", "Distance", "Treeplotsize", "Saplingplotsize", 
                     "Regenerationplotsize", "Establishmentyear", 
                     "Crownstatus", "Crownwidthnorth", "Crownwidthwest", 
                     "Crownwidthsouth", "Crownwidtheast")
    output <- output[,Recordtype := as.integer(Recordtype)][Recordtype == 2,]
    output[,':='(Agency = as.integer(Agency), Groupnumber = as.integer(Groupnumber), 
                 Plotnumber = as.integer(Plotnumber), Measurementnumber = as.integer(Measurementnumber), 
                 MeasureYear = as.integer(MeasureYear), MeasureMonth = as.integer(MeasureMonth),
                 MeasureDay = as.integer(MeasureDay), Legalsubdivision = as.integer(Legalsubdivision),
                 Section = as.integer(Section), Township = as.integer(Township), Range = as.integer(Range), 
                 Meridian = as.integer(Meridian), Plottreatment = as.integer(Plottreatment), 
                 Imperial = as.logical(Imperial), Recordtype = as.integer(Recordtype), 
                 Treenumber = as.integer(Treenumber), Species = as.character(Species), 
                 DBH = as.integer(DBH), Height = as.integer(Height), 
                 Heighttolivecrown = as.integer(Heighttolivecrown), 
                 Crownclass = as.character(Crownclass),
                 Conditioncode1 = as.integer(Conditioncode1), Conditioncode2 = as.integer(Conditioncode2), 
                 Conditioncode3 = as.integer(Conditioncode3), DBHage = as.integer(DBHage), 
                 Stumpage = as.integer(Stumpage), 
                 Incrementprevioustenyears = as.integer(Incrementprevioustenyears), 
                 Incrementpreviousyears = as.integer(Incrementpreviousyears), 
                 Azimuth = as.integer(Azimuth), Distance = as.integer(Distance),
                 Treeplotsize = as.integer(Treeplotsize),
                 Saplingplotsize = as.integer(Saplingplotsize), 
                 Regenerationplotsize = as.integer(Regenerationplotsize), 
                 Establishmentyear = as.integer(Establishmentyear), 
                 Crownstatus = as.character(Crownstatus), Crownwidthnorth = as.integer(Crownwidthnorth), 
                 Crownwidthwest = as.integer(Crownwidthwest), 
                 Crownwidthsouth = as.integer(Crownwidthsouth), 
                 Crownwidtheast = as.integer(Crownwidtheast))]
    return(output)
  })
    

setGeneric("obtainRegenDataABMature", function(rawdata) {
  standardGeneric("obtainRegenDataABMature")
})

#' @export
#' @rdname obtainRegenDataABMature
setMethod(
  "obtainRegenDataABMature",
  signature = c(rawdata = "data.frame"),
  definition = function(rawdata) {
    output <- data.table(matrix(nrow = nrow(rawdata), ncol = 26))
    rawdata <- as.character(rawdata[, 1])
    output[ , 1] <- substr(rawdata, 1, 2)
    output[ , 2] <- substr(rawdata, 3, 12)
    output[ , 3] <- substr(rawdata, 13, 13)
    output[ , 4] <- substr(rawdata, 14, 15)
    output[ , 5] <- substr(rawdata, 16, 19)
    output[ , 6] <- substr(rawdata, 20, 21)
    output[ , 7] <- substr(rawdata, 22, 23)
    output[ , 8] <- substr(rawdata, 24, 25)
    output[ , 9] <- substr(rawdata, 26, 27)
    output[ , 10] <- substr(rawdata, 28, 30)
    output[ , 11] <- substr(rawdata, 31, 32)
    output[ , 12] <- substr(rawdata, 33, 33)
    output[ , 13] <- substr(rawdata, 34, 36)
    output[ , 14] <- substr(rawdata, 37, 37)
    output[ , 15] <- substr(rawdata, 38, 39)
    output[ , 16] <- substr(rawdata, 40, 43)
    output[ , 17] <- substr(rawdata, 44, 45)
    output[ , 18] <- substr(rawdata, 46, 48)
    output[ , 19] <- substr(rawdata, 49, 51)
    output[ , 20] <- substr(rawdata, 52, 54)
    output[ , 21] <- substr(rawdata, 55, 57)
    output[ , 22] <- substr(rawdata, 58, 60)
    output[ , 23] <- substr(rawdata, 61, 67)
    output[ , 24] <- substr(rawdata, 68, 74)
    output[ , 25] <- substr(rawdata, 75, 81)
    output[ , 26] <- substr(rawdata, 82, 85)
    names(output) <- c("Agency", "Groupnumber", "Plotnumber", 
                     "Measurementnumber", "MeasureYear", 
                     "MeasureMonth", "MeasureDay", 
                     "Legalsubdivision", "Section", 
                     "Township", "Range", "Meridian", 
                     "Plottreatment", "Imperial", "Recordtype", 
                     "Treenumber", "Species", 
                     "Regeneration_in_height_class1", 
                     "Regeneration_in_height_class2", 
                     "Regeneration_in_height_class3", 
                     "Regeneration_in_height_class4", 
                     "Regeneration_in_height_class5", 
                     "Treeplotsize", "Saplingplotsize", 
                     "Regenerationplotsize", 
                     "Establishmentyear")
    output <- output[,Recordtype := as.integer(Recordtype)][Recordtype==3,]
    output[,':='(Agency = as.integer(Agency), Groupnumber = as.integer(Groupnumber),
                 Plotnumber = as.integer(Plotnumber), 
                 Measurementnumber = as.integer(Measurementnumber), 
                 MeasureYear = as.integer(MeasureYear), 
                 MeasureMonth = as.integer(MeasureMonth), 
                 MeasureDay = as.integer(MeasureDay), 
                 Legalsubdivision = as.integer(Legalsubdivision), 
                 Section = as.integer(Section), 
                 Township = as.integer(Township), Range = as.integer(Range),
                 Meridian = as.integer(Meridian), 
                 Plottreatment = as.integer(Plottreatment), 
                 Imperial = as.logical(Imperial),
                 Recordtype = as.integer(Recordtype), 
                 Treenumber = as.integer(Treenumber), 
                 Species = as.character(Species), 
                 Regeneration_in_height_class1 = as.integer(Regeneration_in_height_class1), 
                 Regeneration_in_height_class2 = as.integer(Regeneration_in_height_class2), 
                 Regeneration_in_height_class3 = as.integer(Regeneration_in_height_class3), 
                 Regeneration_in_height_class4 = as.integer(Regeneration_in_height_class4), 
                 Regeneration_in_height_class5 = as.integer(Regeneration_in_height_class5), 
                 Treeplotsize = as.integer(Treeplotsize), 
                 Saplingplotsize = as.integer(Saplingplotsize), 
                 Regenerationplotsize = as.integer(Regenerationplotsize), 
                 Establishmentyear = as.integer(Establishmentyear))]
    return(output)
  })


setGeneric("obtainTreeDataABJuvenile", function(rawdata) {
  standardGeneric("obtainTreeDataABJuvenile")
})

#' @export
#' @rdname obtainTreeDataABJuvenile
setMethod(
  "obtainTreeDataABJuvenile",
  signature = c(rawdata = "data.frame"),
  definition = function(rawdata) {
    output <- data.table(matrix(nrow = nrow(rawdata), ncol = 28))
    rawdata <- as.character(rawdata[, 1])
    output[ , 1] <- substr(rawdata, 1, 5)
    output[ , 2] <- substr(rawdata, 6, 9)
    output[ , 3] <- substr(rawdata, 10, 11)
    output[ , 4] <- substr(rawdata, 12, 13)
    output[ , 5] <- substr(rawdata, 14, 17)
    output[ , 6] <- substr(rawdata, 18, 18)
    output[ , 7] <- substr(rawdata, 19, 20)
    output[ , 8] <- substr(rawdata, 21, 21)
    output[ , 9] <- substr(rawdata, 22, 25)
    output[ , 10] <- substr(rawdata, 26, 29)
    output[ , 11] <- substr(rawdata, 30, 33)
    output[ , 12] <- substr(rawdata, 34, 35)
    output[ , 13] <- substr(rawdata, 36, 37)
    output[ , 14] <- substr(rawdata, 38, 39)
    output[ , 15] <- substr(rawdata, 40, 43)
    output[ , 16] <- substr(rawdata, 44, 44)
    output[ , 17] <- substr(rawdata, 45, 47)
    output[ , 18] <- substr(rawdata, 48, 50)
    output[ , 19] <- substr(rawdata, 51, 53)
    output[ , 20] <- substr(rawdata, 54, 54)
    output[ , 21] <- substr(rawdata, 55, 55)
    output[ , 22] <- substr(rawdata, 56, 59)
    output[ , 23] <- substr(rawdata, 60, 60)
    output[ , 24] <- substr(rawdata, 61, 63)
    output[ , 25] <- substr(rawdata, 64, 66)
    output[ , 26] <- substr(rawdata, 67, 69)
    output[ , 27] <- substr(rawdata, 70, 72)
    output[ , 28] <- substr(rawdata, 73, 74)
    names(output)<-c("Plotnumber","MeasureYear",
                     "MeasureMonth","Recordtype",
                     "Treenumber","RegenPlot",
                     "Species", "Treetype",
                     "Birthyear","Height",
                     "DBH","Conditioncode1",
                     "Conditioncode2","Conditioncode3",
                     "LiveCrownHeight","CrownClass",
                     "Azimuth","Distance",
                     "Age","competitionHeight",
                     "competitionType", 
                     "RootCollarDiameter",
                     "CrownStat","CrownN",
                     "CrownW","CrownS",
                     "CrownE","PlotTreatmentType")
    output[,':='(Plotnumber = as.integer(Plotnumber), MeasureYear = as.integer(MeasureYear), 
                 MeasureMonth = as.integer(MeasureMonth), Recordtype = as.integer(Recordtype), 
                 Treenumber = as.integer(Treenumber), RegenPlot = as.integer(RegenPlot), 
                 Species = as.character(Species),  Treetype = as.character(Treetype), 
                 Birthyear = as.integer(Birthyear), Height = as.integer(Height), 
                 DBH = as.integer(DBH), Conditioncode1 = as.integer(Conditioncode1), 
                 Conditioncode2 = as.integer(Conditioncode2), Conditioncode3 = as.integer(Conditioncode3), 
                 LiveCrownHeight = as.integer(LiveCrownHeight), 
                 CrownClass = as.character(CrownClass), 
                 Azimuth = as.integer(Azimuth), Distance = as.integer(Distance), 
                 Age = as.integer(Age), competitionHeight = as.integer(competitionHeight), 
                 competitionType = as.integer(competitionType),  
                 RootCollarDiameter = as.integer(RootCollarDiameter), 
                 CrownStat = as.character(CrownStat), CrownN = as.integer(CrownN), 
                 CrownW = as.integer(CrownW), CrownS = as.integer(CrownS), 
                 CrownE = as.integer(CrownE),
                 PlotTreatmentType = as.integer(PlotTreatmentType))]
    return(output)
  })
    



