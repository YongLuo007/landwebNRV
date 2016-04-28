################################################################################
#' purify NWT TSP data
#' 
#' 
#' @param plotHeaderRaw  data table, This is the PLOT sheet from MS access file
#'
#' @param primarySAInfor  data table, This is SAMPLE_TREE sheet from MS access file
#' 
#' @param secondSAInfor data table, This is stand age information from the geodatabase NT_FORCOV_ATT.gdb
#'                                  SAinfor.csv was attribute table output from the geodatabase
#'                                  the linkage between treedatasummary and SAinfor is FC_ID
#'                                  the origin column contains the year when the stands originate
#' 
#' 
#' @param additionalLocationInfor data.table, This is plot locations that contained in VolumeSample.dbf in provided
#'                             VolumeSample shapefile. This is used as supplementary location infor to plotHeaderRaw
#'
#' @param treeDataRaw data.table, This is the TREE sheet in MS access file        
#'
#' @return  two data tables, the first one is plot header data, which contains the location and SA info.
#'                           the second one is purified tree data, which contains inividual tree infor.
#'                           for the tree data, all trees are alive.
#'                      
#' @importFrom data.table setnames setkey ':=' unique
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname dataPurification_NWTTSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_NWTTSP", function(plotHeaderRaw,
                                               primarySAInfor,
                                               secondSAInfor,
                                               additionalLocationInfor,
                                               treeDataRaw) {
  standardGeneric("dataPurification_NWTTSP")
})
#' @export
#' @rdname dataPurification_MBTSP
setMethod(
  "dataPurification_NWTTSP",
  signature = c(plotHeaderRaw = "data.table",
                primarySAInfor = "data.table",
                secondSAInfor = "data.table",
                additionalLocationInfor = "data.table",
                treeDataRaw = "data.table"),
  definition = function(plotHeaderRaw,
                        primarySAInfor,
                        secondSAInfor,
                        additionalLocationInfor,
                        treeDataRaw) {
    
  # these codes are for organize the sampling data for NWT
    plotHeaderRaw <- plotHeaderRaw[,.(PLOT_CODE, FMU, STAND, PROJECT_CODE, Year,
                                      SAMPLE, PLOT, PLOT_TYPE, SIZE, ID, Lat_DEC,
                                      Long_dec, LAT_DEG, LAT_MIN, LAT_SEC, LONG_DEG, LONG_MIN,
                                      LONG_SEC, ZONE, EASTING, NORTHING, DATUM, FORESTNO, NUMPLOTS,
                                      fc_id)]
    
    # remove plot without plot size infor
    plotHeaderRaw <- plotHeaderRaw[!is.na(SIZE),]

    # gathering location information in plotdata
    # the location information in this dataset can be found in three ways: 1 by Lat_DEC, Long_DEC
    # 2. by LAT_DEG and so on
    # 3. by UTM
    setnames(plotHeaderRaw,c("Lat_DEC","Long_dec"),c("Latitude","Longitude"))
    plotHeaderRaw[!is.na(LAT_DEG) & is.na(LAT_MIN),LAT_MIN:=0]
    plotHeaderRaw[!is.na(LAT_DEG) & is.na(LAT_SEC),LAT_SEC:=0]
    plotHeaderRaw[!is.na(LAT_DEG) & is.na(LONG_MIN),LONG_MIN:=0]
    plotHeaderRaw[!is.na(LAT_DEG) & is.na(LONG_SEC),LONG_SEC:=0]
    
    plotHeaderRaw[(is.na(Latitude) | Latitude==0) & (!is.na(LAT_DEG) & !is.na(LONG_DEG)),
             ':='(Latitude=LAT_DEG+LAT_MIN/60+LAT_SEC/3600,
                  Longitude=LONG_DEG+LONG_MIN/60+LONG_SEC/3600)][,':='(LONG_DEG=NULL,LONG_MIN=NULL,LONG_SEC=NULL,
                                                                       LAT_DEG=NULL,LAT_MIN=NULL,LAT_SEC=NULL)]
    
    # additional location file gives the additional locations that may not be provided by plotdata
    
    additionalLocationInfor <- additionalLocationInfor[LATITUDE!=0,.(PROJ_CODE,
                                                                         SAMPLE,PLOT,LATITUDE,LONGITUDE)]
    additionalLocationInfor$SAMPLE <- as.factor(additionalLocationInfor$SAMPLE)
    setnames(additionalLocationInfor,"PROJ_CODE","PROJECT_CODE")
    headerdata <- dplyr::left_join(plotHeaderRaw,additionalLocationInfor,by=c("PROJECT_CODE","SAMPLE","PLOT")) %>%
      data.table
    headerdata[(is.na(Latitude) | Latitude==0),':='(Latitude=LATITUDE,Longitude=LONGITUDE)]
    
    set(headerdata, ,c("LATITUDE", "LONGITUDE"), NULL)
    
    # get plots with locations either longlat or UTM
    headerdata <- headerdata[(!is.na(Latitude) & !is.na(Longitude) & Longitude != 0) |
                               (!is.na(ZONE) & !is.na(EASTING) & !is.na(NORTHING)),]
    headerdata[Longitude>0, Longitude:=-Longitude]
    # unique(headerdata$PLOT_TYPE)
    #[1] "F" "P" "L" "T" "C"
    # F is fixed radius plot
    # P is prism or variable radius plot
    # L is psp fixed radius
    # T is TSP
    # C is cummulative voloum # what is this????
    headerdata <- headerdata[PLOT_TYPE=="F" | PLOT_TYPE=="L",]
    
    # get SA infor
    primarySAInfor <- primarySAInfor[!is.na(AGE),]
    primarySAInfor <- setkey(primarySAInfor, TREE_CODE)[setkey(treeDataRaw[,.(TREE_CODE, PLOT_CODE)], TREE_CODE),
                                                        nomatch = 0]
    primarySAInfor[, baseSA:=as.integer(mean(AGE)), by = PLOT_CODE]
    primarySAInfor <- unique(primarySAInfor[,.(PLOT_CODE, baseSA)], by = "PLOT_CODE")
    headerdata <- dplyr::left_join(headerdata, primarySAInfor, by = "PLOT_CODE") %>%
      data.table
    headerdata[baseSA == 0, baseSA := NA]
    # add stand age information from the geodatabase NT_FORCOV_ATT.gdb
    # SAinfor.csv was attribute table output from the geodatabase
    # the linkage between treedatasummary and SAinfor is FC_ID
    # the origin column contains the year when the stands originate
   
    secondSAInfor <- data.table(secondSAInfor)[!is.na(ORIGIN) & ORIGIN!=0 & FC_ID!=0][,.(FC_ID,ORIGIN)]
    setnames(secondSAInfor, "FC_ID", "fc_id")
    
    headerdata <- dplyr::left_join(headerdata,secondSAInfor,by="fc_id") %>%
      data.table
    headerdata[is.na(baseSA),baseSA:=Year-ORIGIN]
    headerdata <- headerdata[!is.na(baseSA),]
    headerdata <- unique(headerdata, by = "PLOT_CODE")
    headerdata[,MeasureID:=paste("NWTTSP_", as.numeric(as.factor(PLOT_CODE)),
                                 sep = "")]
    headerdata <- headerdata[,.(MeasureID, OrigPlotID1 = PLOT_CODE, MeasureYear = Year, 
                                Longitude, Latitude, Zone = ZONE, Easting = EASTING,
                                Northing = NORTHING, PlotSize = SIZE/10000, baseYear = Year,
                                baseSA)]
    setnames(treeDataRaw, "PLOT_CODE", "OrigPlotID1")
    treeData <- treeDataRaw[OrigPlotID1 %in% unique(headerdata$OrigPlotID1),][
      ,.(OrigPlotID1, SPECIES, DBH, CALC_HT, TREE_ALT_KEY, TREE_CL)]
    # the link between treedata and plot data is PLOT_CODE
    treeData <- treeData[TREE_CL != 4, ][, TREE_CL:=NULL]
    treeData <- treeData[!is.na(DBH) & DBH != 0, ]
    treeData <- setkey(treeData, OrigPlotID1)[setkey(headerdata[,.(MeasureID, OrigPlotID1, MeasureYear)], OrigPlotID1),
                                              nomatch = 0]
    treeData <- treeData[,.(MeasureID, OrigPlotID1, OrigPlotID2 = NA, MeasureYear,
                            TreeNumber = TREE_ALT_KEY, Species = SPECIES,  DBH,
                            Height = CALC_HT)]
    headerdata <- headerdata[OrigPlotID1 %in% unique(treeData$OrigPlotID1),]
    return(list(plotHeaderData = headerdata,
                treeData = treeData))
  })
