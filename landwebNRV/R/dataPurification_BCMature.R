################################################################################
#' purify BC PSP data 
#' 
#' 
#' @param treeDataRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'
#' 
#' @param headDataRaw data.table, which is PSPDATA2015B, provided by AB
#'        
#'
#' @return  two data tables, the first one head data, which contains the location and SA info.
#'                           the second one is purified tree data, which contains inividual tree infor.
#'                           for the tree data, all trees are alive.
#'                      
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname dataPurification_BC
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_BC", function(treeDataRaw, headDataRaw) {
  standardGeneric("dataPurification_BC")
})
#' @export
#' @rdname dataPurification_BC
setMethod(
  "dataPurification_BC",
  signature = c(treeDataRaw = "data.table", 
                headDataRaw = "data.table"),
  definition = function(treeDataRaw, headDataRaw) {
    # purifying headData
    # like AB data, the BC PSP have two level heirachical design (SAMP_ID and plot)
    
    # get the plots with SA
    headDataRaw <- headDataRaw[tot_stand_age != -99,][
      ,':='(utmtimes = length(unique(utm_zone)),
            eastingtimes = length(unique(utm_easting)),
            northingtimes = length(unique(utm_northing)),
            SAtimes = length(unique(tot_stand_age)),
            plotsizetimes = length(unique(area_pm)),
            standorigtimes = length(unique(stnd_org)),
            treatmenttimes = length(unique(treatment))),
      by = SAMP_ID]
    # unique(headDataRaw$utmtimes)#1
    # unique(headDataRaw$eastingtimes)#1
    # unique(headDataRaw$northingtimes)# 1
    # unique(headDataRaw$SAtimes) # no NA
    # unique(headDataRaw$plotsizetimes)#1
    # unique(headDataRaw$standorigtimes) # 1
    
    
    # select the natural originated and untreated  
    headDataRaw <- headDataRaw[stnd_org == "N" | stnd_org == "F",]
                               
    headDataRaw[,treatmenttimes := length(unique(treatment)), by = c("SAMP_ID", "meas_yr")]
    # unique(headDataRaw$treatmenttimes) # 1 2
    headDataRaw <- headDataRaw[treatmenttimes == 1 & treatment == "UNTREATED", .(SAMP_ID, no_plots, utm_zone, utm_easting,
                 utm_northing, area_pm, tot_stand_age,
                 meas_yr)]
    

    headDataRaw[, baseYear := min(meas_yr),
                by = SAMP_ID]
    headDataRaw[, baseSA := as.integer(tot_stand_age-(meas_yr-baseYear))]
    # get the plots with locations
    headData <- headDataRaw[!is.na(utm_zone) & !is.na(utm_easting) & !is.na(utm_northing),]
    # get plots with plot size
    headData <- headData[!is.na(area_pm),][,':='(tot_stand_age = NULL)]
    names(headData)[1:7] <- c("SampleID", "Nofplot", "Zone", "Easting",
                              "Northing", "Plotsize", "Measureyear")
    headData <- unique(headData, by = c("SampleID", "Measureyear"))
    
    smpIDbyYear <- headData[,.(SampleID, Measureyear)]
    
    # for tree data
    setnames(treeDataRaw, c("SAMP_ID", "plot_no"),
             c("SampleID", "Plotnumber"))
    treeData <- treeDataRaw[SampleID %in% unique(headData$SampleID),]
    # range(treeData$meas_yr) # 1926 2014
    # unique(treeData$Plotnumber) # 01 02 03
    treeData <- treeData[sub_plot_tree == "N",][, Plotnumber := as.numeric(Plotnumber)]
    treeData <- treeData[tree_cls == 1 | tree_cls == 2,.(SampleID, Plotnumber, meas_yr, tree_no, species,
                            dbh, height)]
    # unique(treeData$ld)
    names(treeData)[3:7] <- c("Measureyear", "Treenumber", "Species", "DBH", "Height")
    treeData <- setkey(treeData, SampleID, Measureyear)[setkey(smpIDbyYear, SampleID, Measureyear), 
                                                        nomatch = 0]
    headDataNew <- unique(treeData[,.(SampleID, Plotnumber)], by = c("SampleID", "Plotnumber"))
    headDataNew[,PlotID:=as.numeric(row.names(headDataNew))]
    treeData <- setkey(treeData, SampleID, Plotnumber)[setkey(headDataNew, SampleID, Plotnumber),
                                                      nomatch = 0]
    
    headDataAdded <- unique(headData, by = c("SampleID"))
    headData <- setkey(headDataNew, SampleID)[setkey(headDataAdded, SampleID), nomatch=0]
    return(list(treeData = treeData, headData = headData))
})
