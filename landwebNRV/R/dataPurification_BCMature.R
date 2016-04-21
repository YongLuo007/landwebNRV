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
            plotsizetimes = length(unique(area_pm))),
      by = SAMP_ID]
    # unique(headDataRaw$utmtimes)#1
    # unique(headDataRaw$eastingtimes)#1
    # unique(headDataRaw$northingtimes)# 1
    # unique(headDataRaw$SAtimes)
    # unique(headDataRaw$plotsizetimes)#1
    headDataRaw[, SA:=as.integer(mean(tot_stand_age)),
                by = SAMP_ID]
    
    # get the plots with locations
    headData <- headDataRaw[!is.na(utm_zone) & !is.na(utm_easting) & !is.na(utm_northing),]
    # get plots with plot size
    headData <- headData[!is.na(area_pm),][,.(SAMP_ID, )]
    
    
    
    

  })
