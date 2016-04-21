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
    # get the plots with locations
    headDataRaw[,':='(utmtimes = length(unique(utm_zone)),
                      eastingtimes = length(unique(utm_easting)),
                      northingtimes = length(unique(utm_northing)),
                      SAtimes = length(unique(tot_stand_age)),
                      plotsizetimes = length(unique(area_pm))),
                by = SAMP_ID]
    # unique(headDataRaw$utmtimes)
    # unique(headDataRaw$eastingtimes)
    # unique(headDataRaw$northingtimes)
    # unique(headDataRaw$SAtimes)
    # unique(headDataRaw$plotsizetimes)
    unique(headDataRaw[SAtimes == 2,]$SAMP_ID)[1]
    headDataRaw[SAMP_ID == "3251_0012_VRI", ]
    
    headData <- headDataRaw[!is.na(utm_zone) & !is.na(utm_easting) & !is.na(utm_northing),]
    # get the plots with SA
    
    
    
    

  })
