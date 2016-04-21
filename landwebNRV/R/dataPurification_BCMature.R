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
#' @rdname dataPurification_BCMature
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_BCMature", function(treeDataRaw, headDataRaw) {
  standardGeneric("dataPurification_BCMature")
})
#' @export
#' @rdname dataPurification_BCMature
setMethod(
  "dataPurification_BCMature",
  signature = c(treeDataRaw = "data.table", 
                headDataRaw = "data.table"),
  definition = function(treeDataRaw, headDataRaw) {
    
    
    
    
    

  })
