################################################################################
#' purify SK PSP data 
#' 
#' 
#' @param SADataRaw  data table, is age_samples in the MS access file
#' 
#' 
#' @param plotHeadRaw data.table, is plot_header in MS access file
#' 
#' @param measureHeadRaw data.table, is mearsurement_header in MS access file
#'        
#' @param treeDataRaw data.table, is trees in MS access file
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
#' @rdname dataPurification_SKPSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_SKPSP", function(SADataRaw, plotHeadRaw,
                                              measureHeadRaw, treeDataRaw) {
  standardGeneric("dataPurification_SKPSP")
})
#' @export
#' @rdname dataPurification_SKPSP
setMethod(
  "dataPurification_SKPSP",
  signature = c(SADataRaw = "data.table", 
                plotHeadRaw = "data.table",
                measureHeadRaw = "data.table",
                treeDataRaw = "data.table"),
  definition = function(SADataRaw, plotHeadRaw,
                        measureHeadRaw, treeDataRaw) {
    rm(list=ls())
    load("SKPSP.RData")
    SADataRaw <- plotheader1
    plotHeadRaw <- plotheader3
    measureHeadRaw <- plotheader2
    treeDataRaw <- treedata
    # range(SADataRaw$COUNTED_AGE) # NA NA
    # range(SADataRaw$TOTAL_AGE) # NA NA
    SADataRaw <- SADataRaw[!is.na(TOTAL_AGE),]
    SADataRawDomSA <- SADataRaw[CROWN_CLASS == 1,] # the stand age first determined by dominant trees
    
    
    
    
        
    # SA1_3 <- SADataRaw[BORED_HEIGHT == 1.3 & !is.na(COUNTED_AGE),]
    # unique(SA1_3$TOTAL_AGE) # NA  there is no empirical age difference between 1.3 counted and total age
    # the total age for these trees are corrected by 7+counted age (chen at al.)
    # SADataRaw[is.na(TOTAL_AGE), TOTAL_AGE := as.numeric(COUNTED_AGE)+7]
    # range(SADataRaw$TOTAL_AGE) # 6 214
    SADataRaw[, baseYear:=min(YEAR), by = PLOT_ID]
    SADataRaw[, SA:=TOTAL_AGE-(YEAR-baseYear)]
    SADataRaw[, SAdif:=max(SA)-min(SA), by = PLOT_ID]
    range(SADataRaw$SAdif) # 0 127 that is too much difference within one plot
    # SADataRaw[SAdif == 110,]$SA
    
    
    
    
    
    return(list(treeData = treeData, headData = headData))
  })
