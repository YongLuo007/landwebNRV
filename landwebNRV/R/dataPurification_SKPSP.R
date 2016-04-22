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
    SADataRaw[, baseYear := min(YEAR), by = PLOT_ID]
    SADataRaw[, treeAge := TOTAL_AGE-(YEAR-baseYear)]
    SADataRawDomSA <- SADataRaw[CROWN_CLASS == 1,] # the stand age first determined by dominant trees
    SADataRawDomSA[, NofTrees:=length(CROWN_CLASS), by  = PLOT_ID]
    # unique(SADataRawDomSA$NofTrees) # 1 2 3 4 5
    # stand age must determined by using at least 2 trees
    SADataRawDomSA <- SADataRawDomSA[NofTrees != 1,]
    # SADataRawDomSA[, treeAgeDif:=max(treeAge)-min(treeAge), by = PLOT_ID]
    # range(SADataRawDomSA$treeAgeDif) # 0 44
    # mean(SADataRawDomSA$treeAgeDif) # 7.03
    SADataRawDomSA[, baseSA:=as.integer(mean(treeAge)), by = PLOT_ID]
    SADataRawDomSA <- unique(SADataRawDomSA[,.(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
    # for the other plots determine SA using codominant trees
    SADataRawCodomSA <- SADataRaw[CROWN_CLASS == 2,]
    
    SADataRawCodomSA <- SADataRawCodomSA[!(PLOT_ID %in% unique(SADataRawDomSA$PLOT_ID)),]
    SADataRawCodomSA[, NofTrees:=length(CROWN_CLASS), by  = PLOT_ID]
    # unique(SADataRawCodomSA$NofTrees)
    SADataRawCodomSA <- SADataRawCodomSA[NofTrees != 1,]
    SADataRawCodomSA[, baseSA:=as.integer(mean(treeAge)), by = PLOT_ID]
    SADataRawCodomSA <- unique(SADataRawCodomSA[,.(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID") 
    headData_SA <- rbind(SADataRawDomSA, SADataRawCodomSA)
    
    headData_loca <- plotHeadRaw[PLOT_ID %in% unique(headData_SA$PLOT_ID),][
      ,.(PLOT_ID, Z13nad83_e, Z13nad83_n, Zone = 13)]
    names(headData_loca)[2:3] <- c("Easting", "Northing")
    headData_SALoca <- setkey(headData_SA, PLOT_ID)[setkey(headData_loca, PLOT_ID),
                                                    nomatch = 0]
    headData_PS <- measureHeadRaw[PLOT_ID %in% unique(headData_SALoca$PLOT_ID),][
      ,.(PLOT_ID, PLOT_SIZE)][!is.na(PLOT_SIZE),]
    headData_PS <- unique(headData_PS, by = "PLOT_ID")
    setnames(headData_PS, "PLOT_SIZE", "Plotsize")
    headData <- headData_SALoca[setkey(headData_PS, PLOT_ID), nomatch = 0]
    
    
    # for tree data
    treeDataRaw <- treeDataRaw[PLOT_ID %in% headData$PLOT_ID,][
      ,.(PLOT_ID, TREE_NO, YEAR, SPECIES, DBH, HEIGHT, CONDITION_CODE1,
         CONDITION_CODE2, CONDITION_CODE3, MORTALITY)]
    
    # choose living trees
    
    
    
    return(list(treeData = treeData, headData = headData))
  })
