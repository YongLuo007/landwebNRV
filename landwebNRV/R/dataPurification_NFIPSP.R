################################################################################
#' purify NFI PSP data
#' 
#' 
#' @param lgptreeRaw  data table, which is the raw tree data in the large tree plot, i.e., ltp_tree.csv
#'                  
#'                  
#' @param lgpHeaderRaw  data table, which is the large plot header data, i.e., ltp_header.csv 
#'
#' @param approxLocation  data table, which is approximate locations of the plots, ie., climate_approx_loc.csv.                                     
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
#' @rdname dataPurification_NFIPSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_NFIPSP", function(lgptreeRaw,
                                               lgpHeaderRaw,
                                               approxLocation) {
  standardGeneric("dataPurification_NFIPSP")
})
#' @export
#' @rdname dataPurification_NFIPSP
setMethod(
  "dataPurification_NFIPSP",
  signature = c(lgptreeRaw = "data.table",
                lgpHeaderRaw = "data.table",
                approxLocation = "data.table"),
  definition = function(lgptreeRaw,
                        lgpHeaderRaw,
                        approxLocation) {
    # start from tree data to obtain plot infor
    lgptreeRaw[, year:=as.numeric(substr(lgptreeRaw$meas_date, 1, 4))]
    lgpHeaderRaw[, year:=as.numeric(substr(lgpHeaderRaw$meas_date, 1, 4))]
    lgpHeaderRaw <- lgpHeaderRaw[nfi_plot %in% unique(lgptreeRaw$nfi_plot),][
      ,.(nfi_plot, year, meas_plot_size, site_age)]
    approxLocation <- approxLocation[,.(nfi_plot, longitude, latitude, elevation)] %>%
      unique(, by = "nfi_plot")
    lgpHeaderRaw <- setkey(lgpHeaderRaw, nfi_plot)[setkey(approxLocation, nfi_plot), nomatch = 0]
    treeData <- lgptreeRaw[,.(nfi_plot, year, tree_num, lgtree_genus, lgtree_species, 
                                lgtree_status, dbh, height)][nfi_plot %in% unique(lgpHeaderRaw$nfi_plot),]
    treeData <- treeData[lgtree_status != "DS" & lgtree_status != "M",][, lgtree_status:=NULL]
    setnames(treeData, c("nfi_plot", "year", "tree_num", "lgtree_genus", "lgtree_species", "dbh", "height"),
             c("PlotID", "MeasureYear", "treeNumber", "Genus", "Species", "DBH", "Height"))
    names(lgpHeaderRaw) <- c("PlotID", "MeasureYear", "PlotSize", "SA", "Longitude", "Latitude", "Elevation")
    return(list(plotHeaderData = lgpHeaderRaw,
                treeData = treeData))
  })
