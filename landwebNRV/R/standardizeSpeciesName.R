################################################################################
#' Standardize species name from different forest inventory data, this function to make all
#' the species compatible to biomassCalculation function
#' 
#' 
#' @param speciesTable  data table. It must at least have one column species
#'
#' 
#' @param forestInventorySource,  Character string. Give the forest inventory data source             
#'        Currently support MBPSP, MBTSP, ABPSP, BCPSP, SKPSP, SKTSP and NFIPSP           
#'        
#'
#' @return  a data tables, the first one contains successfully standardized species.
#'          the newSpeciesName is the standardized name, unknown means the species in 
#'          the original species table can not be found according to manual
#'                      
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname standardizeSpeciesName
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("standardizeSpeciesName", function(speciesTable, forestInventorySource) {
  standardGeneric("standardizeSpeciesName")
})
#' @export
#' @rdname standardizeSpeciesName
setMethod(
  "standardizeSpeciesName",
  signature = c(speciesTable = "data.table", 
                forestInventorySource = "character"),
  definition = function(speciesTable, forestInventorySource) {
    if(forestInventorySource == "MBPSP" | forestInventorySource == "MBTSP"){
      speciesTable[species == "WP", newSpeciesName := "white pine"]
      speciesTable[species == "RP", newSpeciesName := "red pine"]
      speciesTable[species == "JP", newSpeciesName := "jack pine"]
      speciesTable[species == "SP", newSpeciesName := "scots pine"]
      speciesTable[species == "BS", newSpeciesName := "black spruce"]
      speciesTable[species == "WS", newSpeciesName := "white spruce"]
      speciesTable[species == "BF", newSpeciesName := "balsam fir"]
      speciesTable[species == "TL", newSpeciesName := "tamarack larch"]
      speciesTable[species == "EC", newSpeciesName := "eastern redcedar"]
      # assume cedar in original document is eastern redcedar
      speciesTable[species == "TA", newSpeciesName := "trembling aspen"]
      speciesTable[species == "LA", newSpeciesName := "largetooth aspen"]
      speciesTable[species == "BA", newSpeciesName := "balsam poplar"]
      speciesTable[species == "CO", newSpeciesName := "eastern cottonwood"]
      speciesTable[species == "W", newSpeciesName := "willow"]
      speciesTable[species == "WB", newSpeciesName := "white birch"]
      speciesTable[species == "B", newSpeciesName := "basswood"]
      speciesTable[species == "HB", newSpeciesName := "hackberry"]
      speciesTable[species == "MM", newSpeciesName := "manitoba maple"]
      speciesTable[species == "AS", newSpeciesName := "back ash"]
      # assume ash in original document is black ash
      speciesTable[species == "E", newSpeciesName := "white elm"]
      speciesTable[species == "HH", newSpeciesName := "hop-hornbeam"]
      speciesTable[species == "BO", newSpeciesName := "white oak"]
      # assume bur oak in original document is white oak
    } else if (forestInventorySource == "BCPSP"){
      speciesTable[species == "SB", newSpeciesName := "black spruce"]
      speciesTable[species == "LT", newSpeciesName := "tamarack larch"]
      speciesTable[species == "SW", newSpeciesName := "white spruce"]
      speciesTable[species == "EP", newSpeciesName := "white brich"]
      speciesTable[species == "PLI", newSpeciesName := "lodgepole pine"]
      speciesTable[species == "ACB", newSpeciesName := "balsam poplar"]
      speciesTable[species == "S", newSpeciesName := "black spruce"]
      # S for spruce in original doc. assume S is black spruce
      speciesTable[species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[species == "ACT", newSpeciesName := "black cottonwood"]
      speciesTable[species == "AT", newSpeciesName := "trembling aspen"]
      speciesTable[species == "E", newSpeciesName := "white birch"]
      # E for birch in original doc. assume S is white birch
      # speciesTable[species == "XC", newSpeciesName := "unknow conifer"]
      speciesTable[species == "BL", newSpeciesName := "alpine fir"]
      speciesTable[species == "EA", newSpeciesName := "white birch"]
      # EA is alaka paper birch, assume it is white birch here
      speciesTable[species == "AC", newSpeciesName := "balsam poplar"]
      # speciesTable[species == "X", newSpeciesName := "unknown"]
      speciesTable[species == "B", newSpeciesName := "fir"]
      speciesTable[species == "W", newSpeciesName := "willow"]
      speciesTable[species == "DR", newSpeciesName := "red alder"]
      speciesTable[species == "DM", newSpeciesName := "mountain alder"]
      speciesTable[species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[species == "ZH", newSpeciesName := "hardwood"]
      speciesTable[species == "SXW", newSpeciesName := "white spruce"]
      # SXW is hybrid between engelmann and white spruce
      speciesTable[species == "XH", newSpeciesName := "hardwood"]
      speciesTable[species == "SX", newSpeciesName := "white spruce"]
      # SX is hybrid
      speciesTable[species == "A", newSpeciesName := "trembling aspen"]
      # A is aspen cottonwood and poplar, assume it is trembling aspen
      speciesTable[species == "P", newSpeciesName := "lodgepole pine"]
      # P is pine species, assume it is lodgepole pine
      speciesTable[species == "MR", newSpeciesName := "red maple"]
      # MR could not find, assume it is red maple
      speciesTable[species == "SE", newSpeciesName := "engelmann spruce"]
      speciesTable[species == "FD", newSpeciesName := "douglas-fir"] 
      speciesTable[species == "BA", newSpeciesName := "amabalis fir"] 
      speciesTable[species == "CW", newSpeciesName := "western redcedar"]
      speciesTable[species == "HW", newSpeciesName := "western hemlock"]
      speciesTable[species == "FDI", newSpeciesName := "douglas-fir"]
      # speciesTable[species == "SXE", newSpeciesName := "unknown"]
      speciesTable[species == "D", newSpeciesName := "red alder"]
      # D is alder, assume red alder
      speciesTable[species == "H", newSpeciesName := "western hemlock"]
      speciesTable[species == "MV", newSpeciesName := "vine maple"] 
      speciesTable[species == "HM", newSpeciesName := "mountain hemlock"] 
      speciesTable[species == "EXP", newSpeciesName := "white birch"]
      speciesTable[species == "WS", newSpeciesName := "scoulers willow"]
      speciesTable[species == "AX", newSpeciesName := "trembling aspen"]
      # hybrid poplars for AX, assume trembling aspen
    } else if(forestInventorySource == "ABPSP"){
      # speciesTable[species == "  ", newSpeciesName := "unknown"]
      speciesTable[species == "AW", newSpeciesName := "trembling aspen"]
      speciesTable[species == "BW", newSpeciesName := "white birch"]
      speciesTable[species == "FA", newSpeciesName := "alpine fir"]
      speciesTable[species == "FB", newSpeciesName := "balsam fir"]
      speciesTable[species == "FD", newSpeciesName := "douglas-fir"]
      speciesTable[species == "LA", newSpeciesName := "alpine larch"]
      speciesTable[species == "LT", newSpeciesName := "tamarack larch"]
      # speciesTable[species == "P ", newSpeciesName := "unknown"]
      speciesTable[species == "PB", newSpeciesName := "balsam poplar"]
      speciesTable[species == "PF", newSpeciesName := "limber pine"]
      speciesTable[species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[species == "PW", newSpeciesName := "whitebark pine"]
      speciesTable[species == "SB", newSpeciesName := "black spruce"]
      speciesTable[species == "SE", newSpeciesName := "englemann spruce"]
      speciesTable[species == "SW", newSpeciesName := "white spruce"]
    } else if(forestInventorySource == "SKPSP" | forestInventorySource == "SKTSP"){
      # speciesTable[species == "", newSpeciesName := "unknown"]
      speciesTable[species == "BF", newSpeciesName := "balsam fir"]
      speciesTable[species == "BP", newSpeciesName := "balsam poplar"]
      speciesTable[species == "BS", newSpeciesName := "black spruce"]
      # speciesTable[species == "DC", newSpeciesName := "unknown"] # what is this
      # speciesTable[species == "DD", newSpeciesName := "unknown"] # what is this
      # speciesTable[species == "DU", newSpeciesName := "unknown"] # what is this
      speciesTable[species == "GA", newSpeciesName := "green ash"]
      speciesTable[species == "JP", newSpeciesName := "jack pine"]
      speciesTable[species == "MM", newSpeciesName := "manitoba maple"]
      # speciesTable[species == "PC", newSpeciesName := "unknown"] # what is this
      speciesTable[species == "TA", newSpeciesName := "trembling aspen"]
      speciesTable[species == "TL", newSpeciesName := "tamarack larch"]
      # speciesTable[species == "UI", newSpeciesName := "unknown"] # what is this
      speciesTable[species == "WB", newSpeciesName := "white birch"]
      speciesTable[species == "WE", newSpeciesName := "white elm"]
      speciesTable[species == "WS", newSpeciesName := "white spruce"]
      # speciesTable[species == "XX", newSpeciesName := "unknown"] # what is this
    } else if(forestInventorySource == "NWTTSP"){
      speciesTable[species == "A", newSpeciesName := "trembling aspen"]
      speciesTable[species == "BW", newSpeciesName := "white birch"]
      speciesTable[species == "Bw", newSpeciesName := "white birch"]
      speciesTable[species == "F", newSpeciesName := "balsam fir"]
      speciesTable[species == "FB", newSpeciesName := "balsam fir"]
      speciesTable[species == "L", newSpeciesName := "tamarack larch"]
      speciesTable[species == "LT", newSpeciesName := "tamarack larch"]
      speciesTable[species == "PB", newSpeciesName := "balsam poplar"]
      speciesTable[species == "Pj", newSpeciesName := "jack pine"]
      speciesTable[species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[species == "Pl", newSpeciesName := "lodgepole pine"]
      speciesTable[species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[species == "Po", newSpeciesName := "balsam poplar"]
      speciesTable[species == "PO", newSpeciesName := "balsam poplar"]
      speciesTable[species == "Sb", newSpeciesName := "black spruce"]
      speciesTable[species == "SB", newSpeciesName := "black spruce"]
      speciesTable[species == "sw", newSpeciesName := "white spruce"]
      speciesTable[species == "Sw", newSpeciesName := "white spruce"]
      speciesTable[species == "SW", newSpeciesName := "white spruce"]
      speciesTable[species == "W", newSpeciesName := "willow"]
    } else if(forestInventorySource == "NFIPSP"){
      # the species code system in NFI is different from provincial systems
      # the speciesTable for NFIPSP must have genus and species column 
      
      speciesTable[genus == "ABIE", species == "BAL",
                   newSpeciesName := "balsam fir"]
      speciesTable[genus == "ABIE", species == "LAS",
                   newSpeciesName := "subalpine fir"]
      speciesTable[genus == "ACER", species == "SPI",
                   newSpeciesName := "mountain maple"]
      speciesTable[genus == "ALNU", species == "SPP",
                   newSpeciesName := "alder"]
      speciesTable[genus == "ALNU", species == "INC",
                   newSpeciesName := "gray alder"]
      speciesTable[genus == "ALNU", species == "RUB",
                   newSpeciesName := "red alder"]
      speciesTable[genus == "ALNU", species == "CRI",
                   newSpeciesName := "alder"]
      speciesTable[genus == "ALNU", species == "VIR",
                   newSpeciesName := "sitka alder"]
      speciesTable[genus == "AMEL", species == "ALN",
                   newSpeciesName := "saskatoon-berry"]
      speciesTable[genus == "BETU", species == "GLA",
                   newSpeciesName := "birch"]
      
      speciesTable[genus == "BETU", species == "NAN",
                   newSpeciesName := "birch"] # not found
      speciesTable[genus == "BETU", species == "GLA",
                   newSpeciesName := "birch"] # not found
      speciesTable[genus == "BETU", species == "NEO",
                   newSpeciesName := "white birch"] # alaska paper birch
      speciesTable[genus == "BETU", species == "OCC",
                   newSpeciesName := "white birch"] # water birch
      speciesTable[genus == "BETU", species == "PAP",
                   newSpeciesName := "white birch"] 
      speciesTable[genus == "BETU", species == "PUM",
                   newSpeciesName := "birch"] # not found
      speciesTable[genus == "BETU", species == "SPP",
                   newSpeciesName := "birch"] # water birch
      speciesTable[genus == "CORN", species == "STO",
                   newSpeciesName := "redosier dogwood"] # water birch
      speciesTable[genus == "CORY", species == "COR",
                   newSpeciesName := "unknown"] # water birch
      speciesTable[genus == "FRAX", species == "PEN",
                   newSpeciesName := "red ash"] 
      speciesTable[genus == "GENC", species == "SPP",
                   newSpeciesName := "softwood"] 
      speciesTable[genus == "GENH", species == "SPP",
                   newSpeciesName := "hardwood"] 
      speciesTable[genus == "LARI", species == "LAR",
                   newSpeciesName := "tamarack larch"] 
      speciesTable[genus == "LARI", species == "OCC",
                   newSpeciesName := "western larch"] 
      speciesTable[genus == "PICE", species == "ENG",
                   newSpeciesName := "engelmann spruce"] 
      speciesTable[genus == "PICE", species == "GLA",
                   newSpeciesName := "white spruce"] 
      speciesTable[genus == "PICE", species == "MAR",
                   newSpeciesName := "black spruce"] 
      speciesTable[genus == "PICE", species == "SPP",
                   newSpeciesName := "spruce"] 
      speciesTable[genus == "PINU", species == "BAN",
                   newSpeciesName := "jack pine"] 
      speciesTable[genus == "PINU", species == "CON",
                   newSpeciesName := "lodgepole pine"] 
      speciesTable[genus == "POPU", species == "BAL",
                   newSpeciesName := "balsam poplar"] 
      speciesTable[genus == "POPU", species == "SPP",
                   newSpeciesName := "poplar"] 
      speciesTable[genus == "POPU", species == "TRE",
                   newSpeciesName := "trembling aspen"]
      speciesTable[genus == "PRUN", species == "PEN",
                   newSpeciesName := "pin cherry"]
      speciesTable[genus == "PRUN", species == "VIR",
                   newSpeciesName := "choke cherry"]
      speciesTable[genus == "QUER", species == "MAC",
                   newSpeciesName := "bur oak"]
      speciesTable[genus == "SALI", species == "PED",
                   newSpeciesName := "willow"] # not found assume willow
      speciesTable[genus == "SALI", species == "PLA",
                   newSpeciesName := "willow"] # not found assume willow
      speciesTable[genus == "SALI", species == "SCO",
                   newSpeciesName := "scouler willow"]
      speciesTable[genus == "SALI", species == "SPP",
                   newSpeciesName := "willow"]
      speciesTable[genus == "SALI", species == "SPP",
                   newSpeciesName := "willow"]
      speciesTable[genus == "SHEP", species == "CAN",
                   newSpeciesName := "berry"] # not found assume berry
      speciesTable[genus == "UNKN", species == "SPP",
                   newSpeciesName := "unknown"] # 
    } else {
      stop("Please define the correct forestInventorySource among ")
    }
    speciesTable[is.na(newSpeciesName), newSpeciesName := "unknown"]
    return(speciesTable)
  })







