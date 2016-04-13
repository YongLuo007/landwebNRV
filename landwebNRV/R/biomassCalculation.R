################################################################################
#' this function is to calculate aboveground biomass for boreal species based on DBH
#' or DBH and Height
#' 
#' @param species  Character string. The species name.
#'
#' @param DBH  Numeric. The tree's diameter at breast height (DBH, cm).
#' 
#' @param heightIncluded  Logical. Whether the biomass is calculated based on DBH and height.
#'        If TURE, height must be provided.
#'        Default \code{FALSE}
#' 
#' @param height  Numeric. The tree's height (m).
#' 
#' @param paperSource  Character. Determine the sources of equations. Currently, this functions has two
#'        options, i.e., "Lambert2005" and "Ung2008".
#'        Default \code{Lambert2005}
#'
#' @return Biomass (kg) and missedSpecies list that was not calculated.
#'
#' @note no note
#'
#' @seealso no
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname biomassCalculation
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#'  DBH <- seq(1, 100, 5)
#'  species <- c(rep("jack pine", 10), rep("black spruce", 10))
#'  species[1] <- "wrongSpecies"
#'  height <- seq(20, 40, length = 20)
#'  biomass1 <- biomassCalculation(species = species, DBH = DBH) # without height information and
#'                                                              # and taking the eqations from Lambert 2005
#'                                                              
#'  biomass2 <- biomassCalculation(species = species, DBH = DBH, heightIncluded = TRUE, height = height)
#'                                                              # with height information and
#'                                                              # and taking the eqations from Lambert 2005
#' }
setGeneric("biomassCalculation", function(species, DBH, heightIncluded, 
                                          height, paperSource) {
  standardGeneric("biomassCalculation")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "logical",
                height = "numeric", paperSource = "character"),
  definition = function(species, DBH, heightIncluded, 
                        height, paperSource) {
    if(paperSource == "Lambert2005"){
      # the below parameters are from Table 3 in Lambert 2005
      if(!heightIncluded){
        browser()
        tempdatatable <- data.table::data.table(species = species, DBH = DBH)
        uniqueSpecies <- unique(species)
        for (individualSpecies in uniqueSpecies){
          if(individualSpecies == "alpine fir"){
            wood1 <- 0.0528
            wood2 <- 2.4309
            bark1 <- 0.0108
            bark2 <- 2.3876
            branches1 <- 0.0121
            branches2 <- 2.3519
            foliage1 <- 0.0251
            foliage2 <- 2.0389
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "balsam fir"){
            wood1 <- 0.0534
            wood2 <- 2.4030
            bark1 <- 0.0115
            bark2 <- 2.3484
            branches1 <- 0.0070
            branches2 <- 2.5406
            foliage1 <- 0.0840
            foliage2 <- 1.6695
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "balsam poplar"){
            wood1 <- 0.0510
            wood2 <- 2.4529
            bark1 <- 0.0297
            bark2 <- 2.1131
            branches1 <- 0.0120
            branches2 <- 2.4165
            foliage1 <- 0.0276
            foliage2 <- 1.6215
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
            
          } else if (individualSpecies == "basswood"){
            wood1 = 0.0562
            wood2 <- 2.4102
            bark1 <- 0.0302
            bark2 <- 2.0976
            branches1 <- 0.0230
            branches2 <- 2.2382
            foliage1 <- 0.0288
            foliage2 <- 1.6378
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if(individualSpecies == "black ash"){
            wood1 <- 0.0941
            wood2 <- 2.3491
            bark1 <- 0.0323
            bark2 <- 2.0761
            branches1 <- 0.0448
            branches2 <- 1.9771
            foliage1 <- 0.0538
            foliage2 <- 1.3584
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "black cherry"){
            wood1 <- 0.3743
            wood2 <- 1.9406
            bark1 <- 0.0679
            bark2 <- 1.8377
            branches1 <- 0.0796
            branches2 <- 2.0103
            foliage1 <- 0.0840
            foliage2 <- 1.2319
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "black spruce"){
            wood1 <- 0.0477
            wood2 <- 2.5147
            bark1 <- 0.0153
            bark2 <- 2.2429
            branches1 <- 0.0278
            branches2 <- 2.0839
            foliage1 <- 0.1648
            foliage2 <- 1.4143
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "eastern hemlock"){
            wood1 <- 0.0619
            wood2 <- 2.3821
            bark1 <- 0.0139
            bark2 <- 2.3282
            branches1 <- 0.0217
            branches2 <- 2.2653
            foliage1 <- 0.0776
            foliage2 <- 1.6995
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "eastern redcedar"){
            wood1 <- 0.1277
            wood2 <- 1.9778
            bark1 <- 0.0377
            bark2 <- 1.6064
            branches1 <- 0.0254
            branches2 <- 2.2884
            foliage1 <- 0.0550
            foliage2 <- 1.8656
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "eastern whitecedar"){
            wood1 <- 0.0654
            wood2 <- 2.2121
            bark1 <- 0.0114
            bark2 <- 2.1432
            branches1 <- 0.0335
            branches2 <- 1.9367
            foliage1 <- 0.0499
            foliage2 <- 1.7278
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "eastern whitepine"){
            wood1 <- 0.0997
            wood2 <- 2.2709
            bark1 <- 0.0192
            bark2 <- 2.2038
            branches1 <- 0.0056
            branches2 <- 2.6011
            foliage1 <- 0.0284
            foliage2 <- 1.9375
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "grey birch"){
            wood1 <- 0.0720
            wood2 <- 2.3885
            bark1 <- 0.0168
            bark2 <- 2.2569
            branches1 <- 0.0088
            branches2 <- 2.5689
            foliage1 <- 0.0099
            foliage2 <- 1.8985
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "hickory"){
            wood1 <- 0.2116
            wood2 <- 2.2013
            bark1 <- 0.0365
            bark2 <- 2.1133
            branches1 <- 0.0087
            branches2 <- 2.8927
            foliage1 <- 0.0173
            foliage2 <- 1.9830
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "hop-hornbeam"){
            wood1 <- 0.1929
            wood2 <- 1.9672
            bark1 <- 0.0671
            bark2 <- 1.5911
            branches1 <- 0.0278
            branches2 <- 2.1336
            foliage1 <- 0.0293
            foliage2 <- 1.9502
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "jack pine"){
            wood1 <- 0.0804
            wood2 <- 2.4041
            bark1 <- 0.0184
            bark2 <- 2.0703
            branches1 <- 0.0079
            branches2 <- 2.4155
            foliage1 <- 0.0389
            foliage2 <- 1.7290
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "largetooth aspen"){
            wood1 <- 0.0959
            wood2 <- 2.3430
            bark1 <- 0.0308
            bark2 <- 2.2240
            branches1 <- 0.0047
            branches2 <- 2.6530
            foliage1 <- 0.0080
            foliage2 <- 2.0149
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "lodgepole pine"){
            wood1 <- 0.0475
            wood2 <- 2.5437
            bark1 <- 0.0186
            bark2 <- 2.0807
            branches1 <- 0.0198
            branches2 <- 2.1287
            foliage1 <- 0.0432
            foliage2 <- 1.7166
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "lodgepole pine"){
            wood1 <- 0.0475
            wood2 <- 2.5437
            bark1 <- 0.0186
            bark2 <- 2.0807
            branches1 <- 0.0198
            branches2 <- 2.1287
            foliage1 <- 0.0432
            foliage2 <- 1.7166
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if(individualSpecies == "red ash"){
            wood1 <- 0.1571
            wood2 <- 2.1817
            bark1 <- 0.0416
            bark2 <- 2.0509
            branches1 <- 0.0177
            branches2 <- 2.3370
            foliage1 <- 0.1041
            foliage2 <- 1.2185
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "red maple"){
            wood1 <- 0.1014
            wood2 <- 2.3448
            bark1 <- 0.0291
            bark2 <- 2.0893
            branches1 <- 0.0175
            branches2 <- 2.4846
            foliage1 <- 0.0515
            foliage2 <- 1.5198
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "red oak"){
            wood1 <- 0.1754
            wood2 <- 2.1616
            bark1 <- 0.0381
            bark2 <- 2.0991
            branches1 <- 0.0085
            branches2 <- 2.7790
            foliage1 <- 0.0373
            foliage2 <- 1.6740
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "red pine"){
            wood1 <- 0.0564
            wood2 <- 2.4465
            bark1 <- 0.0188
            bark2 <- 2.0527
            branches1 <- 0.0033
            branches2 <- 2.7515
            foliage1 <- 0.0212
            foliage2 <- 2.0690
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "red spruce"){
            wood1 <- 0.0989
            wood2 <- 2.2814
            bark1 <- 0.0220
            bark2 <- 2.0908
            branches1 <- 0.0005
            branches2 <- 3.2750
            foliage1 <- 0.0066
            foliage2 <- 2.4213
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "silver maple"){
            wood1 <- 0.2324
            wood2 <- 2.1000
            bark1 <- 0.0278
            bark2 <- 2.0433
            branches1 <- 0.0028
            branches2 <- 3.1020
            foliage1 <- 0.1430
            foliage2 <- 1.2580
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "sugar maple"){
            wood1 <- 0.1315
            wood2 <- 2.3129
            bark1 <- 0.0631
            bark2 <- 1.9241
            branches1 <- 0.0330
            branches2 <- 2.3741
            foliage1 <- 0.0393
            foliage2 <- 1.6930
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "tamarack larch"){
            wood1 <- 0.0625
            wood2 <- 2.4475
            bark1 <- 0.0174
            bark2 <- 2.1109
            branches1 <- 0.0196
            branches2 <- 2.2652
            foliage1 <- 0.0801
            foliage2 <- 1.4875
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "trembling aspen"){
            wood1 <- 0.0605
            wood2 <- 2.4750
            bark1 <- 0.0168
            bark2 <- 2.3949
            branches1 <- 0.0080
            branches2 <- 2.5214
            foliage1 <- 0.0261
            foliage2 <- 1.6304
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "white ash"){
            wood1 <- 0.1861
            wood2 <- 2.1665
            bark1 <- 0.0406
            bark2 <- 1.9946
            branches1 <- 0.0461
            branches2 <- 2.2291
            foliage1 <- 0.1106
            foliage2 <- 1.2277
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "white birch"){
            wood1 <- 0.0593
            wood2 <- 2.5026
            bark1 <- 0.0135
            bark2 <- 2.4053
            branches1 <- 0.0135
            branches2 <- 2.5532
            foliage1 <- 0.0546
            foliage2 <- 1.6351
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "white elm"){
            wood1 <- 0.0402
            wood2 <- 2.5804
            bark1 <- 0.0073
            bark2 <- 2.4859
            branches1 <- 0.0401
            branches2 <- 2.1826
            foliage1 <- 0.0750
            foliage2 <- 1.3436
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "white spruce"){
            wood1 <- 0.0359
            wood2 <- 2.5775
            bark1 <- 0.0116
            bark2 <- 2.3022
            branches1 <- 0.0283
            branches2 <- 2.0823
            foliage1 <- 0.1601
            foliage2 <- 1.4670
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "yellow birch"){
            wood1 <- 0.1932
            wood2 <- 2.1569
            bark1 <- 0.0192
            bark2 <- 2.2475
            branches1 <- 0.0305
            branches2 <- 2.4044
            foliage1 <- 0.1119
            foliage2 <- 1.3973
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "hardwood"){
            wood1 <- 0.0871
            wood2 <- 2.3702
            bark1 <- 0.0241
            bark2 <- 2.1969
            branches1 <- 0.0167
            branches2 <- 2.4807
            foliage1 <- 0.0390
            foliage2 <- 1.6229
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else if (individualSpecies == "softwood"){
            wood1 <- 0.0648
            wood2 <- 2.3927
            bark1 <- 0.0162
            bark2 <- 2.1959
            branches1 <- 0.0156
            branches2 <- 2.2916
            foliage1 <- 0.0861
            foliage2 <- 1.6261
            tempdatatable[species == individualSpecies,
                          biomass := wood1*DBH^wood2+
                            bark1*DBH^bark2+
                            foliage1*DBH^foliage2+
                            branches1*DBH^branches2]
          } else {
            tempdatatable[species == individualSpecies,
                          biomass := 0]
          }
        }
        
      } else {
        tempdatatable <- data.table::data.table(species = species,
                                                DBH = DBH,
                                                height = height)
        uniqueSpecies <- unique(species)
        for (individualSpecies in uniqueSpecies){
          if(individualSpecies == "alpine fir"){
            wood1 <- 0.0268
            wood2 <- 1.7579
            wood3 <- 0.9871
            bark1 <- 0.0009
            bark2 <- 1.4460
            bark3 <- 1.8839
            branches1 <- 0.0470
            branches2 <- 2.9288
            branches3 <- -1.1588
            foliage1 <- 0.0551
            foliage2 <- 1.7585
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "balsam fir"){
            wood1 <- 0.0294
            wood2 <- 1.8357
            wood3 <- 0.8640
            bark1 <- 0.0053
            bark2 <- 2.0876
            bark3 <- 0.5842
            branches1 <- 0.0117
            branches2 <- 3.5097
            branches3 <- -1.3006
            foliage1 <- 0.1245
            foliage2 <- 2.5230
            foliage3 <- -1.1230
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "balsam poplar"){
            wood1 <- 0.0117
            wood2 <- 1.7757
            wood3 <- 1.2555
            bark1 <- 0.0180
            bark2 <- 1.8131
            bark3 <- 0.5144
            branches1 <- 0.0112
            branches2 <- 3.0861
            branches3 <- -0.7164
            foliage1 <- 0.0617
            foliage2 <- 1.8615
            foliage3 <- -0.5375
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "basswood"){
            wood1 <- 0.0168
            wood2 <- 1.9844
            wood3 <- 0.8989
            bark1 <- 0.0057
            bark2 <- 1.5881
            bark3 <- 1.1472
            branches1 <- 0.0039
            branches2 <- 2.0084
            branches3 <- 0.8588
            foliage1 <- 0.0147
            foliage2 <- 1.8300
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "beech"){
            wood1 <- 0.0432
            wood2 <- 2.0378
            wood3 <- 0.7000
            bark1 <- 0.0049
            bark2 <- 1.9057
            bark3 <- 0.6770
            branches1 <- 0.0355
            branches2 <- 2.3749
            branches3 <- 0
            foliage1 <- 0.0452
            foliage2 <- 1.5567
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "black ash"){
            wood1 <- 0.0306
            wood2 <- 2.1836
            wood3 <- 0.5740
            bark1 <- 0.0897
            bark2 <- 2.2634
            bark3 <- -0.5670
            branches1 <- 0.0994
            branches2 <- 2.1630
            branches3 <- -0.4809
            foliage1 <- 0.0124
            foliage2 <- 1.0325
            foliage3 <- 0.8747
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "black cherry"){
            wood1 <- 0.0181
            wood2 <- 1.7013
            wood3 <- 1.3057
            bark1 <- 0.0101
            bark2 <- 1.5956
            bark3 <- 0.9190
            branches1 <- 0.0005
            branches2 <- 2.8004
            branches3 <- 0.8603
            foliage1 <- 0.1976
            foliage2 <- 1.4421
            foliage3 <- -0.5264
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "black spruce"){
            wood1 <- 0.0309
            wood2 <- 1.7527
            wood3 <- 1.0014
            bark1 <- 0.0115
            bark2 <- 1.7405
            bark3 <- 0.6589
            branches1 <- 0.0380
            branches2 <- 3.2558
            branches3 <- -1.4218
            foliage1 <- 0.2048
            foliage2 <- 2.5754
            foliage3 <- -1.3704
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "eastern hemlock"){
            wood1 <- 0.0257
            wood2 <- 1.9277
            wood3 <- 0.8576
            bark1 <- 0.0118
            bark2 <- 1.9893
            bark3 <- 0.4700
            branches1 <- 0.0215
            branches2 <- 2.6553
            branches3 <- -0.4682
            foliage1 <- 0.1471
            foliage2 <- 2.0108
            foliage3 <- -0.6080
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "eastern redcedar"){
            wood1 <- 0.0520
            wood2 <- 1.7731
            wood3 <- 0.7054
            bark1 <- 0.0283
            bark2 <- 1.7079
            bark3 <- 0
            branches1 <- 0.0219
            branches2 <- 2.3585
            branches3 <- 0
            foliage1 <- 0.2575
            foliage2 <- 2.5136
            foliage3 <- -1.5565
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "eastern whitecedar"){
            wood1 <- 0.0295
            wood2 <- 1.7026
            wood3 <- 0.9428
            bark1 <- 0.0076
            bark2 <- 1.7861
            bark3 <- 0.6132
            branches1 <- 0.0501
            branches2 <- 2.5165
            branches3 <- -0.8774
            foliage1 <- 0.0813
            foliage2 <- 2.2180
            foliage3 <- -0.7907
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "eastern whitepine"){
            wood1 <- 0.0170
            wood2 <- 1.7779
            wood3 <- 1.1370
            bark1 <- 0.0069
            bark2 <- 1.6589
            bark3 <- 0.9582
            branches1 <- 0.0184
            branches2 <- 3.1968
            branches3 <- -1.0876
            foliage1 <- 0.0584
            foliage2 <- 2.2389
            foliage3 <- -0.5968
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "grey birch"){
            wood1 <- 0.0295
            wood2 <- 1.9064
            wood3 <- 0.9139
            bark1 <- 0.0148
            bark2 <- 1.8433
            bark3 <- 0.5021
            branches1 <- 0.0150
            branches2 <- 3.0347
            branches3 <- -0.7629
            foliage1 <- 0.0455
            foliage2 <- 2.6447
            foliage3 <- -1.4955
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "hickory"){
            wood1 <- 0.0139
            wood2 <- 1.5913
            wood3 <- 1.5080
            bark1 <- 0.0081
            bark2 <- 1.4943
            bark3 <- 1.1324
            branches1 <- 0.0050
            branches2 <- 3.0463
            branches3 <- 0
            foliage1 <- 0.0121
            foliage2 <- 2.0865
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "hop-hornbeam"){
            wood1 <- 0.0083
            wood2 <- 1.6534
            wood3 <- 1.7479
            bark1 <- 0.0012
            bark2 <- 1.1486
            bark3 <- 2.2903
            branches1 <- 0.0009
            branches2 <- 1.9152
            branches3 <- 1.7769
            foliage1 <- 0.0247
            foliage2 <- 2.0056
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "jack pine"){
            wood1 <- 0.0199
            wood2 <- 1.6883
            wood3 <- 1.2456
            bark1 <- 0.0141
            bark2 <- 1.5994
            bark3 <- 0.5957
            branches1 <- 0.0185
            branches2 <- 3.0584
            branches3 <- -0.9816
            foliage1 <- 0.0325
            foliage2 <- 1.7879
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "largetooth aspen"){
            wood1 <- 0.0128
            wood2 <- 2.0633
            wood3 <- 0.9516
            bark1 <- 0.0240
            bark2 <- 2.3055
            bark3 <- 0
            branches1 <- 0.0131
            branches2 <- 3.1274
            branches3 <- -0.8379
            foliage1 <- 0.0382
            foliage2 <- 2.1673
            foliage3 <- -0.6842
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "lodgepole pine"){
            wood1 <- 0.0202
            wood2 <- 1.7179
            wood3 <- 1.2078
            bark1 <- 0.0099
            bark2 <- 1.6049
            bark3 <- 0.7456
            branches1 <- 0.0440
            branches2 <- 3.7190
            branches3 <- -2.0399
            foliage1 <- 0.0785
            foliage2 <- 2.5377
            foliage3 <- -1.1213
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "red ash"){
            wood1 <- 0.0224
            wood2 <- 1.7845
            wood3 <- 1.0600
            bark1 <- 0.0219
            bark2 <- 1.4190
            bark3 <- 0.8963
            branches1 <- 0.0176
            branches2 <- 2.3313
            branches3 <- 0
            foliage1 <- 0.0761
            foliage2 <- 1.3077
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "red maple"){
            wood1 <- 0.0315
            wood2 <- 2.0342
            wood3 <- 0.7485
            bark1 <- 0.0283
            bark2 <- 2.0907
            bark3 <- 0
            branches1 <- 0.0225
            branches2 <- 2.4106
            branches3 <- 0
            foliage1 <- 0.0571
            foliage2 <- 1.4898
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "red oak"){
            wood1 <- 0.0285
            wood2 <- 1.8501
            wood3 <- 1.0204
            bark1 <- 0.0326
            bark2 <- 1.8100
            bark3 <- 0.4153
            branches1 <- 0.0013
            branches2 <- 3.0637
            branches3 <- 0.3153
            foliage1 <- 0.0582
            foliage2 <- 1.5438
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "red pine"){
            wood1 <- 0.0106
            wood2 <- 1.7725
            wood3 <- 1.3285
            bark1 <- 0.0277
            bark2 <- 1.5192
            bark3 <- 0.4645
            branches1 <- 0.0125
            branches2 <- 3.3865
            branches3 <- -1.1939
            foliage1 <- 0.0731
            foliage2 <- 2.3439
            foliage3 <- -0.7378
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "red spruce"){
            wood1 <- 0.0143
            wood2 <- 1.6441
            wood3 <- 1.4065
            bark1 <- 0.0274
            bark2 <- 2.0188
            bark3 <- 0
            branches1 <- 0.0005
            branches2 <- 3.3136
            branches3 <- 0
            foliage1 <- 0.0106
            foliage2 <- 2.2709
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "silver maple"){
            wood1 <- 0.0274
            wood2 <- 1.7126
            wood3 <- 1.1086
            bark1 <- 0.0123
            bark2 <- 1.8250
            bark3 <- 0.5010
            branches1 <- 0.0543
            branches2 <- 3.7343
            branches3 <- -1.6497
            foliage1 <- 6.6808
            foliage2 <- 2.1092
            foliage3 <- -2.1697
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "sugar maple"){
            wood1 <- 0.0301
            wood2 <- 2.0313
            wood3 <- 0.8171
            bark1 <- 0.0103
            bark2 <- 1.7111
            bark3 <- 0.8509
            branches1 <- 0.0661
            branches2 <- 2.5940
            branches3 <- -0.4933
            foliage1 <- 2.5019
            foliage2 <- 2.4527
            foliage3 <- -2.3008
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "tamarack larch"){
            wood1 <- 0.0276
            wood2 <- 1.6724
            wood3 <- 1.1443
            bark1 <- 0.0120
            bark2 <- 1.7059
            bark3 <- 0.5811
            branches1 <- 0.0336
            branches2 <- 3.1335
            branches3 <- -1.1559
            foliage1 <- 0.1324
            foliage2 <- 2.1140
            foliage3 <- -0.8781
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "trembling aspen"){
            wood1 <- 0.0142
            wood2 <- 1.9389
            wood3 <- 1.0572
            bark1 <- 0.0063
            bark2 <- 2.0819
            bark3 <- 0.6617
            branches1 <- 0.0137
            branches2 <- 2.9270
            branches3 <- -0.6221
            foliage1 <- 0.0270
            foliage2 <- 1.6183
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "white ash"){
            wood1 <- 0.0224
            wood2 <- 1.7438
            wood3 <- 1.1899
            bark1 <- 0.0126
            bark2 <- 1.6456
            bark3 <- 0.7893
            branches1 <- 0.0354
            branches2 <- 2.3046
            branches3 <- 0
            foliage1 <- 0.0195
            foliage2 <- 1.0509
            foliage3 <- 0.7836
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "white birch"){
            wood1 <- 0.0338
            wood2 <- 2.0702
            wood3 <- 0.6876
            bark1 <- 0.0080
            bark2 <- 1.9754
            bark3 <- 0.6659
            branches1 <- 0.0257
            branches2 <- 3.1754
            branches3 <- -0.9417
            foliage1 <- 0.1415
            foliage2 <- 2.3074
            foliage3 <- -1.1189
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "white elm"){
            wood1 <- 0.0207
            wood2 <- 2.2276
            wood3 <- 0.6488
            bark1 <- 0.0078
            bark2 <- 2.4540
            bark3 <- 0
            branches1 <- 0.0393
            branches2 <- 2.1880
            branches3 <- 0
            foliage1 <- 0.0516
            foliage2 <- 1.4511
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "white oak"){
            wood1 <- 0.0442
            wood2 <- 1.6818
            wood3 <- 1.0310
            bark1 <- 0.0308
            bark2 <- 1.7479
            bark3 <- 0.3504
            branches1 <- 0.0022
            branches2 <- 2.0165
            branches3 <- 1.3953
            foliage1 <- 0.0053
            foliage2 <- 1.2822
            foliage3 <- 1.1323
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "white spruce"){
            wood1 <- 0.0265
            wood2 <- 1.7952
            wood3 <- 0.9733
            bark1 <- 0.0124
            bark2 <- 1.6962
            bark3 <- 0.6489
            branches1 <- 0.0325
            branches2 <- 2.8573
            branches3 <- -0.9127
            foliage1 <- 0.2020
            foliage2 <- 2.3802
            foliage3 <- -1.1103
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "yellow birch"){
            wood1 <- 0.0259
            wood2 <- 1.9044
            wood3 <- 0.9715
            bark1 <- 0.0069
            bark2 <- 2.0834
            bark3 <- 0.5371
            branches1 <- 0.0325
            branches2 <- 2.3851
            branches3 <- 0
            foliage1 <- 0.1683
            foliage2 <- 1.2764
            foliage3 <- 0
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "hardwood"){
            wood1 <- 0.0359
            wood2 <- 2.0263
            wood3 <- 0.6987
            bark1 <- 0.0094
            bark2 <- 1.8677
            bark3 <- 0.6985
            branches1 <- 0.0433
            branches2 <- 2.6817
            branches3 <- -0.5731
            foliage1 <- 0.0859
            foliage2 <- 1.8485
            foliage3 <- -0.5383
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else if(individualSpecies == "softwood"){
            wood1 <- 0.0284
            wood2 <- 1.6894
            wood3 <- 1.0857
            bark1 <- 0.0100
            bark2 <- 1.8463
            bark3 <- 0.5616
            branches1 <- 0.0301
            branches2 <- 3.0038
            branches3 <- -1.0520
            foliage1 <- 0.1554
            foliage2 <- 2.4021
            foliage3 <- -1.1043
            tempdatatable[species == individualSpecies,
                          biomass := wood1*(DBH^wood2)*(height^wood3)+
                            bark1*(DBH^bark2)*(height^bark3)+
                            foliage1*(DBH^foliage2)*(height^foliage3)+
                            branches1*(DBH^branches2)*(height^branches3)]
          } else {
            tempdatatable[species == individualSpecies,
                          biomass := 0]
          }
        }
      }
    } else if (paperSource == "Ung2008"){
      if(!heightIncluded){
        
      } else {
        
      }
    } else {
      stop("Please select the correct paperSource Lambert2005 or Ung2008")
    }
    return(list(biomass = tempdatatable$biomass,
                missedSpecies = unique(tempdatatable[biomass == 0,]$species)))
  })

#' @export
#' @rdname biomassCalculation
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "missing",
                height = "numeric", paperSource = "character"),
  definition = function(species, DBH, height, paperSource) {
    biomassCalculation(species = species, DBH = DBH, heightIncluded = FALSE,
                       height = height, paperSource = paperSource)
  })

#' @export
#' @rdname biomassCalculation
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "logical",
                height = "numeric", paperSource = "missing"),
  definition = function(species, DBH, heightIncluded, height) {
    biomassCalculation(species = species, DBH = DBH, heightIncluded = heightIncluded,
                       height = height, paperSource = "Lambert2005")
  })

#' @export
#' @rdname biomassCalculation
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "missing",
                height = "numeric", paperSource = "missing"),
  definition = function(species, DBH, height) {
    biomassCalculation(species = species, DBH = DBH, heightIncluded = FALSE,
                       height = height, paperSource = "Lambert2005")
  })

#' @export
#' @rdname biomassCalculation
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "missing",
                height = "missing", paperSource = "character"),
  definition = function(species, DBH, paperSource) {
    biomassCalculation(species = species, DBH = DBH, heightIncluded = FALSE,
                       height = 1, paperSource = paperSource)
  })

#' @export
#' @rdname biomassCalculation
setMethod(
  "biomassCalculation",
  signature = c(species = "character", DBH = "numeric", heightIncluded = "missing",
                height = "missing", paperSource = "missing"),
  definition = function(species, DBH) {
    biomassCalculation(species = species, DBH = DBH, heightIncluded = FALSE,
                       height = 1, paperSource = "Lambert2005")
  })
