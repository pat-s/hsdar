#' vegindex
#' 
#' Function calculates a variety of hyperspectral vegetation indices
#' 
#' Index must be a charater vector containing pre-defined indices (selected by their name) or self defined indices or any combination of pre- and self-defined indices.
#' \subsection{Pre-defined indices}{
#'   The following indices are available:
#'     \tabular{lll}{
#'       \tab\tab\cr
#'       \strong{Name}\tab \strong{Formula} \tab \strong{Reference*}\cr
#'       \tab\tab\cr
#'       Boochs \tab \eqn{D_{703}}                                                                       \tab Boochs et al. (1990)\cr
#'       Boochs2 \tab \eqn{D_{720}}                                                                      \tab Boochs et al. (1990)\cr
#'       CAI \tab \eqn{0.5\cdot (R_{2000}+R_{2200})-R_{2100}}{0.5*(R_{2000}+R_{2200})-R_{2100}}          \tab Nagler et al. (2003)\cr
#'       CARI \tab \eqn{a = (R_{700}-R_{550}) / 150}                                                     \tab Kim et al. (1994)\cr
#'       \tab \eqn{b = R_{550}-(a\cdot 550)}{b = R_{550}-(a*550)}                                   \tab \cr
#'       \tab \eqn{R_{700}\cdot \textnormal{abs}(a\cdot 670+R_{670}+b)/R_{670}\cdot}{R_{700}*abs(a*670+R_{670}+b)/R_{670}*} 
#'       \tab \cr
#'       \tab \eqn{(a^2+1)^{0.5}}                                                                   \tab \cr
#'       Carter \tab \eqn{R_{695}/R_{420}}                                                               \tab Carter (1994)\cr
#'       Carter2 \tab \eqn{R_{695}/R_{760}}                                                              \tab Carter (1994)\cr
#'       Carter3 \tab \eqn{R_{605}/R_{760}}                                                              \tab Carter (1994)\cr
#'       Carter4 \tab \eqn{R_{710}/R_{760}}                                                              \tab Carter (1994)\cr
#'       Carter5 \tab \eqn{R_{695}/R_{670}}                                                              \tab Carter (1994)\cr
#'       Carter6 \tab \eqn{R_{550}}                                                                      \tab Carter (1994)\cr
#'       CI \tab \eqn{R_{675}\cdot R_{690}/R_{683}^2}{R_{675}*R_{690}/R_{683}^2}                         \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                         \tab (2003)\cr
#'       CI2 \tab \eqn{R_{760}/R_{700}-1}                                                                \tab Gitelson et al. (2003) \cr   
#'       ClAInt \tab \eqn{\int_{600 nm}^{735 nm} R}                                                      \tab Oppelt and Mauser\cr
#'       \tab                                                                                     \tab (2004)\cr
#'       CRI1 \tab \eqn{1/R_{515}-1/R_{550}}                                                             \tab Gitelson et al. (2003)\cr
#'       CRI2 \tab \eqn{1/R_{515}-1/R_{770}}                                                             \tab Gitelson et al. (2003)\cr
#'       CRI3 \tab \eqn{1/R_{515}-1/R_{550}\cdot R_{770}}{1/R_{515}-1/R_{550}*R_{770}}                   \tab Gitelson et al. (2003)\cr
#'       CRI4 \tab \eqn{1/R_{515}-1/R_{700}\cdot R_{770}}{1/R_{515}-1/R_{700}*R_{770}}                   \tab Gitelson et al. (2003)\cr
#'       D1 \tab \eqn{D_{730}/D_{706}}                                                                   \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                         \tab (2003)\cr
#'       D2 \tab \eqn{D_{705}/D_{722}}                                                                   \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                         \tab (2003)\cr
#'       Datt \tab \eqn{(R_{850}-R_{710})/(R_{850}-R_{680})}                                             \tab  Datt (1999b) \cr
#'       Datt2 \tab \eqn{R_{850}/R_{710}}                                                                \tab  Datt (1999b) \cr
#'       Datt3 \tab \eqn{D_{754}/D_{704}}                                                                \tab  Datt (1999b) \cr
#'       Datt4 \tab \eqn{R_{672}/(R_{550}\cdot R_{708})}{R_{672}/(R_{550}*R_{708})}                      \tab  Datt (1998) \cr
#'       Datt5 \tab \eqn{R_{672}/R_{550}}                                                                \tab  Datt (1998) \cr
#'       Datt6 \tab \eqn{(R_{860})/(R_{550}\cdot R_{708})}{(R_{860})/(R_{550}*R_{708})}                  \tab  Datt (1998) \cr
#'       Datt7 \tab \eqn{(R_{860} - R_{2218})/(R_{860} - R_{1928})}                                      \tab  Datt (1999a) \cr
#'       Datt8 \tab \eqn{(R_{860} - R_{1788})/(R_{860} - R_{1928})}                                      \tab  Datt (1999a) \cr
#'       DD \tab \eqn{(R_{749}-R_{720})-(R_{701}-R_{672})}                                               \tab le Maire et al. (2004)\cr
#'       DDn \tab \eqn{2\cdot (R_{710}-R_{660}-R_{760})}{2*(R_{710}-R_{660}-R_{760})}                    \tab le Maire et al. (2008)\cr
#'       DPI\tab \eqn{(D_{688}\cdot D_{710})/D_{697}^2}{(D_{688}*D_{710})/D_{697}^2}                     \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                         \tab (2003)\cr
#'       DWSI1 \tab \eqn{R_{800}/R_{1660}}\tab Apan et al. (2004)\cr
#'       DWSI2 \tab \eqn{R_{1660}/R_{550}}\tab Apan et al. (2004)\cr
#'       DWSI3 \tab \eqn{R_{1660}/R_{680}}\tab Apan et al. (2004)\cr
#'       DWSI4 \tab \eqn{R_{550}/R_{680}}\tab Apan et al. (2004)\cr
#'       DWSI5 \tab \eqn{(R_{800} + R_{550})/(R_{1660} + R_{680})}\tab Apan et al. (2004)\cr
#'       EGFN \tab \eqn{(\max(D_{650:750})-\max(D_{500:550}))/}                                          \tab Penuelas et al. \cr     
#'       \tab \eqn{(\max(D_{650:750})+\max(D_{500:550}))}                                           \tab (1994)\cr
#'       EGFR \tab \eqn{\max(D_{650:750})/\max(D_{500:550})}                                             \tab Penuelas et al. (1994)\cr
#'       EVI \tab \eqn{2.5\cdot ((R_{800}-R_{670})/}{2.5*((R_{800}-R_{670})/}                            \tab Huete et al. (1997)\cr    
#'       \tab \eqn{(R_{800}-(6\cdot R_{670})-(7.5\cdot R_{475})+1))}{(R_{800}-(6*R_{670})-(7.5*R_{475})+1))} 
#'       \tab \cr
#'       GDVI\tab \eqn{(R_{800}^n-R_{680}^n) / (R_{800}^n+R_{680}^n)}**                                  \tab Wu (2014)\cr                                                                                             
#'       GI \tab \eqn{R_{554}/R_{677}}                                                                   \tab Smith et al. (1995)\cr
#'       Gitelson \tab \eqn{1/R_{700}}                                                                   \tab Gitelson et al. (1999)\cr
#'       Gitelson2 \tab \eqn{(R_{750}-R_{800}/R_{695}-R_{740})-1}                                        \tab Gitelson et al. (2003)\cr
#'       GMI1 \tab \eqn{R_{750}/R_{550}}                                                                 \tab Gitelson et al. (2003)\cr
#'       GMI2 \tab \eqn{R_{750}/R_{700}}                                                                 \tab Gitelson et al. (2003)\cr
#'       Green NDVI \tab \eqn{(R_{800}-R_{550})/(R_{800}+R_{550})}                                       \tab Gitelson et al. (1996)\cr
#'       LWVI_1 \tab \eqn{(R_{1094}-R_{983}) / (R_{1094}+R_{983})}                                       \tab Galvao et al. (2005)\cr
#'       LWVI_2 \tab \eqn{(R_{1094}-R_{1205}) / (R_{1094}+R_{1205})}                                     \tab Galvao et al. (2005)\cr
#'       Maccioni \tab \eqn{(R_{780}-R_{710})/(R_{780}-R_{680})}                                         \tab Maccioni et al. (2001)\cr
#'       MCARI \tab \eqn{((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550}))\cdot}{((R_{700}-R_{670})-0.2*(R_{700}-R_{550}))*}   
#'       \tab Daughtry et al. (2000)\cr
#'       \tab \eqn{(R_{700}/R_{670})}                                                              \tab \cr
#'       MCARI/OSAVI \tab                                                                                \tab Daughtry et al. (2000)\cr
#'       MCARI2 \tab \eqn{((R_{750}-R_{705})-0.2\cdot (R_{750}-R_{550}))\cdot}{((R_{750}-R_{705})-0.2*(R_{750}-R_{550}))*}    
#'       \tab Wu et al. (2008)\cr     
#'       \tab \eqn{ (R_{750}/R_{705})}                                                            \tab \cr
#'       MCARI2/OSAVI2\tab                                                                               \tab Wu et al. (2008)\cr
#'       mND705 \tab \eqn{(R_{750}-R_{705})/(R_{750}+R_{705}-2\cdot R_{445})}{(R_{750}-R_{705})/(R_{750}+R_{705}-2*R_{445})}  
#'       \tab Sims and Gamon (2002)\cr
#'       mNDVI \tab \eqn{(R_{800}-R_{680})/(R_{800}+R_{680}-2\cdot R_{445})}{(R_{800}-R_{680})/(R_{800}+R_{680}-2*R_{445})}    
#'       \tab Sims and Gamon (2002)\cr
#'       MPRI \tab \eqn{(R_{515}-R_{530})/(R_{515}+R_{530})}                                             \tab Hernandez-Clemente et al.\cr
#'       \tab                                                                                       \tab (2011)\cr   
#'       mREIP \tab Red-edge inflection point using Gaussain fit                                         \tab Miller et al. (1990)\cr
#'       MSAVI \tab \eqn{0.5 \cdot  (2\cdot R_{800}+1-}{0.5*(2*R_{800}+1-}                               \tab Qi et al. (1994)\cr      
#'       \tab \eqn{((2\cdot R_{800}+1)^2-8\cdot (R_{800}-R_{670}))^{0.5})}{((2*R_{800}+1)^2-8*(R_{800}-R_{670}))^{0.5})}  
#'       \tab \cr
#'       MSI \tab \eqn{R_{1600}/ R_{817}}                                                                \tab Hunt and Rock (1989)\cr
#'       mSR \tab \eqn{(R_{800}-R_{445})/(R_{680}-R_{445})}                                              \tab Sims and Gamon (2002)\cr
#'       mSR2 \tab \eqn{(R_{750}/R_{705})-1/(R_{750}/R_{705}+1)^{0.5}}                                   \tab Chen (1996)\cr
#'       mSR705 \tab \eqn{(R_{750}-R_{445})/(R_{705}-R_{445})}                                           \tab Sims and Gamon (2002)\cr
#'       MTCI \tab \eqn{(R_{754}-R_{709})/(R_{709}-R_{681})}                                             \tab Dash and Curran (2004)\cr
#'       MTVI \tab \eqn{1.2\cdot (1.2\cdot (R_{800}-R_{550})-}{1.2*(1.2*(R_{800}-R_{550})-}              \tab Haboudane et al.\cr
#'       \tab \eqn{2.5\cdot (R_{670}-R_{550}))}{2.5* (R_{670}-R_{550}))}                            \tab (2004)\cr
#'       NDLI\tab \eqn{(log(1/R_{1754}) - log(1/R_{1680}))/}                                             \tab Serrano et al. (2002) \cr 
#'       \tab \eqn{(log(1/R_{1754}) + log(1/R_{1680}))}                                              \tab \cr
#'       NDNI\tab \eqn{(log(1/R_{1510}) - log(1/R_{1680}))/}                                             \tab Serrano et al. (2002) \cr 
#'       \tab \eqn{(log(1/R_{1510}) + log(1/R_{1680}))}                                              \tab \cr
#'       NDVI \tab \eqn{(R_{800}-R_{680}) / (R_{800}+R_{680})}                                           \tab Tucker (1979)\cr
#'       NDVI2 \tab \eqn{(R_{750}-R_{705})/(R_{750}+R_{705})}                                            \tab Gitelson and Merzlyak  \cr
#'       \tab                                                                                      \tab (1994)\cr
#'       NDVI3 \tab \eqn{(R_{682}-R_{553})/(R_{682}+R_{553})}                                            \tab Gandia et al. (2004)\cr
#'       NDWI \tab \eqn{(R_{860}-R_{1240}) / (R_{860}+R_{1240})}                                         \tab Gao (1996)\cr
#'       NPCI \tab \eqn{(R_{680}-R_{430})/(R_{680}+R_{430})}                                             \tab Penuelas et al. (1994)\cr
#'       OSAVI \tab \eqn{(1+0.16) \cdot  (R_{800}-R_{670})/}{(1+0.16)*(R_{800}-R_{670})/}                \tab Rondeaux et al.\cr
#'       \tab \eqn{(R_{800}+R_{670}+0.16)}                                                         \tab (1996)\cr
#'       OSAVI2 \tab \eqn{(1+0.16) \cdot  (R_{750}-R_{705})/}{(1+0.16)*(R_{750}-R_{705})/}               \tab Wu et al. (2008)\cr
#'       \tab \eqn{(R_{750}+R_{705}+0.16)}                                                        \tab \cr
#'       PARS \tab \eqn{R_{746}/R_{513}}                                                                 \tab Chappelle et al. (1992)\cr
#'       PRI \tab \eqn{(R_{531}-R_{570})/(R_{531}+R_{570})}                                              \tab Gamon et al. (1992)\cr
#'       PRI_norm \tab \eqn{\textnormal{PRI} \cdot (-1)/(\textnormal{RDVI}\cdot R_{700}/R_{670})}{PRI*(-1)/(RDVI*R_{700}/R_{670})}
#'       \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                        \tab (2013)\cr
#'       PRI*CI2 \tab \eqn{\textnormal{PRI} \cdot \textnormal{CI}2}{PRI*CI2}                             \tab Garrity et al. (2011)\cr
#'       PSRI \tab \eqn{(R_{678}-R_{500}/R_{750}}                                                        \tab Merzlyak et al. (1999)\cr
#'       PSSR \tab \eqn{R_{800}/R_{635}}                                                                 \tab Blackburn (1998)\cr
#'       PSND \tab \eqn{(R_{800}-R_{470})/(R_{800}-R_{470})}                                             \tab Blackburn (1998)\cr
#'       PWI \tab \eqn{R_{970}/R_{900}}                                                                  \tab Penuelas et al. (1997)\cr
#'       RDVI \tab \eqn{(R_{800}-R_{670})/\sqrt{R_{800}+R_{670}}}                                        \tab Roujean and Breon (1995)\cr
#'       % RDVI \tab \eqn{(R_{800}-R_{670})/(R_{800}+R_{670})^{0.5}}                                     \tab Roujean and Breon (1995)\cr
#'       REP_LE \tab Red-edge position through linear extrapolation.                                     \tab Cho and Skidmore (2006)\cr
#'       REP_Li \tab \eqn{R_{re}=(R_{670}+R_{780})/2}                                                    \tab Guyot and Baret (1988)\cr
#'       \tab \eqn{700 + 40\cdot((R_{re} -R_{700})/(R_{740}-R_{700}))}{700 + 40*((R_{re} -R_{700})/(R_{740}-R_{700}))}\tab \cr
#'       % REP_Li \tab \eqn{700 + 40((((R_{670}+R_{780})/2)-R_{700})/}                                     \tab Guyot and Baret (1988)\cr
#'       %        \tab \eqn{(R_{740}-R_{700}))}                                                            \tab \cr
#'       SAVI \tab \eqn{(1+L)\cdot (R_{800}-R_{670})/(R_{800}+R_{670}+L)}{(1+L)*(R_{800}-R_{670})/(R_{800}+R_{670}+L)} 
#'       \tab Huete (1988)\cr
#'       SIPI \tab \eqn{(R_{800}-R_{445})/(R_{800}-R_{680})}                                             \tab Penuelas et al. (1995),\cr
#'       \tab                                                                                       \tab Penuelas et al. (1995a) \cr
#'       SPVI \tab \eqn{0.4\cdot 3.7\cdot (R_{800}-R_{670})-1.2\cdot }{0.4*3.7*(R_{800}-R_{670})-1.2*}   \tab Vincini et al. (2006)\cr  
#'       \tab \eqn{((R_{530}-R_{670})^2)^{0.5}}                                                     \tab \cr
#'       SR \tab \eqn{R_{800}/R_{680}}                                                                   \tab Jordan (1969)\cr
#'       SR1 \tab \eqn{R_{750}/R_{700}}                                                                  \tab Gitelson and Merzlyak  \cr
#'       \tab                                                                                        \tab (1997)\cr
#'       SR2 \tab \eqn{R_{752}/R_{690}}                                                                  \tab Gitelson and Merzlyak  \cr
#'       \tab                                                                                        \tab (1997)\cr
#'       SR3 \tab \eqn{R_{750}/R_{550}}                                                                  \tab Gitelson and Merzlyak  \cr
#'       \tab                                                                                        \tab (1997)\cr
#'       SR4 \tab \eqn{R_{700}/R_{670}}                                                                  \tab McMurtey et al. (1994)\cr
#'       SR5 \tab \eqn{R_{675}/R_{700}}                                                                  \tab Chappelle et al. (1992)\cr
#'       SR6 \tab \eqn{R_{750}/R_{710}}                                                                  \tab Zarco-Tejada and Miller  \cr
#'       \tab                                                                                        \tab (1999)\cr
#'       SR7 \tab \eqn{R_{440}/R_{690}}                                                                  \tab Lichtenthaler et al. (1996)\cr
#'       SR8 \tab \eqn{R_{515}/R_{550}}                                                                  \tab Hernandez-Clemente et al. \cr
#'       \tab                                                                                        \tab (2012)\cr
#'       SRPI \tab \eqn{R_{430}/R_{680}}                                                                 \tab Penuelas et al. (1995)\cr
#'       SRWI \tab \eqn{R_{850}/R_{1240}}                                                                \tab Zarco-Tejada et al. \cr
#'       \tab                                                                                        \tab (2003)\cr
#'       Sum_Dr1 \tab \eqn{\sum_{i=626}^{795} D1_i}                                                      \tab Elvidge and Chen (1995)\cr
#'       Sum_Dr2 \tab \eqn{\sum_{i=680}^{780} D1_i}                                                      \tab Filella and Penuelas  \cr
#'       \tab                                                                                        \tab (1994)\cr
#'       SWIR FI\tab \eqn{R_{2133}^2/(R_{2225} \cdot R_{2209}^3}                                         \tab Levin et al. (2007)\cr  
#'       SWIR LI\tab \eqn{3.87  \cdot (R_{2210} - R_{2090}) - }                                          \tab Lobell et al. (2001)\cr
#'       \tab \eqn{27.51 \cdot (R_{2280} - R_{2090}) - 0.2}                                       \tab \cr  
#'       SWIR SI\tab \eqn{-41.59 \cdot (R_{2210} - R_{2090}) + }                                          \tab Lobell et al. (2001)\cr
#'       \tab \eqn{1.24 \cdot (R_{2280} - R_{2090}) + 0.64}                                       \tab \cr 
#'       SWIR VI\tab \eqn{37.72  \cdot (R_{2210} - R_{2090}) + }                                          \tab Lobell et al. (2001)\cr
#'       \tab \eqn{26.27 \cdot (R_{2280} - R_{2090}) + 0.57}                                       \tab \cr        
#'       TCARI \tab \eqn{3\cdot ((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550})\cdot}{3*((R_{700}-R_{670})-0.2*R_{700}-R_{550})*} 
#'       \tab Haboudane et al. (2002)\cr  
#'       \tab \eqn{(R_{700}/R_{670}))}                                                             \tab \cr
#'       TCARI/OSAVI \tab  \eqn{\textnormal{TCARI} / \textnormal{OSAVI}}{TCARI/OSAVI}                    \tab Haboudane et al. (2002)\cr
#'       TCARI2 \tab \eqn{3\cdot ((R_{750}-R_{705})-0.2\cdot (R_{750}-R_{550})\cdot}{3*((R_{750}-R_{705})-0.2*(R_{750}-R_{550})*}
#'       \tab Wu et al. (2008)\cr  
#'       \tab \eqn{(R_{750}/R_{705}))}                                                            \tab \cr
#'       TCARI2/OSAVI2 \tab \eqn{\textnormal{TCARI}2 / \textnormal{OSAVI}2}{TCARI2/OSAVI2}               \tab Wu et al. (2008)\cr
#'       TGI\tab \eqn{-0.5 (190 (R_{670} - R_{550} ) - 120 (R_{670} - R_{480} ))}                        \tab Hunt et al. (2013)\cr
#'       TVI \tab \eqn{0.5\cdot (120\cdot (R_{750}-R_{550})-}{0.5*(120*(R_{750}-R_{550})-}               \tab Broge and Leblanc \cr
#'       \tab \eqn{200\cdot (R_{670}-R_{550}))}{200*(R_{670}-R_{550}))}                              \tab (2000)\cr
#'       Vogelmann \tab \eqn{R_{740}/R_{720}}                                                            \tab Vogelmann et al. (1993)\cr
#'       Vogelmann2 \tab \eqn{(R_{734}-R_{747})/(R_{715}+R_{726})}                                       \tab Vogelmann et al. (1993)\cr
#'       Vogelmann3 \tab \eqn{D_{715}/D_{705}}                                                           \tab Vogelmann et al. (1993)\cr
#'       Vogelmann4 \tab \eqn{(R_{734}-R_{747})/(R_{715}+R_{720})}                                       \tab Vogelmann et al. (1993)\cr
#'       % CI_Hanna \tab \eqn{R_{760}/R_{695}}\tab \cr
#'       % REIP \tab \eqn{NULL}                                                                          \tab Collins (1978), Horler et al. (1983)\cr
#'       % TCARI \tab \eqn{3\cdot ((R_{700}-R_{670})-0.2\cdot (R_{700}-R_{550})\cdot (R_{700}/R_{670}))} \tab \cr
#'       % TVI \tab \eqn{0.5\cdot (120\cdot (R_{750}-R_{550})-200\cdot (R_{670}-R_{550}))}               \tab \cr
#'       % WBI \tab \eqn{R_{970}/ R_{900}}                                                               \tab \cr
#'       
#'     }
#' 
#' \* For references please type: `hsdardocs("References.pdf")`. \cr
#' \** For GDVI n must be defined appending an underscore and the intended exponent to
#' the index name.  
#' E.g., for n = 2, the correct index name would be "GDVI_2".
#' Note that GDVI-indices with n = 2, 3, 4 will be derived if all available
#' indices are calculated.
#' }
#' 
#' \subsection{Self-defining indices}{  
#' Self-defined indices may be passed using the following syntax:
#'  \itemize{
#'    \item{Rxxx: }{Reflectance at wavelength 'xxx'. Note that R must be upper case.}
#'    \item{Dxxx: }{First derivation of reflectance values at wavelength 'xxx'. Note that D must be upper case.}
#'  }
#' Using this syntax, complex indices can be easily defined. Note that the 
#' entire definition of the index must be passed as one character string. 
#' Consequently, the NDVI would be written as "(R800-R680)/(R800+R680)".
#' }
#' 
#' \subsection{HyperSpecRaster}{If the input object is of class `HyperSpecRaster`, a raster 
#' file is written to disk if a file name is provided. 
#' Otherwise an `HyperSpecRaster` object is returned. If the file is written to disk,
#' the user needs to specify the final number of bands. This information is required 
#' by `writeRaster`. NAs are handled internally. }
#' 
#' @param x Object of class `Speclib` or `HyperSpecRaster`.
#' @param index Character string. Name or definition of index or vector with
#' names/definitions of indices to calculate. See Details section for further
#' information.
#' @param returnHCR If TRUE, the result will be of class HyperSpecRaster,
#' otherwise it is a data frame. If "auto", the class is automatically
#' determined by passed Speclib.
#' @param L Factor for SAVI index. Unused for other indices.
#' @param weighted Logical indicating if reflectance values should be
#' interpolated to fit wavelength position. If `FALSE` the reflectance
#' values of nearest neighbour to passed position are returned. See
#' [=get_reflectance.speclib::get_reflectance()] for further
#' explanation.
#' @param filename Filename of the raster file written to disk. Only used if 
#' an object of class `HyperSpecRaster` is provided
#' @param bnames (optional) Character vector of band names. Only used if 
#' an object of class `HyperSpecRaster` is provided
#' @param ...  Further arguments passed to derivative functions. Only used for
#' indices requiring derivations.
#' 
#' 
#' 
#' @return A vector containing indices values. If index is a vector with 
#' `length > 1`, a data frame with ncol = length(index) and nrow = number of spectra in
#' x is returned.
#' 
#' If function is called without any arguments, return value will be a vector
#' containing all available indices in alphabetical order.
#' 
#' @author Hanna Meyer and Lukas Lehnert
#' @seealso [soilindex()], [derivative.speclib()],
#' [rededge()],
#' [=get_reflectance.speclib::get_reflectance()]
#' @references See `hsdardocs("References.pdf")`
#' @keywords multivariate
#' @examples
#' 
#' ## Example calculating NDVI
#' data(spectral_data)
#' ndvi <- vegindex(spectral_data, "NDVI")
#' 
#' 
#' ## Example calculating all available indices
#' ## Get available indices
#' avl <- vegindex()
#' vi <- vegindex(spectral_data, avl)
#' 
#' ## Self-defined indices
#' ## NDVI
#' vi <- vegindex(spectral_data, "(R800-R680)/(R800+R680)")
#' ## NDNI
#' vi <- vegindex(spectral_data, 
#'                "(log(1/R1510) - log(1/R1680))/(log(1/R1510) + log(1/R1680))")
#' ## D1
#' vi <- vegindex(spectral_data, "D730/D706")
#' 
#' @export vegindex
#' 
vegindex <- function(
  x,
  index,
  returnHCR = "auto",
  L = 0.5,
  weighted = TRUE, 
  bnames = NULL,
  filename = '', 
  method = NULL, 
  ...
)
  #### ToDo: 
  # - hand over "nl" argument automatically from length(index)
  # - when not writing to disk, return rasterlayer/stack instead of HyperSpecRaster
{  
  
  ### HyperSpecRaster approach  Sat Mar 25 09:20:18 2017 ------------------------------
  # HyperSpecRaster is converted in speclib in `hsdar::getValuesBlock`. 
  # Then vegindex calculation is performed with the `vegindex` function on class `speclib`
  # Advantages: 
  # - Write raster to disk when filename is provided
  # - label bands by calculated vegindices when writing to format "raster" (.gri)
  # 
  if (class(x) == "HyperSpecRaster") {
    
    out_ras <- x
    big <- !canProcessInMemory(out_ras, 3)
    filename <- trim(filename)
    if (big & filename == '') {
      filename <- rasterTmpFile()
    }
    if (filename != '') {
      out_ras <- writeStart(out_ras, filename, overwrite = TRUE, ...)
      todisk <- TRUE
    } else {
      vv <- matrix(ncol = nrow(out_ras), nrow = ncol(out_ras))
      todisk <- FALSE
    }

    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)

    if (todisk) {
      for (i in 1:bs$n) {
        v <- hsdar::getValuesBlock(x, row = bs$row[i], nrows = bs$nrows[i] )

        ### processing function here
        v <- as.matrix(vegindex(v, index = index, method = method))
        ###

        # provide the possibility to rename bands
        if (!is.null(bnames)) {
          names(out_ras) <- bnames
        }

        out_ras <- writeValues(out_ras, v, bs$row[i])
        pbStep(pb, i)
      }
      out_ras <- writeStop(out_ras)
    } else {
      for (i in 1:bs$n) {
        v <- getValuesBlock(x, row = bs$row[i], nrows = bs$nrows[i] )

        v <- as.matrix(vegindex(v, index = index))

        cols <- bs$row[i]:(bs$row[i] + bs$nrows[i] - 1)
        vv[,cols] <- matrix(v, nrow = out_ras@ncols)
        pbStep(pb, i)
      }
      out_ras <- setValues(out_ras, as.vector(vv))
    }
    # provide the possibility to rename bands (second time, just in case for returning the objects)
    # too lazy to check if this is really necessary atm
    if (!is.null(bnames)) {
      names(out_ras) <- bnames
    }
    pbClose(pb)
    return(out_ras)
  }
  
  #### end of HyperSpecRaster approach
  
  if (length(names(match.call())) == 0)
  {
    return(vegindex_available())
  }
  
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
  
  # x_back <- x # not used in scope?
  
  if (!is.speclib(x))
    stop("x is not of class 'Speclib'")
  # if (!x@continuousdata)
  #   stop("x does not contain continuous spectra")
  if (returnHCR == "auto")
    returnHCR <- .is.rastermeta(x)
  
  # convertSpatialGrid <- returnHCR # not used in scope?
  gridMeta <- x@rastermeta
  
  if (returnHCR)
  {
    if (!.is.rastermeta(x))
      stop("If returnHCR, x must contain meta information")
  }
  
  if (length(index) > 1)
  {
    
    result <- as.data.frame(matrix(data = NA,
                                   nrow = dim(x)[1],
                                   ncol = length(index)))
    
    for (i in 1:length(index))
    {
      temp <- vegindex(x, index[i], returnHCR = FALSE, ...)
      if (!is.null(temp))
      {
        result[,i] <- temp
      }
    }
    if (nspectra(x) > 1 & nspectra(x) < 10000)
    {
      names(result) <- index
      row.names(result) <- idSpeclib(x)
    }
    if (returnHCR)
    {
      spec <- speclib(result, c(1:ncol(result)))
      if (.is.rastermeta(x))
        spec@rastermeta <- x@rastermeta
      result <- HyperSpecRaster(spec)
    }
    return(result)
  }
  
  d_indexs <- c("Boochs","Boochs2","Datt3","D1","D2","EGFR","EGFN",
                "Vogelmann3","Sum_Dr1","Sum_Dr2","REP_LE","DPI")
  m <- c(rep.int(1,length(d_indexs)))
  
  # index_current <<- index
  # row_names_x <<- row.names(x$spectra)
  
  if (any(index == d_indexs)) 
    x <- derivative.speclib(x, m = m[d_indexs == index], ...)
  
  y <- spectra(x)
  x <- wavelength(x)
  
  #######################################################STRUCTURAL INDICES#############################################
  if (index == "NDVI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,680,weighted)) /
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,680,weighted))))
  }
  if (index == "OSAVI")
  {
    return(return_index((1+0.16) * (get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,670,weighted)+0.16) ))
  }
  if (index == "RDVI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))/
                          sqrt(get_reflectance(y,x,800,weighted)+get_reflectance(y,x,670,weighted))))
  }
  if (index == "SAVI")
  {
    return(return_index((1+L)*(get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,670,weighted)+L)))
  }
  if (index == "MTVI")
  {
    return(return_index(1.2*(1.2*(get_reflectance(y,x,800,weighted)-get_reflectance(y,x,550,weighted))-
                               2.5*(get_reflectance(y,x,670,weighted)-get_reflectance(y,x,550,weighted)))))
  }
  
  #############################################################WATER INDICES#################################################
  
  if (index == "NDWI")
  {
    return(return_index((get_reflectance(y,x,860,weighted)-get_reflectance(y,x,1240,weighted)) /
                          (get_reflectance(y,x,860,weighted)+get_reflectance(y,x,1240,weighted))))
  }
  if (index == "PWI")
  {
    return(return_index(get_reflectance(y,x,970,weighted)/get_reflectance(y,x,900,weighted)))
  }
  if (index == "MSI")  
  {
    return(return_index(get_reflectance(y,x,1600,weighted)/ get_reflectance(y,x,817,weighted)))
  }
  if (index == "WBI")  
  {
    return(return_index(get_reflectance(y,x,970,weighted)/ get_reflectance(y,x,900,weighted)))
  }
  if (index == "SRWI")
  {
    return(return_index(get_reflectance(y,x,850,weighted)/get_reflectance(y,x,1240,weighted)))
  }
  #######################################################CHLOROPHYLL AND RED EDGE INDICES######################################
  
  if (index == "GMI1")
  {
    return(return_index(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,550,weighted)))
  }
  if (index == "GMI2")
  {
    return(return_index(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,700,weighted)))
  }
  if (index == "MCARI")
  {
    return(return_index(((get_reflectance(y,x,700,weighted)-get_reflectance(y,x,670,weighted))-
                           0.2*(get_reflectance(y,x,700,weighted)-get_reflectance(y,x,550,weighted)))*
                          (get_reflectance(y,x,700,weighted)/get_reflectance(y,x,670,weighted))))
  }
  if (index == "TVI")
  {
    return(return_index(0.5*(120*(get_reflectance(y,x,750,weighted)-get_reflectance(y,x,550,weighted))-
                               200*(get_reflectance(y,x,670,weighted)-get_reflectance(y,x,550,weighted)))))
  }
  
  if (index == "Vogelmann4")
  {
    return(return_index((get_reflectance(y,x,734,weighted)-get_reflectance(y,x,747,weighted))/
                          (get_reflectance(y,x,715,weighted)+get_reflectance(y,x,720,weighted))))
  }
  if (index == "Boochs")
  {
    return(return_index(get_reflectance(y,x,703,weighted)))
  }
  if (index == "Boochs2")
  {
    return(return_index(get_reflectance(y,x,720,weighted)))
  }
  if (index == "CARI")
  {
    a = (get_reflectance(y,x,700,weighted)-get_reflectance(y,x,550,weighted)) / 150
    b = get_reflectance(y,x,550,weighted)-(a*550)
    return(return_index(get_reflectance(y,x,700,weighted)*abs(a*670+get_reflectance(y,x,670,weighted)+b)/
                          get_reflectance(y,x,670,weighted)*(a^2+1)^0.5))
  }
  if (index == "CI")
  {
    return(return_index(get_reflectance(y,x,675,weighted)*get_reflectance(y,x,690,weighted)/
                          get_reflectance(y,x,683,weighted)^2))
  }
  if (index == "Carter")
  {
    return(return_index(((get_reflectance(y,x,695,weighted))/(get_reflectance(y,x,420,weighted)))))
  }
  if (index == "Carter2")
  {
    return(return_index(((get_reflectance(y,x,695,weighted))/(get_reflectance(y,x,760,weighted)))))
  }
  if (index == "Carter3")
  {
    return(return_index(((get_reflectance(y,x,605,weighted))/(get_reflectance(y,x,760,weighted)))))
  }
  if (index == "Carter4")
  {
    return(return_index(((get_reflectance(y,x,710,weighted))/(get_reflectance(y,x,760,weighted)))))
  }
  if (index == "Carter5")
  {
    return(return_index(((get_reflectance(y,x,695,weighted))/(get_reflectance(y,x,670,weighted)))))
  }
  if (index == "Carter6")
  {
    return(return_index((get_reflectance(y,x,550,weighted))))
  }
  if (index == "Datt")
  {
    return(return_index(((get_reflectance(y,x,850,weighted)-get_reflectance(y,x,710,weighted))/
                           (get_reflectance(y,x,850,weighted)-get_reflectance(y,x,680,weighted)))))
  }
  if (index == "Datt2")
  {
    return(return_index(((get_reflectance(y,x,850,weighted))/(get_reflectance(y,x,710,weighted)))))
  }
  if (index == "Datt3")
  {
    return(return_index(((get_reflectance(y,x,754,weighted))/(get_reflectance(y,x,704,weighted)))))
  }
  if (index == "Datt4")
  {
    return(return_index(((get_reflectance(y,x,672,weighted))/
                           (get_reflectance(y,x,550,weighted)*get_reflectance(y,x,708,weighted)))))
  }
  if (index == "Datt5")
  {
    return(return_index(((get_reflectance(y,x,672,weighted))/(get_reflectance(y,x,550,weighted)))))
  }
  if (index == "Datt6")
  {
    return(return_index(((get_reflectance(y,x,860,weighted))/
                           (get_reflectance(y,x,550,weighted)*get_reflectance(y,x,708,weighted)))))
  }
  if (index == "Datt7")
  {
    return(return_index((get_reflectance(y,x,860,weighted) - get_reflectance(y,x,2218,weighted))/
                          (get_reflectance(y,x,860,weighted) - get_reflectance(y,x,1928,weighted))))
  }
  if (index == "Datt8")
  {
    return(return_index((get_reflectance(y,x,860,weighted) - get_reflectance(y,x,1788,weighted))/
                          (get_reflectance(y,x,860,weighted) - get_reflectance(y,x,1928,weighted))))
  }
  if (index == "DD")
  {
    return(return_index((get_reflectance(y,x,749,weighted)-get_reflectance(y,x,720,weighted))-
                          (get_reflectance(y,x,701,weighted)-get_reflectance(y,x,672,weighted))))
  }
  if (index == "DDn")
  {
    return(return_index(2*(get_reflectance(y,x,710,weighted)-get_reflectance(y,x,660,weighted)-
                             get_reflectance(y,x,760,weighted))))
  }
  if (index == "D1")
  {
    return(return_index(get_reflectance(y,x,730,weighted)/get_reflectance(y,x,706,weighted)))
  }
  if (index == "D2")
  {
    return(return_index(get_reflectance(y,x,705,weighted)/get_reflectance(y,x,722,weighted)))
  }
  if (index == "DPI")
  {
    return(return_index(get_reflectance(y,x,688,weighted)*get_reflectance(y,x,710,weighted)/get_reflectance(y,x,697,weighted)^2))
  }
  if (index == "EVI")
  {
    return(return_index(2.5*((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))/
                               (get_reflectance(y,x,800,weighted)-(6*get_reflectance(y,x,670,weighted))-
                                  (7.5*get_reflectance(y,x,475,weighted))+1))))
  }
  if (index == "EGFR")
  {
    if (x[1] > 500) return(NULL)
    if (x[length(x)] < 750) return(NULL)
    dG  <- apply(y[,x>=500 & x<=550],1,max)
    dRE <- apply(y[,x>=650 & x<=750],1,max)
    return(return_index(dRE/dG))
  }
  if (index == "EGFN")
  {
    if (x[1] > 500) return(NULL)
    if (x[length(x)] < 750) return(NULL)
    dG  <- apply(y[,x>=500 & x<=550],1,max)
    dRE <- apply(y[,x>=650 & x<=750],1,max)
    return(return_index((dRE-dG)/(dRE+dG)))
  }
  if (index == "GI")
  {
    return(return_index(get_reflectance(y,x,554,weighted)/get_reflectance(y,x,677,weighted)))
  }
  if (index == "Gitelson")
  {
    return(return_index(1/get_reflectance(y,x,700,weighted)))
  }
  if (index == "Gitelson2")
  {
    return(return_index((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,800,weighted)/
                           get_reflectance(y,x,695,weighted)-get_reflectance(y,x,740,weighted))-1))
  }
  if (index == "Green NDVI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,550,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,550,weighted))))
  }
  # if (index == "MCARI")
  # {
  #   return(return_index(((get_reflectance(y,x,700)-get_reflectance(y,x,670))-0.2*(get_reflectance(y,x,700)-get_reflectance(y,x,550)))*(get_reflectance(y,x,700)/get_reflectance(y,x,670))
  # }
  if (index == "MCARI/OSAVI")
  {
    x <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(x,"MCARI",weighted=weighted)/vegindex(x,"OSAVI",weighted=weighted)))
  }
  if (index == "MCARI2")
  {
    return(return_index(((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,705,weighted))-
                           0.2*(get_reflectance(y,x,750,weighted)-get_reflectance(y,x,550,weighted)))*
                          (get_reflectance(y,x,750,weighted)/get_reflectance(y,x,705,weighted))))
  }
  if (index == "MCARI2/OSAVI2")
  {
    x <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(x,"MCARI2",weighted=weighted)/vegindex(x,"OSAVI2",weighted=weighted)))
  }
  if (index == "mNDVI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,680,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,680,weighted)-
                             2*get_reflectance(y,x,445,weighted))))
  }
  if (index == "mND705")
  {
    return(return_index((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,705,weighted))/
                          (get_reflectance(y,x,750,weighted)+get_reflectance(y,x,705,weighted)-
                             2*get_reflectance(y,x,445,weighted))))
  }
  if (index == "Maccioni")
  {
    return(return_index((get_reflectance(y,x,780,weighted)-get_reflectance(y,x,710,weighted))/
                          (get_reflectance(y,x,780,weighted)-get_reflectance(y,x,680,weighted))))
  }
  if (index == "mREIP")
  {
    mREIP_fun <- function(x, wl)
    {
      Rs <- x[length(x)-1]
      R0 <- x[length(x)]
      x  <- x[1:(length(x)-2)]
      Bl <- -1*log(sqrt((Rs-x)/(Rs-R0)))
      if (all(is.finite(Bl)))
      {
        coef <- summary(lm(Bl~wl))$coefficients
        c(-1*coef[1,1]/coef[2,1])#,1/sqrt(abs(2*coef[2,1])))
      } else {
        c(NA, NA)
      }
    }
    if (x[1] > 670) return(rep.int(NA,nrow(y)))
    if (x[length(x)] < 795) return(rep.int(NA,nrow(y)))
    #   R0 <- x[x>=670&x<=685]
    #   if (any((R0[-length(R0)]-R0[-1]) < 1)) return(rep.int(NA,nrow(y)))
    #   R0 <- x[x>=780&x<=795]
    #   if (any((R0[-length(R0)]-R0[-1]) < 1)) return(rep.int(NA,nrow(y)))
    if (nrow(y) == 1)
    {
      R0 <- matrix(data=apply(matrix(y[,x>=670&x<=685], nrow = 1),1,mean),ncol=1)
      Rs <- matrix(data=apply(matrix(y[,x>=780&x<=795], nrow = 1),1,mean),ncol=1)
      Rl <- matrix(y[,x>=670&x<=685], nrow = 1)
    } else {
      R0 <- matrix(data=apply(y[,x>=670&x<=685],1,mean),ncol=1)
      Rs <- matrix(data=apply(y[,x>=780&x<=795],1,mean),ncol=1)
      Rl <- as.matrix(y[,x>=670&x<=685])
    }
    dat <- cbind(Rl,Rs,R0)
    Bl <- apply(dat,1,mREIP_fun,x[x>=670&x<=685])
    return(return_index(as.vector(t(Bl))))
  }
  if (index == "MSAVI")
  {
    return(return_index(0.5 * (2*get_reflectance(y,x,800,weighted)+1-((2*get_reflectance(y,x,800,weighted)+1)^2-
                                                                        8*(get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted)))^0.5)))
  }
  if (index == "mSR")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,445,weighted))/
                          (get_reflectance(y,x,680,weighted)-get_reflectance(y,x,445,weighted))))
  }
  if (index == "mSR705")
  {
    return(return_index((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,445,weighted))/
                          (get_reflectance(y,x,705,weighted)-get_reflectance(y,x,445,weighted))))
  }
  if (index == "mSR2")
  {
    return(return_index((get_reflectance(y,x,750,weighted)/get_reflectance(y,x,705,weighted))-
                          1/(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,705,weighted)+1)^0.5))
  }
  if (index == "MTCI")
  {
    return(return_index((get_reflectance(y,x,754,weighted)-get_reflectance(y,x,709,weighted))/
                          (get_reflectance(y,x,709,weighted)-get_reflectance(y,x,681,weighted))))
  }
  if (index == "NDVI2")
  {
    return(return_index((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,705,weighted))/
                          (get_reflectance(y,x,750,weighted)+get_reflectance(y,x,705,weighted))))
  }
  if (index == "NDVI3")
  {
    return(return_index((get_reflectance(y,x,682,weighted)-get_reflectance(y,x,553,weighted))/
                          (get_reflectance(y,x,682,weighted)+get_reflectance(y,x,553,weighted))))
  }
  if (index == "NPCI")
  {
    return(return_index((get_reflectance(y,x,680,weighted)-get_reflectance(y,x,430,weighted))/
                          (get_reflectance(y,x,680,weighted)+get_reflectance(y,x,430,weighted))))
  }
  if (index == "OSAVI2")
  {
    return(return_index((1+0.16) * (get_reflectance(y,x,750,weighted)-get_reflectance(y,x,705,weighted))/
                          (get_reflectance(y,x,750,weighted)+get_reflectance(y,x,705,weighted)+0.16) ))
  }
  if (index == "RDVI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,670,weighted))^0.5))
  }
  # if (index == "GMAX")
  # {
  #   if (x[1] > 400) return(NULL)
  #   if (x[length(x)] < 680) return(NULL)
  #   
  #   mFR <- (get_reflectance(y,x,400,weighted)-get_reflectance(y,x,550,weighted))/(400-550)
  #   tFR <- get_reflectance(y,x,400,weighted) - mFR * 400
  # 
  #   mNIR <- (get_reflectance(y,x,570,weighted)-get_reflectance(y,x,680,weighted))/(570-680)
  #   tNIR <- get_reflectance(y,x,570,weighted) - mNIR * 570
  #   
  #   return(return_index((tNIR-tFR)/(mFR-mNIR)))
  # }
  if (index == "REP_LE")
  {
    if (x[1] > 680) return(NULL)
    if (x[length(x)] < 760) return(NULL)
    
    mFR <- (get_reflectance(y,x,680,weighted)-get_reflectance(y,x,700,weighted))/(680-700)
    tFR <- get_reflectance(y,x,680,weighted) - mFR * 680
    
    mNIR <- (get_reflectance(y,x,725,weighted)-get_reflectance(y,x,760,weighted))/(725-760)
    tNIR <- get_reflectance(y,x,725,weighted) - mNIR * 725
    
    return(return_index((tNIR-tFR)/(mFR-mNIR)))
  }
  if (index == "REP_Li")
  {
    Rre <- (get_reflectance(y,x,670,weighted)+get_reflectance(y,x,780,weighted))/2
    return(return_index(700 + 40*(((Rre - get_reflectance(y,x,700,weighted))/
                                     (get_reflectance(y,x,740,weighted)-get_reflectance(y,x,700,weighted))))))
  }
  if (index == "SIPI")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,445,weighted))/
                          (get_reflectance(y,x,800,weighted)-get_reflectance(y,x,680,weighted))))
  }
  if (index == "SPVI")
  {
    return(return_index(0.4*3.7*(get_reflectance(y,x,800,weighted)-get_reflectance(y,x,670,weighted))-
                          1.2*((get_reflectance(y,x,530,weighted)-get_reflectance(y,x,670,weighted))^2)^0.5))
  }
  if (index == "SR")
  {
    return(return_index(get_reflectance(y,x,800,weighted)/get_reflectance(y,x,680,weighted)))
  }
  if (index == "SR1")
  {
    return(return_index(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,700,weighted)))
  }
  if (index == "SR2")
  {
    return(return_index(get_reflectance(y,x,752,weighted)/get_reflectance(y,x,690,weighted)))
  }
  if (index == "SR3")
  {
    return(return_index(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,550,weighted)))
  }
  if (index == "SR4")
  {
    return(return_index(get_reflectance(y,x,700,weighted)/get_reflectance(y,x,670,weighted)))
  }
  if (index == "SR5")
  {
    return(return_index(get_reflectance(y,x,675,weighted)/get_reflectance(y,x,700,weighted)))
  }
  if (index == "SR6")
  {
    return(return_index(get_reflectance(y,x,750,weighted)/get_reflectance(y,x,710,weighted)))
  }
  if (index == "SR7")
  {
    return(return_index(get_reflectance(y,x,440,weighted)/get_reflectance(y,x,690,weighted)))
  }
  if (index == "SR8")
  {
    return(return_index(get_reflectance(y,x,515,weighted)/get_reflectance(y,x,550,weighted)))
  }
  if (index == "SRPI")
  {
    return(return_index(get_reflectance(y,x,430,weighted)/get_reflectance(y,x,680,weighted)))
  }
  if (index == "Sum_Dr1")
  {
    if (x[1] > 626) return(NULL)
    if (x[length(x)] < 795) return(NULL)
    y <- abs(y[,x>=626&x<=795])
    return(return_index(as.vector(rowSums(y))))
  }
  if (index == "Sum_Dr2")
  {
    if (x[1] > 680) return(NULL)
    if (x[length(x)] < 780) return(NULL)
    y <- y[,x>=680&x<=780]
    return(return_index(as.vector(rowSums(y))))
  }
  if (index == "TCARI")
  {
    return(return_index(3*((get_reflectance(y,x,700,weighted)-get_reflectance(y,x,670,weighted))-
                             0.2*(get_reflectance(y,x,700,weighted)-get_reflectance(y,x,550,weighted))*
                             (get_reflectance(y,x,700,weighted)/get_reflectance(y,x,670,weighted)))))
  }
  if (index == "TCARI2")
  {
    return(return_index(3*((get_reflectance(y,x,750,weighted)-get_reflectance(y,x,705,weighted))-
                             0.2*(get_reflectance(y,x,750,weighted)-get_reflectance(y,x,550,weighted))*
                             (get_reflectance(y,x,750,weighted)/get_reflectance(y,x,705,weighted)))))
  }
  if (index == "TCARI/OSAVI")
  {
    x <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(x,"TCARI",weighted=weighted)/vegindex(x,"OSAVI",weighted=weighted)))
  }
  if (index == "TCARI2/OSAVI2")
  {
    x <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(x,"TCARI2",weighted=weighted)/vegindex(x,"OSAVI2",weighted=weighted)))
  }
  if (index == "TVI")
  {
    return(return_index(0.5*(120*(get_reflectance(y,x,750,weighted)-get_reflectance(y,x,550,weighted))-
                               200*(get_reflectance(y,x,670,weighted)-get_reflectance(y,x,550,weighted)))))
  }
  if (index == "Vogelmann")
  {
    return(return_index(get_reflectance(y,x,740,weighted)/get_reflectance(y,x,720,weighted)))
  }
  if (index == "Vogelmann2")
  {
    return(return_index((get_reflectance(y,x,734,weighted)-get_reflectance(y,x,747,weighted))/
                          (get_reflectance(y,x,715,weighted)+get_reflectance(y,x,726,weighted))))
  }
  if (index == "Vogelmann3")
  {
    return(return_index(get_reflectance(y,x,715,weighted)/get_reflectance(y,x,705,weighted)))
  }
  ###########################################################OTHER INDICES######################################################
  
  if (index == "PRI")
  {
    return(return_index((get_reflectance(y,x,531,weighted)-get_reflectance(y,x,570,weighted))/
                          (get_reflectance(y,x,531,weighted)+get_reflectance(y,x,570,weighted))))
  }
  if (index == "PRI_norm")
  {
    xx <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(xx,"PRI",weighted=weighted)*(-1)/(vegindex(xx,"RDVI",weighted=weighted)*
                                                                     get_reflectance(y,x,700,weighted)/get_reflectance(y,x,670,weighted))))
  }
  if (index == "TCARI")
  {
    return(return_index(3*((get_reflectance(y,x,700,weighted)-get_reflectance(y,x,670,weighted))-
                             0.2*(get_reflectance(y,x,700,weighted)-get_reflectance(y,x,550,weighted))*
                             (get_reflectance(y,x,700,weighted)/get_reflectance(y,x,670,weighted)))))
  }
  if (index == "CAI")
  {
    return(return_index(0.5*(get_reflectance(y,x,2000,weighted)+get_reflectance(y,x,2200,weighted))-
                          get_reflectance(y,x,2100,weighted)))
  }
  if (index == "NDNI")
  {
    return(return_index((log(1/get_reflectance(y,x,1510,weighted)) - log(1/get_reflectance(y,x,1680,weighted)))/
                          (log(1/get_reflectance(y,x,1510,weighted)) + log(1/get_reflectance(y,x,1680,weighted)))))
  }
  if (index == "NDLI")
  {
    return(return_index((log(1/get_reflectance(y,x,1754,weighted)) - log(1/get_reflectance(y,x,1680,weighted)))/
                          (log(1/get_reflectance(y,x,1754,weighted)) + log(1/get_reflectance(y,x,1680,weighted)))))
  }
  
  
  
  if (index == "PARS")
  {
    return(return_index(get_reflectance(y,x,746,weighted)/get_reflectance(y,x,513,weighted)))
  }
  
  if (index == "PSSR")
  {
    return(return_index(get_reflectance(y,x,800,weighted)/get_reflectance(y,x,635,weighted)))
  }
  
  if (index == "PSND")
  {
    return(return_index((get_reflectance(y,x,800,weighted)-get_reflectance(y,x,470,weighted))/
                          (get_reflectance(y,x,800,weighted)+get_reflectance(y,x,470,weighted))))
  }
  
  if (index == "CRI1")
  {
    return(return_index(1/get_reflectance(y,x,515,weighted)-1/get_reflectance(y,x,550,weighted)))
  }
  
  if (index == "CRI2")
  {
    return(return_index(1/get_reflectance(y,x,515,weighted)-1/get_reflectance(y,x,700,weighted)))
  }
  
  if (index == "CRI3")
  {
    return(return_index(1/get_reflectance(y,x,515,weighted)-1/get_reflectance(y,x,550,weighted)*
                          get_reflectance(y,x,770,weighted)))
  }
  
  if (index == "CRI4")
  {
    return(return_index(1/get_reflectance(y,x,515,weighted)-1/get_reflectance(y,x,700,weighted)*
                          get_reflectance(y,x,770,weighted)))
  }
  
  if (index == "MPRI")
  {
    return(return_index((get_reflectance(y,x,515,weighted)-get_reflectance(y,x,530,weighted)/
                           (get_reflectance(y,x,515,weighted)+get_reflectance(y,x,530,weighted)))))
  }
  
  if (index == "PRI*CI2")
  {
    x <- speclib(spectra=y,wavelength=x)
    return(return_index(vegindex(x,"PRI",weighted=weighted)*vegindex(x,"CI2",weighted=weighted)))
  }
  
  if (index == "CI2")
  {
    return(return_index(get_reflectance(y,x,760,weighted)/get_reflectance(y,x,700,weighted)-1))
  }
  
  if (index == "PSRI")
  {
    return(return_index((get_reflectance(y,x,678,weighted)-get_reflectance(y,x,500,weighted))/
                          get_reflectance(y,x,750,weighted)))
  }
  
  if (index == "ClAInt")
  {
    if (x[1] > 600) return(NULL)
    if (x[length(x)] < 735) return(NULL)
    y <- abs(y[,x>=600&x<=735])
    return(return_index(as.vector(rowSums(y))))
  }
  if (index == "TGI")
  {
    return(return_index(-0.5*(190*(get_reflectance(y,x,670,weighted)-get_reflectance(y,x,550,weighted)) -
                                120*(get_reflectance(y,x,670,weighted)-get_reflectance(y,x,480,weighted)))))
  }
  if (substr(index, 1, 4) == "GDVI")
  {
    pow <- strsplit(index, "_")
    if (length(pow[[1]]) < 2)
    {
      pow <- 2
      warning("Exponent of GDVI missing. Use 2 for exponent")
    }
    pow <- try(as.numeric(pow[[1]][2]), silent = TRUE)
    if (inherits(pow, "try-error"))
    {
      pow <- 2
      warning("Exponent of GDVI not numeric. Use 2 for exponent")
    }
    return(return_index((get_reflectance(y,x,800,weighted)^pow-get_reflectance(y,x,680,weighted)^pow) /
                          (get_reflectance(y,x,800,weighted)^pow+get_reflectance(y,x,680,weighted)^pow)))
  }
  if (index == "LWVI1")
  {
    return(return_index((get_reflectance(y,x,1094,weighted)-get_reflectance(y,x,983,weighted)) /
                          (get_reflectance(y,x,1094,weighted)+get_reflectance(y,x,983,weighted))))
  }
  if (index == "LWVI2")
  {
    return(return_index((get_reflectance(y,x,1094,weighted)-get_reflectance(y,x,1205,weighted)) /
                          (get_reflectance(y,x,1094,weighted)+get_reflectance(y,x,1205,weighted))))
  }
  if (index == "DWSI1")
  {
    return(return_index(get_reflectance(y,x,800,weighted)/get_reflectance(y,x,1660,weighted)))
  }
  if (index == "DWSI2")
  {
    return(return_index(get_reflectance(y,x,1660,weighted)/get_reflectance(y,x,550,weighted)))
  }
  if (index == "DWSI3")
  {
    return(return_index(get_reflectance(y,x,1660,weighted)/get_reflectance(y,x,680,weighted)))
  }
  if (index == "DWSI4")
  {
    return(return_index(get_reflectance(y,x,550,weighted)/get_reflectance(y,x,680,weighted)))
  }
  if (index == "DWSI5")
  {
    return(return_index((get_reflectance(y,x,800,weighted)+get_reflectance(y,x,550,weighted)) /
                          (get_reflectance(y,x,1660,weighted)+get_reflectance(y,x,680,weighted))))
  }
  if (index == "SWIR FI")
  {
    return(return_index((get_reflectance(y,x,2133,weighted)^2) /
                          (get_reflectance(y,x,2225,weighted)*get_reflectance(y,x,2209,weighted)^3)))
  }
  if (index == "SWIR LI")
  {
    return(return_index(3.87* (get_reflectance(y,x,2210,weighted)-get_reflectance(y,x,2090,weighted)) -
                          27.51*(get_reflectance(y,x,2280,weighted)-get_reflectance(y,x,2090,weighted)) - 0.2))
  }
  if (index == "SWIR SI")
  {
    return(return_index(-41.59* (get_reflectance(y,x,2210,weighted)-get_reflectance(y,x,2090,weighted)) +
                          1.24*(get_reflectance(y,x,2280,weighted)-get_reflectance(y,x,2090,weighted)) + 0.64))
  }
  if (index == "SWIR VI")
  {
    return(return_index(37.72* (get_reflectance(y,x,2210,weighted)-get_reflectance(y,x,2090,weighted)) +
                          26.27*(get_reflectance(y,x,2280,weighted)-get_reflectance(y,x,2090,weighted)) + 0.57))
  }
  
  
  index <- gsub("R", "", gsub("(R[0-9]+)", "get_reflectance(y,x,\\1,weighted)", index, 
                              perl = TRUE)
  )
  index <- gsub("D", "", gsub("(D[0-9]+)", "get_reflectance(spectra(derivative.speclib(x_back, m=1, ...)),x,\\1,weighted)", index, 
                              perl = TRUE)
  )
  index_val <- try(return_index(eval(parse(text = index))), silent = TRUE)
  if (inherits(index_val, "try-error"))
  {
    cat("Error in self-defined index string or unimplemented index selected\n")
    cat("Index string evals to:\n")
    cat(paste(index, "\n"))
    return(NULL)
  }  
  return(index_val)
  }
