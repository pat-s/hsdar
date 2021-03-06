list.available.sensors <- function(returnHelp = FALSE)
{
  ########################################################################
  ##
  ##   Neue Sensoren IMMER am ENDE der jeweiligen Vektoren hinzufuegen !
  ##   Alle Werte muessen in [nm] angegeben werden!
  ##
  ########################################################################
  available <- c("MODIS",
                 "RapidEye",
                 "WorldView2-8",
                 "Landsat5",
                 "ALI",
                 "Hyperion",
                 "Quickbird",
                 "WorldView2-4",
                 "Landsat4",
                 "Landsat7",
                 "EnMAP",
                 "Landsat8",
                 "Sentinel2"
                 )
  if (returnHelp)
  {
    reponse_function_available <- vector(mode="logical", length=0)
    for (i in 1:length(available))
    {
      reponse_function_available <- c(reponse_function_available,
                                      !is.null(get_response_function(available[i])))
    }
    sensor <- data.frame(a=c(1:length(available))[order(available)],b=reponse_function_available[order(available)])
    row.names(sensor) <- sort(available)
    names(sensor) <- c("Numerical abbreviation", "Response function implemented")
    return(sensor)
  } else {
    return(available)
  }
}




#' Sensor characteristics
#' 
#' Get channel wavelength of satellite sensor
#' 
#' 
#' @aliases get.sensor.characteristics list.available.sensors
#' @param sensor Character or integer. Name or numerical abbreviation of
#' sensor. See 'sensor="help"' or 'sensor=0' for an overview of available
#' sensors.
#' @param response_function If TRUE, the spectral response function is returned
#' @author Lukas Lehnert
#' @seealso [spectralResampling()]
#' @keywords utilities
#' @examples
#' 
#' ## Return implemented sensors
#' get.sensor.characteristics(0)
#' 
#' ## RapidEye
#' data_wv <- get.sensor.characteristics("RapidEye", TRUE)
#' 
#' ## Plot response functions
#' plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
#'      ylab = "Spectral response")
#' xwl_response <- seq.int(attr(data_wv$response, "minwl"),
#'                         attr(data_wv$response, "maxwl"),
#'                         attr(data_wv$response, "stepsize"))
#' for (i in 1:nrow(data_wv$characteristics))
#'   lines(xwl_response, data_wv$response[,i], col = i)
#' 
#' @export get.sensor.characteristics
get.sensor.characteristics <- function (
                                         sensor,
                                         response_function=FALSE
                                       )
{
  
  if (sensor=="help" | sensor == 0) 
  {
    return(list.available.sensors(TRUE))
  } else {
    available <- list.available.sensors(FALSE)
  }
  
  if (is.numeric(sensor)) sensor <- available[sensor]
    
  if (is.na(sensor)) 
  {
    warning("Sensor not defined! Try 'get.sensor.characteristics(\"help\")'\n  for an overview of available sensors")
    return(NULL)
  }
    
  found  <- FALSE
  center <- NA
  fwhm   <- NA
  lb     <- NA
  ub     <- NA
  if (sensor==available[1]) # MODIS
  {
    lb <- c(620, 841, 459, 545, 1230, 1628, 2105, 405, 438, 483, 526, 546, 662, 673, 743, 862, 890, 931, 915)
    ub <- c(670, 876, 479, 565, 1250, 1652, 2155, 420, 448, 493, 536, 556, 672, 683, 753, 877, 920, 941, 965)
    nch <- 19
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  if (sensor==available[2]) # RapidEye
  {
    lb <- c(440,520,630,690,760)
    ub <- c(510,590,685,730,850)
    nch <- 5
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }  
  if (sensor==available[3]) # WorldView2 (8 bands)
  {
    lb <- c(0.401,0.448,0.511,0.589,0.629,0.704,0.772,0.862)*1000
    ub <- c(0.453,0.508,0.581,0.627,0.689,0.744,0.89,0.954)*1000
    nch <- 8
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  if (sensor==available[4]) # Landsat 5
  {
    lb <- c(450, 520, 630, 760, 1550, 2080)
    ub <- c(520, 600, 690, 900, 1750, 2350)
    nch <- 6
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  if (sensor==available[5]) # ALI
  {
    lb <- c(433, 450, 525, 630, 775, 845, 1200, 1550, 2080)
    ub <- c(453, 525, 605, 690, 805, 890, 1300, 1750, 2350)
    nch <- 9
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[6]) # Hyperion
  {
    lb<-c(351,361,371,381,391,401,412,422,432,442,452,463,473,483,493,503,513,524,534,544,554,564,
          574,585,595,605,615,625,636,646,656,666,676,686,697,707,717,727,737,747,758,768,778,788,
          798,808,819,829,839,847,849,857,859,867,870,877,880,887,890,897,900,907,910,918,920,928,
          931,938,941,948,951,958,961,968,971,978,981,988,992,998,1002,1008,1012,1018,1022,1028,
          1032,1039,1043,1049,1053,1059,1069,1079,1089,1099,1109,1119,1129,1139,1150,1160,1170,
          1180,1190,1200,1210,1220,1230,1240,1250,1261,1271,1281,1291,1301,1311,1321,1331,1341,
          1351,1361,1372,1382,1392,1402,1412,1422,1432,1442,1452,1462,1472,1483,1493,1503,1513,
          1523,1533,1543,1553,1563,1573,1583,1594,1604,1614,1624,1634,1644,1654,1664,1674,1684,
          1694,1705,1715,1725,1735,1745,1755,1765,1775,1785,1795,1805,1815,1826,1836,1846,1856,
          1866,1876,1886,1896,1906,1916,1926,1937,1947,1957,1967,1977,1987,1997,2007,2017,2027,
          2037,2047,2058,2068,2078,2088,2098,2108,2118,2128,2138,2148,2158,2169,2179,2189,2199,
          2209,2219,2229,2239,2249,2259,2269,2280,2290,2300,2310,2320,2330,2340,2350,2360,2370,
          2380,2391,2401,2411,2421,2431,2441,2451,2461,2471,2481,2491,2501,2512,2522,2532,2542,
          2552,2562,2572)
    ub<-c(361,371,381,391,401,411,422,432,442,452,462,473,483,493,503,513,523,534,544,554,564,574,
          584,595,605,615,625,635,646,656,666,676,686,696,707,717,727,737,747,757,768,778,788,798,
          808,818,829,839,849,857,859,867,869,877,880,887,890,897,900,907,910,917,920,928,930,938,
          941,948,951,958,961,968,971,978,981,988,991,998,1002,1008,1012,1018,1022,1028,1032,1038,
          1042,1049,1053,1059,1063,1069,1079,1089,1099,1109,1119,1129,1139,1149,1160,1170,1180,
          1190,1200,1210,1220,1230,1240,1250,1260,1271,1281,1291,1301,1311,1321,1331,1341,1351,
          1361,1371,1382,1392,1402,1412,1422,1432,1442,1452,1462,1472,1482,1493,1503,1513,1523,
          1533,1543,1553,1563,1573,1583,1593,1604,1614,1624,1634,1644,1654,1664,1674,1684,1694,
          1704,1715,1725,1735,1745,1755,1765,1775,1785,1795,1805,1815,1825,1836,1846,1856,1866,
          1876,1886,1896,1906,1916,1926,1936,1947,1957,1967,1977,1987,1997,2007,2017,2027,2037,
          2047,2057,2068,2078,2088,2098,2108,2118,2128,2138,2148,2158,2168,2179,2189,2199,2209,
          2219,2229,2239,2249,2259,2269,2279,2290,2300,2310,2320,2330,2340,2350,2360,2370,2380,
          2390,2401,2411,2421,2431,2441,2451,2461,2471,2481,2491,2501,2511,2522,2532,2542,2552,
          2562,2572,2582)
    nch <- length(lb)
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[7]) # Quickird
  {
    lb <- c(450, 520, 630, 760)
    ub <- c(520, 600, 690, 900)
    nch <- 4
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[8]) # WorldView2 (4 bands)
  {
    lb <- c(450,585,630,770)
    ub <- c(510,625,690,895)
    nch <- 4
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[9]) # Landsat 4
  {
    lb <- c(450, 520, 630, 760, 1550, 2080)
    ub <- c(520, 600, 690, 900, 1750, 2350)
    nch <- 6
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[10]) # Landsat 7
  {
    lb <- c(450, 530, 630, 780, 1550, 2090)
    ub <- c(520, 610, 690, 900, 1750, 2350)
    nch <- 6
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[11]) # EnMAP
  {
    center <- c(423.0300, 428.8000, 434.2900, 439.5800, 444.7200, 
                449.7500, 454.7000, 459.5900, 464.4300, 469.2500, 
                474.0500, 478.8400, 483.6300, 488.4200, 493.2300, 
                498.0500, 502.9000, 507.7700, 512.6700, 517.6000, 
                522.5700, 527.5800, 532.6300, 537.7200, 542.8700, 
                548.0600, 553.3000, 558.6000, 563.9500, 569.3600, 
                574.8300, 580.3600, 585.9500, 591.6000, 597.3200, 
                603.1000, 608.9500, 614.8600, 620.8400, 626.9000, 
                633.0200, 639.2100, 645.4700, 651.8000, 658.2000, 
                664.6700, 671.2100, 677.8300, 684.5100, 691.2600, 
                698.0800, 704.9700, 711.9200, 718.9500, 726.0300, 
                733.1900, 740.4000, 747.6800, 755.0100, 762.4100, 
                769.8600, 777.3700, 784.9300, 792.5400, 800.2000, 
                807.9100, 815.6700, 823.4600, 831.3000, 839.1800, 
                847.1000, 855.0500, 863.0300, 871.0500, 879.0900, 
                887.1600, 895.2500, 903.3600, 911.4901, 919.6400, 
                927.8000, 935.9800, 944.1700, 952.3700, 960.5700, 
                968.7800, 976.9900, 985.2100, 904.7800, 914.4400, 
                924.2300, 934.1600, 944.2300, 954.4200, 964.7400,
                975.1700, 985.7300, 996.4000, 1007.2000, 1018.1000, 
                1029.1000, 1040.2000, 1051.3000, 1062.6000, 1074.0000, 
                1085.4000, 1096.9000, 1108.5000, 1120.1000, 1131.8000, 
                1143.5000, 1155.3000, 1167.1000, 1179.0000, 1190.9000, 
                1202.8000, 1214.8000, 1226.7000, 1238.7001, 1250.7000, 
                1262.7000, 1274.7001, 1286.7000, 1298.7000, 1310.7001,
                1322.7001, 1334.7000, 1346.6001, 1358.5000, 1370.3999,
                1382.3000, 1394.2000, 1406.0000, 1417.7999, 1429.6000, 
                1441.3000, 1453.0000, 1464.6000, 1476.3000, 1487.8000, 
                1499.4000, 1510.9000, 1522.3000, 1533.7000, 1545.1000, 
                1556.3999, 1567.7001, 1578.9000, 1590.1001, 1601.2000, 
                1612.3000, 1623.2999, 1634.3000, 1645.3000, 1656.2001, 
                1667.0000, 1677.8000, 1688.5000, 1699.2001, 1709.9000, 
                1720.5000, 1731.0000, 1741.5000, 1752.0000, 1762.4000, 
                1772.7000, 1783.0000, 1793.3000, 1803.5000, 1813.7000, 
                1823.7999, 1833.7999, 1843.9000, 1853.8000, 1863.7001,
                1873.6000, 1883.5000, 1893.2001, 1903.0000, 1912.7001, 
                1922.2999, 1931.9000, 1941.5000, 1951.0000, 1960.5000, 
                1969.9000, 1979.3000, 1988.7001, 1998.0000, 2007.2000, 
                2016.4001, 2025.6000, 2034.8000, 2043.9000, 2052.9001, 
                2061.8999, 2070.8999, 2079.9001, 2088.8000, 2097.6001, 
                2106.3999, 2115.2000, 2124.0000, 2132.7000, 2141.3000, 
                2150.0000, 2158.6001, 2167.0999, 2175.7000, 2184.2000, 
                2192.6001, 2201.0000, 2209.3999, 2217.7998, 2226.0999,
                2234.4001, 2242.5999, 2250.7998, 2259.0000, 2267.2000,
                2275.3000, 2283.4001, 2291.3999, 2299.4001, 2307.3999,
                2315.3999, 2323.2998, 2331.2000, 2339.0999, 2346.8999,
                2354.7002, 2362.5000, 2370.2000, 2377.8999, 2385.6001, 
                2393.3000, 2400.8999, 2408.5000, 2416.1001, 2423.5999,
                2431.0999, 2438.6001)
    fwhm_data <- c(6.925900, 6.584700, 6.343100, 6.167100, 6.036200,
              5.938900, 5.867600, 5.816100, 5.780800, 5.758800,
              5.748100, 5.746500, 5.753300, 5.767300, 5.787500,
              5.813400, 5.844500, 5.880200, 5.919700, 5.963500,
              6.010700, 6.061300, 6.114700, 6.171300, 6.230600,
              6.292500, 6.356500, 6.422400, 6.491600, 6.561500,
              6.633700, 6.707500, 6.783400, 6.859900, 6.938300,
              7.017500, 7.098500, 7.179700, 7.262000, 7.345000, 
              7.429100, 7.512800, 7.597000, 7.681700, 7.766000,
              7.850500, 7.934800, 8.018500, 8.101700, 8.184099,
              8.265901, 8.346900, 8.426300, 8.505100, 8.581900,
              8.657599, 8.731400, 8.803700, 8.874100, 8.942200,
              9.008700, 9.072700, 9.134700, 9.194200, 9.251200,
              9.306200, 9.358100, 9.408000, 9.454800, 9.499100,
              9.540800, 9.579800, 9.616199, 9.649699, 9.680699, 
              9.709100, 9.735100, 9.757899, 9.778600, 9.796900, 
              9.812400, 9.825601, 9.836400, 9.844999, 9.851501, 
              9.855400, 9.857901, 9.857500, 11.592000, 11.755000, 
              11.917000, 12.075001, 12.230000, 12.380000, 12.527000, 
              12.669000, 12.807000, 12.938999, 13.066999, 13.190001, 
              13.306001, 13.417000, 13.522000, 13.622000, 13.715000, 
              13.802000, 13.882000, 13.957001, 14.026000, 14.088000, 
              14.145000, 14.195000, 14.240001, 14.279000, 14.312000, 
              14.340000, 14.362000, 14.379999, 14.393000, 14.400000,
              14.403999, 14.402000, 14.397000, 14.388000, 14.375000, 
              14.358000, 14.337001, 14.315000, 14.288000, 14.259001, 
              14.227000, 14.192000, 14.156000, 14.117000, 14.076000, 
              14.033000, 13.988000, 13.942000, 13.894000, 13.845000, 
              13.794999, 13.743000, 13.690001, 13.637000, 13.582000,
              13.528000, 13.472000, 13.415000, 13.359000, 13.301000, 
              13.243000, 13.186000, 13.127000, 13.069000, 13.010000, 
              12.950999, 12.892000, 12.833000, 12.775001, 12.715000, 
              12.657000, 12.598000, 12.540000, 12.481000, 12.423000, 
              12.365001, 12.307000, 12.249001, 12.191999, 12.135000, 
              12.078000, 12.021999, 11.965000, 11.908999, 11.854000, 
              11.799000, 11.744000, 11.689000, 11.635000, 11.581000,
              11.528000, 11.474999, 11.422000, 11.370001, 11.318000, 
              11.267000, 11.216000, 11.165000, 11.114000, 11.065000, 
              11.014999, 10.966001, 10.917000, 10.869000, 10.821000,
              10.773001, 10.726000, 10.679000, 10.632000, 10.586000,
              10.541000, 10.495000, 10.450000, 10.405001, 10.361000, 
              10.316999, 10.273999, 10.231000, 10.188001, 10.146001, 
              10.104000, 10.061000, 10.020000, 9.979300, 9.938400, 
              9.897900, 9.857901, 9.818299, 9.778700, 9.739900, 
              9.700700, 9.662499, 9.624200, 9.586599, 9.548900,
              9.511700, 9.474800, 9.438100, 9.402000, 9.365600,
              9.330000, 9.294600, 9.259400, 9.224300, 9.189900,
              9.155300, 9.121300, 9.087501, 9.053900, 9.020700, 
              8.987400, 8.954901)
    nch <- length(fwhm)
    found <- TRUE
    fwhm <- TRUE
    if (response_function) response <- get_response_function(sensor)
  }
  if (sensor==available[12]) # Landsat 8
  {
    lb <- c(427, 435, 512, 625, 830, 1340, 1515, 2037)
    ub <- c(462, 530, 602, 685, 897, 1405, 1697, 2352)
    nch <- 8
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (sensor==available[13]) # Sentinel 2A
  {
    centre <- c(443,490,560,665,705,740,783,842,865,945,1375,1610,2190)
    width  <- c(20,65,35,30,15,15,20,115,20,20,30,90,180)
    
    lb <- centre - width/2
    ub <- centre + width/2
    nch <- length(lb)
    found <- TRUE
    fwhm <- FALSE
    if (response_function) response <- get_response_function(sensor)
  }
  
  if (!found) 
  {
    warning("Sensor not defined! Try 'get.sensor.characteristics(\"help\")'\n  for an overview of available sensors")
    return(NULL)
  } else {
    if (fwhm)
    {
      result <- data.frame(channel=c(1:nch),center=center,fwhm=fwhm_data)
    } else {
      result <- data.frame(channel=c(1:nch),lb=lb,ub=ub)
      attr(result, "50pass") <- c(2,3)
    }
    attr(result, "fwhm") <- fwhm
    if (response_function)
    {
      response <- get_response_function(sensor)
      if (is.null(response)) stop(paste("Response function for '",sensor,"' not implemented",sep=""))
      return(list(characteristics=result, response=response))
    } else {
      return(result)
    }
  }
}



#' Satellite sensor name
#' 
#' Get satellite sensor name by integer value
#' 
#' See [get.sensor.characteristics()] to get overview on available
#' satellite sensors.
#' 
#' @param sensor Integer value to match against predefined satellite sensors.
#' @return Name of satellite sensor as character string.
#' @author Lukas Lehnert
#' @seealso [get.sensor.characteristics()]
#' @keywords utilities
#' @examples
#' 
#' get.sensor.name(1)
#' 
#' @export get.sensor.name
get.sensor.name <- function(sensor)
{
  if (!is.numeric(sensor)) return (sensor)
  avl <- get.sensor.characteristics(0)
  sensor <- row.names(avl)[which(avl[,1]==sensor)]
  if (length(sensor)==1) 
  {
    return(sensor)
  } else {
    return(NULL)
  }
}

