#' Wrapper function for 'op.temp.body' to loop over multiple rasters
#' @param max.rast A list providing the full path to all maximum temperature raster files. Should be of same length and order as 'min.rast'
#' @param min.rast A list providing the full path to all minimum temperature raster files. Should be of same length and order as 'max.rast'
#' @param elev Elevation raster object
#' @param J A list corresponding to the Julian day for each maximum/minimum period. Be sure that order dates matches order of file names in 'max.rast' and 'min.rast'
#' @param hours Vector of the hours each day to calculate operative body temperature
#' @param out Results can be returned as the "mean", "sd", "all" (Default = 'mean')
#' @param ... Additional parameters to be passed to \code{\link[biophys]{op.body.temp}} function.
#'
#' @usage multiple.periods(max.rast,
#' min.rast,
#' elev,
#' J,
#' hours,
#' mean = TRUE,
#' st.dev = TRUE,
#' out = "mean"
#' ...)
#'
#'
#' @examples
#' # TO BE WRITTEN
#'
#' @details This is a helper function to calculate operative body temperatures using multiple minimum and maximum temperature surfaces (e.g., monthly averages). This function directly uses \code{\link[biophys]{op.body.temp}}. Note that you should have the same numbe of minimum and maximum surfaces in each list, and they should share a common name.
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function will loop over all maximum and minimum raster surfaces to calculate monthly averages. Specified output can be (1) "mean": mean of all months; (2) "sd": standard deviation of all months; (3) "all": mean, standard deviation, and each monthly average

multiple.periods <- function(max.rast,min.rast,J,elev,hours,out="mean", ...){
  storage.stack <- stack()
  for(i in seq_along(max.rast)){
  tx <- raster(max.rast[i])
  tm <- raster(min.rast[i])
  avg.month <- op.body.temp(Tmax = tx,
                            Tmin = tm,
                            elev = elev,
                            J = J[i],
                            hours = night.hrs,
                            ...)
  storage.stack <- stack(storage.stack,avg.month)
  }
  if(out=="mean"){
    return(mean(storage.stack))
  } else if(out=="sd"){
    return(calc(storage.stack,fun = sd))
  } else {
    mn <- mean(storage.stack)
    sd <- calc(storage.stack,fun = sd)
    ret.dat <- stack(mn,sd,storage.stack)
    names(ret.dat) <- c("Mean","SD",basename(max.rast))
  }
}