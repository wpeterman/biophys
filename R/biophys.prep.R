#' Preparation function to compile biophysical model constants and input.
#' @param Tmax Maximum temperature raster object. Alternatively, a list of full paths to rasters can be supplied (see Details). The order of list elements must be them same for Tmin, month, and Julian.
#' @param Tmin Minimum temperature raster object. Alternatively, a list of full paths to rasters can be supplied (see Details). The order of list elements must be them same for Tmax, month, and Julian.
#' @param elev Elevation raster object
#' @param month A vector of names to assign to the specified raster layer(s). If multiple months are supplied, the order must be them same as Tmax, Tmin, and Julian.
#' @param Julian Julian day (1 - 365). If multiple months are supplied, the order of Julian must be them same as Tmax, Tmin, and month.
#' @param hours A vector of values. Specify hours to calculate operative body temperature over.
#' @param days A vector containing the number of days in each month. Order must correspond with order of months. If unspecified, each month/period will consist of 30 days.
#' @param max.active_temp Maximum temprature that animals will be active (Default = 20)
#' @param min.active_temp Minimum temperature that animals will be active (Default = 3)
#' @param NoData.value Specify NoData value in raster layers (Default = -9999)
#' @param an.height Height of animal off the ground (Default = 0.005)
#' @param wind.height Height at which wind speed is measured (Default = 10)
#' @param u Measured wind speed at wind meter (Default = 1.0)
#' @param pCp Specific heat of air at constant pressure (Default = 1200)
#' @param RH Relative humidity (Default = 0.95)
#' @param length.m Length of animal in meters (Default = 0.055)
#' @param phi Latitude in radians (Default = 0.623)
#' @param SM Standard meridian (Default = 1.5708)
#' @param sigma Stefan-Boltzmann constant (Default = 5.67e-8)
#' @param e Gray body emissivity (Default = 0.93)
#' @param es Gray body emissivity (Default = 0.96)
#' @param cp Specific heat of air (Default = 29.3)
#' @param alphaS Absorptivity in solar waveband (Default = 0.93)
#' @param alphaL Absorptivity in thermal waveband (Default = 0.96)
#' @param Fp ctor: ratio of projected area perpendicular to solar beam. (Default = 0)
#' @param Fd.c Fd constant (Default = 0.8)
#' @param Fa.c Fa constant (Default = 0.5)
#' @param Fr.c Fr constant (Default = 0.5)
#' @param Fg.c Fg constant (Default = 0.5)
#' @param repro Energy (kJ) in 14 egg clutch (eggs 3.5 mm diameter). (Default = 4.86)
#'
#' @usage op.body.temp(Tmax,
#' Tmin,
#' elev,
#' month = NULL,
#' Julian,
#' hours,
#' days = NULL,
#' max.active_temp,
#' min.active_temp,
#' NoData.value = -9999,
#' an.height = 0.005,
#' wind.height = 10,
#' u = 1.0,
#' pCp = 1200,
#' RH = 0.95,
#' length.m = 0.055,
#' phi = 0.623,
#' SM = 1.5708,
#' sigma = 5.67e-8,
#' e = 0.93,
#' es = 0.96,
#' cp = 29.3,
#' alphaS = 0.93,
#' alphaL = 0.96,
#' Fp = 0,
#' Fd.c = 0.8,
#' Fa.c = 0.5,
#' Fr.c = 0.5,
#' Fg.c = 0.5,
#' repro = 4.86)
#'
#' @examples
#' # Create example data
#' r.max <- raster(ncol=10, nrow=10)
#'
#' # assign values to cells
#' r.max[] <- runif(ncell(r.max),10,20)
#' r.min <- r.max - 5
#' r.elev <- r.max * 10
#'
#' # Make vector of hours
#' night.hours <- c(19:24,1:7)
#'
#' biophys.input <- biophys.prep(r.max, r.min, r.elev, hour = night.hours, month = c("April"), Julian = c(106))
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function returns a list object with the necessary inputs for all other biophys functions.
#' @details Full paths to raster files can be obtain using \code{\link[base]{list.files}}.
#'
#'  For example: Tmax.files <- list.files(path = "C:/RasterFiles/", pattern = "*max", full.names = TRUE)
#'
#'  Assuming raster files are stored in a folder called 'RasterFiles', this create the full path to files that include 'max' in the filename. It is advised keep all raster files in a folder separate from other files, and to have a common naming scheme for both minimum and maximum surfaces.

biophys.prep <- function(Tmax,                    # Maximum and minimum temperature raster
                         Tmin,
                         elev,                    # elevation raster
                         month = NULL,            # Name(s) of specified raster layers
                         Julian,                  # Julian day
                         hours,                   # Specify values as vector. e.g., c(1,2,3)
                         days = NULL,                    # A vector of numbers corresponding to the days in each month
                         max.active_temp = 20,    # Maximum body temperature of active animas
                         min.active_temp = 3,     # Minimum body temperature of active animals
                         NoData.value = -9999,    # Specify NoData value in raster layers (-9999)
                         an.height = 0.005,       # Height of animal
                         wind.height = 10,        # Height of wind speed measure
                         u = 1.0,                 # Measured wind speed at meter
                         pCp = 1200,              # Specific heat of air at constant pressure
                         RH = 0.95,               # Relative humidity
                         length.m = 0.055,        # Dimension of salamander
                         phi = 0.623,             # Latitude in radians
                         SM = 1.5708,             # Standard meridian
                         sigma = 5.67e-8,         # Stefan-Boltzmann constant
                         e = 0.93,                # Gray body emissivity
                         es = 0.96,
                         cp = 29.3,               # Specific heat of air
                         alphaS=0.93,             # Absorptivity in solar waveband
                         alphaL=0.96,             # Absorptivity in thermal waveband
                         Fp=0,                    # ctor (ratio of projected area perpendicular to solar beam)
                         Fd.c = 0.8,              # constants for Fd, Fa, Fr, Fg
                         Fa.c = 0.5,
                         Fr.c = 0.5,
                         Fg.c = 0.5,
                         repro = 4.86             # Energy (kJ) in 14 egg clutch (eggs 3.5 mm diameter)
) {
  Tmax.dat <- Tmax
  Tmin.dat <- Tmin

  layers <- ifelse(class(Tmax.dat)!='RasterLayer',length(Tmax.dat),1)
  Tavg <- vector(mode = "list",length = layers)
  for(i in 1:layers){
    if(layers>1){
      Tx <- raster(Tmax.dat[i])
      Tm <- raster(Tmin.dat[i])
      raster.dat <- stack(Tx, Tm)
      names(raster.dat) <- c("Tx","Tmin")
    } else {
      raster.dat <- stack(Tmax.dat, Tmin.dat)
      names(raster.dat) <- c("Tx","Tmin")
    }

  NAvalue(raster.dat) <- NoData.value # Specify the NoData value
#   plot(raster.dat) # Make sure things look right

  # Convert rasters to vectors
  tx <- as.vector(raster.dat[[1]])
  tm <- as.vector(raster.dat[[2]])

  # Remove NA values
  tx <- tx[!is.na(tx)]
  tm <- tm[!is.na(tm)]

  Tavg[[i]] <- (tx+tm)/2

  } # Close layer loop

raster.dat2 = list(
              rast.extent = extent(raster.dat[[1]]),
              rast.res = res(raster.dat[[1]]),
              full.vector = as.vector(raster.dat[[1]]))

if(is.null(month)){
  month <- paste0("month",seq_along(Tavg))
}

if(is.null(days)){
  days <- rep(30,length(month))
}

names(Tavg) <- names(month)
vector.length <- length(tx)

# if(class(Tmax)=='RasterLayer'){
#     raster.dat = list(
#               rast.extent = extent(Tmax),
#               rast.res = res(Tmax),
#               full.vector = as.vector(Tmax))
#   } else {
#     r <- raster(Tmax[1])
#     raster.dat = list(
#               rast.extent = extent(r),
#               rast.res = res(r),
#               full.vector = as.vector(r))
#   }

out <- list(Tmax = Tmax,
            Tmin = Tmin,
            elev = elev,
            month = month,
            Julian = Julian,
            hours = hours,
            days = days,
            max.active_temp = max.active_temp,
            min.active_temp = min.active_temp,
            NoData.value = NoData.value,
            an.height = an.height,
            wind.height = wind.height,
            u = u,
            pCp = pCp,
            RH = RH,
            length.m = length.m,
            phi = phi,
            SM = SM,
            sigma = sigma,
            e = e,
            es = es,
            cp = cp,
            alphaS=alphaS,
            alphaL=alphaL,
            Fp=Fp,
            Fd.c = Fd.c,
            Fa.c = Fa.c,
            Fr.c = Fr.c,
            Fg.c = Fg.c,
            repro = repro,
            Tavg = Tavg,
            raster.dat = raster.dat2,
            vector.length = vector.length
            )
}