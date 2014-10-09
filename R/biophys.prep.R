#' Preparation function to compile biophysical model constants and input.
#' @param Tmax Maximum temperature raster object
#' @param Tmin Minimum temperature raster object
#' @param elev Elevation raster object
#' @param Julian Julian day (1 - 365)
#' @param hours A vector of values. Specify hours to calculate operative body temperature over.
#' @param out If desired, the results of this function can be saved to the workspace as either a "list", RasterStack object ("raster"), or each hourly raster surface can be saved to a directory by specifying a path. (Default = 'list').
#' @param grid.type Arguement specifying type of grid to export (Default =".asc"). Only needed if a directory path is specified for 'out'
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
#'
#' @usage op.body.temp(Tmax,
#' Tmin,
#' elev,
#' Julian,
#' hours,
#' out = 'list',
#' grid.type = ".asc",
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
#' Fg.c = 0.5)
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
#' biophys.input <- biophys.prep(r.max, r.min, r.elev, hour = night.hours, export = NULL)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function will return a raster object of operative body temperature, averaged over the specified hours.

biophys.prep <- function(Tmax,                    # Maximum and minimum temperature raster
                         Tmin,
                         elev,                    # elevation raster
                         NoData.value = -9999,    # Specify NoData value in raster layers (-9999)
                         hours,                   # Specify values as vector. e.g., c(1,2,3)
                         out = 'list',            # Directory where hourly raster files will be written
                         grid.type = ".asc",      # Arguement specifying type of grid to export
                         an.height = 0.005,       # Height of animal
                         wind.height = 10,        # Height of wind speed measure
                         u = 1.0,                # Measured wind speed at meter
                         pCp = 1200,              # Specific heat of air at constant pressure
                         Julian = 15,                  # Julian day
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
                         Fg.c = 0.5
) {
out <- list(Tmax = Tmax,
            Tmin = Tmin,
            elev = elev,
            NoData.value = NoData.value,
            hours = hours,
            out = out,
            grid.type = grid.type,
            an.height = an.height,
            wind.height = wind.height,
            u = u,
            pCp = pCp,
            J = J,
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
            Fg.c = Fg.c)
}