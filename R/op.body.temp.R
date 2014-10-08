#' Calculates operative body temperature
#' @param Tmax Maximum temperature raster object
#' @param Tmin Minimum temperature raster object
#' @param elev Elevation raster object
#' @param NoData.value Specify NoData value in raster layers (Default = -9999)
#' @param hours A vector of values. Specify hours to calculate operative body temperature over.
#' @param an.height Height of animal off the ground (Default = 0.005)
#' @param wind.height Height at which wind speed is measured (Default = 10)
#' @param u1 Measured wind speed at wind meter (Default = 1.0)
#' @param pCp Specific heat of air at constant pressure (Default = 1200)
#' @param J Julian day of month (Default = 15)
#' @param RH Relative humidity (Default = 0.95)
#' @param d1 Length of animal in meters (Default = 0.055)
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
#' NoData.value = -9999,
#' hours,
#' an.height = 0.005,
#' wind.height = 10,
#' u1 = 1.0,
#' pCp = 1200,
#' J = 15,
#' RH = 0.95,
#' d1 = 0.055,
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
#' op.temp <- op.body.temp(r.max, r.min, r.elev, hour = night.hours)
#' plot(op.temp)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function will return a raster object of operative body temperature, averaged over the specified hours.

op.body.temp <- function(Tmax,                    # Maximum and minimum temperature raster
                         Tmin,
                         elev,                    # elevation raster
                         NoData.value = -9999,    # Specify NoData value in raster layers (-9999)
                         hours,                   # Specify values as vector. e.g., c(1,2,3)
                         an.height = 0.005,       # Height of animal
                         wind.height = 10,        # Height of wind speed measure
                         u1 = 1.0,                # Measured wind speed at meter
                         pCp = 1200,              # Specific heat of air at constant pressure
                         J = 15,                  # Julian day
                         RH = 0.95,               # Relative humidity
                         d1 = 0.055,              # Dimension of salamander
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


  #Bioenergetic model for P. jordani (Energy Budget)
  #=========================================================================
  #FIXED VARIABLES
  #jordani=1, teyahalee=2
#   pCp=1200 #Specific heat of air at constant pressure, commonly used value for biological studies (Spotila et al. 1992) in units J m^-3 C^-1
#   J=15 #This is the Julian Day (generally taken to be the 15th of each month)

  # Make a raster stack
  raster.dat <- stack(Tmax, Tmin, elev)
  names(raster.dat) <- c("Tx","Tmin","elev")

  NAvalue(raster.dat) <- NoData.value # Specify the NoData value
#   plot(raster.dat) # Make sure things look right

  # Convert rasters to vectors
  Tx <- as.vector(raster.dat$Tx)
  Tmin <- as.vector(raster.dat$Tmin)
  elev <- as.vector(raster.dat$elev)

  # Remove NA values
  Tx <- Tx[!is.na(Tx)]
  Tmin <- Tmin[!is.na(Tmin)]
  elev <- elev[!is.na(elev)]

  # Combine into single data frame, filter out NoData values
#   vector.dat <- data.frame(Tx,Tmin=Tmin$Tmin,elev=elev$elev) %>% filter(!is.na(Tx),!is.na(Tmin),!is.na(elev))

  Tn=Tx-Tmin #Tn=daily temperature range (Celcius)
  Tavg=(Tx+Tmin)/2
  Tsa=(((1.39734+0.88841*Tx)+273.15)+((1.39734+0.88841*Tmin)+273.15))/2 #mean soil temperature from low and high points on transect (with adiabatic cooling rate of 0.575 degrees per 100m)
  Ad=Tn/2 #variable used in one of the time functions below

remove(Tmin) #removes Tmin matrix from memory

#######################################################
#######################################################

# Start for-loop to calculate values for each hour
bt.hr <- list() # Empty list to store hourly body temp results in
for(i in seq_along(hours)){
hr <- hours[i]
#   RH=0.95 #relative humidity (assumed to be constant); not a great assumption, but perhaps conservative
#   d1=0.055 #characteristic dimension (i.e., SVL of an adult jordani, in meters)

  mass1=4599.1*d1^2.5297 #equation to predict the mass of the salamander from the SVL

  #Day Length and Sun Angles
  #=====================================
  delta=asin(0.39795*cos(0.21631+2*atan(0.967*tan(0.0086*(-186+J))))) #delta=solar declination (in radians); J=calendar day (Jan 1 = 1)

  #Day Length
  #=====================================
  #hd=day length
  #phi=latitude (in radians)
  phi=0.623
  hd=24-(24/pi)*acos((sin(6*pi/180)+sin(phi)*sin(delta))/cos(phi)*cos(delta))

  #time of solar noon
  #=====================================
  #t0=solar noon
  f=(pi/180)*(279.575+0.9856*J)
  #ET=Equation of Time
  ET=((-104.7*sin(f))+(596.2*sin(2*f))+(4.3*sin(3*f))-(12.7*sin(4*f))-(429.3*cos(f)-(2.0*cos(2*f)+(19.3*cos(3*f)))))/3600
  #LC=correction for each degree that a location is east of the standard meridian
  #SM=standard meridian (0, 15, ...., 345 degrees) in radians??
#   SM=1.5708
  LC=0.1315*(1/15)+SM

  t0=12-LC-ET

  #Zenith angle, psi (radians), is the sun angle measured from vertical
  #psi=Zenith Angle==========psi is actually cos(psi)
  #hr=hour of day from above

  psi=acos(sin(delta)*sin(phi)+cos(delta)*cos(phi)*(cos(pi/(12*(hr-t0)))))


  #Air and Soil Temperature
  #==========================================================================

  #Measurement of air temperature at any particular hour
  #w=pi/12
  #Gamma=
  w=pi/12
  Gam=0.44-0.46*sin(0.9+(w*hr))+0.11*sin(0.9+(2*w*hr))

  #Hourly air temperature
  #Ta=air temperature
  Ta=Tx-Tn*(1-Gam)

#   remove(Tn) #remove temp range matrix from memory
#   remove(Tx)
#   remove(elev)


  #Hourly soil surface temperature
  #Ts=soil surface temperature (Kelvin)
  Ts=Tsa+Ad*sin(w*(hr-8)) #The time variable in the sine function is phase adjusted by 8 hours

  #Radiation and Environmental Temperature
#   sigma=5.67e-8 #(Stefan-Boltzmann constant)
  B=sigma*(Ta+273.15)^4 #emitted flux density (W m^2)

  # Assume gray body emissivity (no wavelength dependency)
#   e=0.93
#   es=0.96
  eac=9.2e-6*(Ta+273.15)^2 #clear sky emissivity (approximation from Swinbank 1963)

  #Modeling wind speed at animal level (0.5 cm)
  aheight=rep(an.height, length(Ta)) #height of animal in meters
  z=rep(wind.height, length(Ta)) #height of wind speed measurement in meters
  ds=0.65*aheight
  zm=0.1*aheight
#   u1=1.0 #measured wind speed at measurement height (from instrument)

  ustar=0.4*(u1/(log((z-ds)/zm))) #friction velocity
  u=(ustar/0.4)*log(aheight*(1-0.65)/zm) #wind speed at height of interest (m s^-1)

  remove(z)
  remove(aheight)
  remove(ds)
  remove(zm)


#   cp=29.3 #specific heat of air
  gHa1=1.4*0.135*sqrt(u/d1) #boundary conductance of air (mol/square meter/second)-species1

  gr=(4*e*sigma*(Ta+273.15)^3)/cp #radiative conductance (mol/square meter/second)

  #Longwave Component
  ##############################################
  La=eac*sigma*(Ta+273.15)^4 #longwave flux density from atmosphere (W/square meter)
  Lg=es*sigma*Ts^4 #longwave flux density from the ground (W/square meter)

#   alphaS=0.93 #absorptivity in solar waveband
#   alphaL=0.96 #absorptivity in thermal waveband
#   Fp=0 #  ctor (ratio of projected area perpendicular to solar beam) assumed zero because of nocturnal activity
  Fd=rep(Fd.c, length(Ta)) #estimated for a standing lizard (Bartlett and Gates 1967)
  Fr=rep(Fr.c, length(Ta))  # I made these matrices for the calculations (would need to change these to constants)
  Fa=rep(Fa.c, length(Ta))  # these are just estimated proportions of the animal exposed to different types of temperature exchange
  Fg=rep(Fg.c, length(Ta))


  #Absorbed Energy
  #################################################
  Rabs=alphaL*(Fa*La+Fg*Lg) #Only conduction!!!!!!!

  #Radiation emitted
  Remit1=(es*sigma*(Ta-273.15)^4)

  #Operative body temperature
  #############################################
  Te1=Ta+((Rabs-Remit1)/cp*(gr+gHa1))

  # Store hourly results
    bt.hr[[i]] <- Ta+((Rabs-Remit1)/cp*(gr+gHa1))
  } # Close hours loop

# Get average of hourly values
op.temp <- rowMeans(simplify2array(bt.hr))

# Put results back into raster
op.temp_rast <- raster.dat[[1]]

tmp <- as.vector(raster.dat$Tx)
tmp <- replace(tmp,which(tmp!=NoData.value),op.temp)
op.temp_rast <- setValues(op.temp_rast,tmp)
} # End function

