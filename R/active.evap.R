#' Calculate Potential Foraging Time (PFT) based on evaporative water loss
#' @param biophys.inputs Object created from running \code{\link[biophys]{biophys.prep}}
#' @param temp.out Hourly operative body temperature and air temperature. Pass results from \code{\link[biophys]{op.temp.body}}
#'
#' @usage pft(biophys.inputs,
#' temp.out)
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
#' temp.out <- op.body.temp(biophys.input)
#'
#' PFT.out <- pft(biophys.inputs, temp.out)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function will return the hourly evaporative water loss
#' @details This function is a wrapper to call an internal function (evap.func)

pft <- function(biophys.inputs,
                temp.out
                        ) {

  evap.loss <- vector(mode = "list",length = length(months))
  pft.hr <- vector(mode = "list",length = length(biophys.inputs$hours))

  for(i in seq_along(temp.out)){
      dat.list <- temp.out[[i]]
      for(j in seq_along(dat.list[[1]])){
#         Te <- dat.list$Te[[j]]
#         Ta <- dat.list$Ta[[j]]
        PFT <- evap.func(Te = dat.list$Te[[j]],
                         Ta = dat.list$Ta[[j]],
                         biophys.inputs=biophys.inputs)

        pft.hr[[j]] <- PFT
      }
    HR <- strsplit(names(dat.list$Te), "_")
    HR <- data.frame(matrix(unlist(HR),nrow=length(HR),byrow=TRUE))
    names(pft.hr) <- paste0("PFT_",HR[,2])
    evap.loss[[i]] <- pft.hr
    } # End month loop
names(evap.loss) <- names(temp.out)
return(evap.loss)
#####################################################################

} # End PFT wrapper function

# Evaporative water loss function calculate PFT (potential foraging time)
evap.func <- function(Te, Ta, biophys.inputs) {
#   Te <- dat$Te
#   Ta <- dat$Ta

  ## Use original or Updated Conversion factor?
  unit.conv <- biophys.inputs$unit.conv

  length.m <- biophys.inputs$length.m
  pCp <- biophys.inputs$pCp
  u <- biophys.inputs$u
  RH <- biophys.inputs$RH
  max.active_temp <- biophys.inputs$max.active_temp
  min.active_temp <- biophys.inputs$min.active_temp

  mass=4599.1*length.m^2.5297 #equation to predict the mass of the salamander from the SVL

  #   pCp=1200 #Specific heat of air at constant pressure, commonly used value for biological studies (Spotila et al. 1992) in units J m^-3 C^-1

  # I made this into a matrix for my model, probably won't need to be for this implementation
  W=rep(0.1*mass,length(Te)) #amount of water loss when animal abandons foraging (fixed amount) from Feder (1983)

  hc=1.4*0.135*sqrt(u/length.m) #convection coefficient gHa1 from above

  re=0.93*(pCp/hc) #external resistance to water vapor transfer (s m^-1)

  psa=5.4509+(0.11745*Ta)+(0.024499*Ta^2) #saturation water vapor density in ambient air (kg m^-3)

  pse=5.4509+(0.11745*Te)+(0.024499*Te^2)

  if(unit.conv=="G") {
    As=(9.62*mass^0.614)*0.001 #As=surface area of the body (m^2) from Whitford and Hutcinson 1967. The 0.001 converts to mm^2
  } else {
    As=(9.62*mass^0.614)*0.0001 #As=surface area of the body (m^2) from Whitford and Hutcinson 1967. The 0.001 converts to mm^2
  }


  ##Evaporative Water Loss===================================================

  WL=(As*(1/re)*(pse-RH*psa)) #Rate of water loss (kg s^-1)

  PFT = (W/WL)/3600 #PFT in hours

  # If PFT (potential foraging time) is greater than 12hrs, assign it a value of 12 (i.e., active all night)
  PFT[PFT>12] <- 12

  # If body temp is greater than 20, assign PFT of 0
  PFT[Te>max.active_temp] <- 0

  # If body temp is less than 3, assign PFT of 0
  PFT[Te<min.active_temp] <- 0

  return(PFT)
} # Close evap function
