#' Calculate active and resting metabolic rates, digestive efficiency, and necessary food consumption
#' @param biophys.inputs Object created from running \code{\link[biophys]{biophys.prep}}
#' @param temp.out Hourly operative body temperature and air temperature. Pass results from \code{\link[biophys]{op.temp.body}}
#'
#' @usage metabolism(biophys.inputs,
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
#' metabolism.out <- metabolism(biophys.inputs, temp.out)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return This function returns hourly active and resting metabolic rates, digestive efficiency, and food consumption
#' @details This function is a wrapper to call an internal function (metab.func)


## METABOLISM===============================================================
metabolism <- function(biophys.inputs, temp.out){

  # Unpack inputs
  length.m <- biophys.inputs$length.m
  max.active_temp <- biophys.inputs$max.active_temp
  min.active_temp <- biophys.inputs$min.active_temp

  # Calcualte constants
  mass = 4599.1*length.m^2.5297 # Equation to predict the mass of the salamander from the SVL

  all.metab <- vector(mode = "list",length = length(months))
  mr_active <- vector(mode = "list",length = length(biophys.inputs$hours))
  mr_inactive <- vector(mode = "list",length = length(biophys.inputs$hours))
  dig.eff <- vector(mode = "list",length = length(biophys.inputs$hours))
  food.consump <- vector(mode = "list",length = length(biophys.inputs$hours))

# Wrapper function for metabolism
  for(i in seq_along(temp.out)){
      Te.list <- temp.out[[i]]$Te
      Tavg <- biophys.inputs$Tavg[[i]]
      for(j in seq_along(Te.list)){

        metab.list <- metab.func(Te = Te.list[[j]],
                                 Tavg = Tavg,
                                 mass,
                                 max.active_temp,
                                 min.active_temp )

        mr_active[[j]] <- metab.list$mr_act
        mr_inactive[[j]] <- metab.list$mr_inact
        dig.eff[[j]] <- metab.list$DE
        food.consump[[j]] <- metab.list$food

      } # Close hour loop (j)

    HR <- strsplit(names(Te.list), "_")
    HR <- data.frame(matrix(unlist(HR),nrow=length(HR),byrow=TRUE))
    names(mr_active) <- paste0("mr.act_",HR[,2])
    names(mr_inactive) <- paste0("mr.inact_",HR[,2])
    names(dig.eff) <- paste0("de_",HR[,2])
    names(food.consump) <- paste0("food_",HR[,2])

    # Combine all metabolism lists
    ml <- list(mr_active = mr_active,
               mr_inactive = mr_inactive,
               dig.eff = dig.eff,
               food.consump = food.consump)

    all.metab[[i]] <- ml
    } # End month loop (i)

  names(all.metab) <- names(temp.out)
  return(all.metab)

} # End metabolism wrapper function


# Function to calculate metabolic rates, digestive efficiency, and food consumption
# Function to calculate metabolic rates, digestive efficiency, and food consumption
metab.func <- function(Te, Tavg, mass, max.active_temp, min.active_temp, m1 = NULL, m2 = NULL, m3 = NULL ){

  mr_act <- rep(0, length(Tavg)) # MetRate in kJ/hr

  ## If values specified for m1-m3, use below
  if(!is.null(m1)) {
    # Active metabolic rates only for times when temperatures are suitable for potential activity
    mr_act[Te<max.active_temp] <- (10^((m1*Te[Te<max.active_temp]+
                                          (m2*log10(mass))-m3))*
                                     0.0056*0.001*3600*1.5)

    # If below minimum temp, set to zero
    mr_act[Te<min.active_temp] <- 0


    #Metabolic rates for times of inactivity. Temperature for inactive MR taken as nighttime average
    mr_inact <- 10^((m1*Tavg+(m2*log10(mass))-m3))*0.0056*0.001*3600
  } else {

    # Values used to convert oxygen consumprion to kJ/hr and multiplied by a factor of 1.5 for activity
    # Active metabolic rates only for times when temperatures are suitable for potential activity
    mr_act[Te<max.active_temp] <- (10^((0.035*Te[Te<max.active_temp]+
                                          (0.368*log10(mass))-1.844))*
                                     0.0056*0.001*3600*1.5)

    # If below minimum temp, set to zero
    mr_act[Te<min.active_temp] <- 0


    #Metabolic rates for times of inactivity. Temperature for inactive MR taken as nighttime average
    mr_inact <- 10^((0.035*Tavg+(0.368*log10(mass))-1.844))*0.0056*0.001*3600
  }

  # Digestive Efficiency (Bobka et al.)
  DE <- -0.0094*Te+0.9903 # Dependent on body temperature

  # Equation calculating energy consumption (less percentage determined by digestive efficiency function) on an hourly basis
  food <- (((0.0149*Te^3-0.8119*Te^2+12.756*Te-43.06)*mass*0.004184)*DE)/24

  # Combine all lists
  m.out <- list(mr_act = mr_act,
                mr_inact = mr_inact,
                DE = DE,
                food = food)
  return(m.out)
}