#' Calculate annual energy budget
#' @param biophys.inputs Object created from running \code{\link[biophys]{biophys.prep}}
#' @param PFT.out Hourly potential foraging times. Pass results from \code{\link[biophys]{pft}}
#' @param metabolism.out Hourly metabolic measures. Pass results from \code{\link[biophys]{metabolism}}
#'
#' @usage budget(biophys.inputs,
#' PFT.out,
#' metabolism.out)
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
#' PFT.out <- pft(biophys.inputs, temp.out)
#' metabolism.out <- metabolism(biophys.inputs, temp.data)
#'
#' annual.budget <- budget(biophys.inputs, PFT.out, metabolism.out)
#' plot(annual.budget$Raster.results)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return A list that containing a raster stack that includes (1) annual energy budget; (2) total annual metabolic rate; (3) annual potential foraging time; (4) total annual consumption, as well as the annual values in vector format
#' @details This function summarizes hourly measures to calculate average consumption, average metabolic rate, average potential foraging time, and total metabolic expenditure.


budget <- function(biophys.inputs, PFT.out, metabolism.out){

  PFT <- vector(mode = "list",length = length(PFT.out))
  M.Act <- vector(mode = "list",length = length(PFT.out))
  M.Inact <- vector(mode = "list",length = length(PFT.out))
  DE <- vector(mode = "list",length = length(PFT.out))
  Food <- vector(mode = "list",length = length(PFT.out))

  # Loop over each month
  for(i in seq_along(PFT.out)){

    PFT[[i]] <- list.mean(PFT.out[[i]])
    M.Act[[i]] <- list.mean(metabolism.out[[i]]$mr_active)
    M.Inact[[i]] <- list.mean(metabolism.out[[i]]$mr_inactive)
    DE[[i]] <- list.mean(metabolism.out[[i]]$dig.eff)
    Food[[i]] <- list.mean(metabolism.out[[i]]$food.consump)

  } # Close month loop


  #Need to take average of consumption over a day (12 hrs of potential activity), This is the average of the hourly consumption calculations
  #Need to take average of active metabolic rate over a day (12 hrs of potential activity), This is the average of the hourly active metabolic rate calculations
  #Need to take average of PFT over a day as above
  #Need to calculate the total metabolic expenditure taking into account the active MR, inactive MR and PFT (see equation below)

  metab.day <- vector(mode = "list", length = length(PFT))
  food.day <- vector(mode = "list", length = length(PFT))
  EB.month <- vector(mode = "list", length = length(PFT))

  # Annual data lists
  metab.month <- vector(mode = "list", length = length(PFT))
  food.month <- vector(mode = "list", length = length(PFT))
  PFT.month <- vector(mode = "list", length = length(PFT))

  # Calculate for each month
  for(i in seq_along(PFT)){
    metab.day[[i]] <- (M.Act[[i]]*PFT[[i]])+(M.Inact[[i]]*(24-PFT[[i]]))

    #Need to calculate the total consumption taking into account PFT (see equation below)
    food.day[[i]] <- Food[[i]]*PFT[[i]]

    #Finally, need to calculate the monthly energy budget (EB)
    days <- biophys.inputs$days #number of days in the month
    EB.month[[i]] <- (food.day[[i]]-metab.day[[i]])*days[i]

    # Get other annual measures
    metab.month[[i]] <- (metab.day[[i]] * days[i])
    food.month[[i]] <- (food.day[[i]] * days[i])
    PFT.month[[i]] <- (PFT[[i]] * days[i])


  } # End month loop

  names(EB.month) <- biophys.inputs$month
  names(metab.month) <- biophys.inputs$month
  names(food.month) <- biophys.inputs$month
  names(PFT.month) <- biophys.inputs$month

    #At the very end, we need to calculate the Annual Energy Budget (over two years), and subtract out the energy cost of a clutch of eggs
#     repro=4.86 #energy (kJ) in 14 egg clutch (eggs 3.5 mm diameter)

    # Annual Discretionary Energy AFTER Reproduction
    annual.EB <- (rowSums(data.frame(EB.month))*2)-biophys.inputs$repro

    # Other annual summaries
    annual.metab <- (rowSums(data.frame(metab.month)))
    annual.food <- (rowSums(data.frame(food.month)))
    annual.PFT <- (rowSums(data.frame(PFT.month)))

    # Set annual energy budget cells <=0 to NA
    annual.EB[annual.EB<=0] <- NA

    # Make list of all original annual measures
    Annual.results <- list(Energy.Budget = annual.EB,
                           metabolism = annual.metab,
                           food = annual.food,
                           PFT = annual.PFT)

    # Recompile to raster files
      vector.full <- biophys.inputs$raster.dat$full.vector
      NoData.value <- biophys.inputs$NoData.value
      r <- raster(biophys.inputs$raster.dat$rast.extent)
      res(r) <- biophys.inputs$raster.dat$rast.res
      r.stack <- stack()

          for(i in seq_along(Annual.results)){
            tmp <- replace(vector.full,which(vector.full!=NoData.value),Annual.results[[i]])
            temp_r <- setValues(r,tmp)
            r.stack <- stack(r.stack,temp_r)
          }

      # Name layers
      names(r.stack) <- names(Annual.results)

# Final data to export
out <- list(Raster.results = r.stack,
            Annual.results = Annual.results)

return(out)

} # End function

# Helper function to calcualte mean of hourly data
list.mean <- function(x){rowMeans(data.frame(x))}

