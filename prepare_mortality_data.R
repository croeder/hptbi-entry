################################################################################
# Prepare Mortality Data
#
# Define data processing steps to apply to the data set used to train and test
# models for predicting mortality.
#
# Args:
#   training  (logical) if the data set to read in is the training or testing
#             data set.
#
# Return:
#   A data.frame with the defined primary outcome and any user specific
#   elements needed for training and testing their model.
#
prepare_mortality_data <- function(training = TRUE) {

  # import the data set
  if (!training & file.exists("./csvs/testing.csv")) {
    hackathon_mortality_data <- read.csv(file = "./csvs/testing.csv")
  } else {
    hackathon_mortality_data <- read.csv(file = "./csvs/training.csv")
  }

  # Define the primary outcome -- do not edit this.  If you need the outcome in
  # a different format, e.g., integer or logical, create an additional
  # data.frame element in user defined code section below.
  hackathon_mortality_data$mortality <-
    as.integer(hackathon_mortality_data$hospdisposition == "Mortality")

  # Omit some elements - FSS is omitted from this data set.  FSS could not be
  # assessed for patients who died.  To reduce confusion FSS related elements
  # are omitted as missing values for FSS are be highly correlated with
  # mortality.
  hackathon_mortality_data <-
    hackathon_mortality_data[-grep("fss", names(hackathon_mortality_data))]

  ##############################################################################
  # User Defined Code starts here


  # deal with a possible missing value in icpyn1
  if (any(hackathon_mortality_data$icpyn1)) {
    # if all information about type of monitor is missing then mark icpyn1 as 0
    flags <-
      as.integer( !( (hackathon_mortality_data$icptype1 == "" | is.na(hackathon_mortality_data$icptype1)) &
                     (hackathon_mortality_data$icptype2 == "" | is.na(hackathon_mortality_data$icptype2)) &
                     (hackathon_mortality_data$icptype3 == "" | is.na(hackathon_mortality_data$icptype3)) ))

    idx <- which(is.na(hackathon_mortality_data$icpyn1))
    hackathon_mortality_data$icpyn1[idx] <- flags[idx]
  }

  # ...gcseyeed
  if (any(hackathon_mortality_data$gcseyeed)) {
    na_idx <- which(is.na(hackathon_mortality_data$gcseyeed))
    hackathon_mortality_data$gcseyeed[na_idx] <- 0
  }
  # ...gcsverbaled
  if (any(hackathon_mortality_data$gcsverbaled)) {
    na_idx <- which(is.na(hackathon_mortality_data$gcsverbaled))
    hackathon_mortality_data$gcsverbaled[na_idx] <- 0
  }
  # ...gcsmotored
  if (any(hackathon_mortality_data$gcsmotored)) {
    na_idx <- which(is.na(hackathon_mortality_data$gcsmotored))
    hackathon_mortality_data$gcsmotored[na_idx] <- 0
  }

  # Deal with possible missing values in gcsed, after having corrected possible NAs in input.
  #  sum gcseyeed + gcsverbaled + gcsmotored (from DD)
  if (any(hackathon_mortality_data$gcsed)) {
    gcsed_values <-
      hackathon_mortality_data$gcseyeed  +
      hackathon_mortality_data$gcsverbaled +
      hackathon_mortality_data$gcsmotored 
    na_idx <- which(is.na(hackathon_mortality_data$gcsed))
    hackathon_mortality_data$gcsed[na_idx] <- gcsed_values[na_idx]
  }

  # ...decomcranyn
  if (any(hackathon_mortality_data$decomcranyn)) {
    na_idx <- which(is.na(hackathon_mortality_data$decomcranyn))
    hackathon_mortality_data$decomcranyn[na_idx] <- 0
  }

  # ...gcsicu
  #  DD mentions gcseyecu + gcsverbalicu + gcsmotoricu.  # This round proclaim ignorance.
  if (any(hackathon_mortality_data$gcsicu)) {
    na_idx <- which(is.na(hackathon_mortality_data$gcsicu))
    hackathon_mortality_data$gcsicu[na_idx] <- 0
  }


  # derive this value only once the inputs are fixed
  hackathon_mortality_data$gcs_use <-
    ifelse(is.na(hackathon_mortality_data$gcsed),
           yes = hackathon_mortality_data$gcsicu,
           no  = hackathon_mortality_data$gcsed)

  

  # User Defined Code ends here
  ##############################################################################

  hackathon_mortality_data
}

################################################################################
#                                 End of File
################################################################################
