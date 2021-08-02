################################################################################
# Prepare FSS Data
#
# Define data procesing steps to apply to the data set used to train and test
# models for predicting total FSS.
#
# Args:
#   training  (logicial) if the data set to read in is the training or testing
#             data set.
#
# Return:
#   A data.frame with the defnined primary outcome and any user specific
#   elements needed for training and testing their model.
#
prepare_fss_data <- function(training = TRUE) {

  # import the data set
  if (!training & file.exists("./csvs/testing.csv")) {
    hackathon_fss_data <- read.csv(file = "./csvs/testing.csv")
  } else {
    hackathon_fss_data <- read.csv(file = "./csvs/training.csv")
  }

  # Define the primary outcome -- do not edit this.  If you need the outcome in
  # a different format, e.g., integer or logical, create an additional
  # data.frame element in user defined code section below.
  hackathon_fss_data$fss_total <-
    Reduce(function(x, y) {x + y},
           x = hackathon_fss_data[grep("^fss", names(hackathon_fss_data))])

  # subset to known FSS values
  hackathon_fss_data <-
    subset(hackathon_fss_data, hospdisposition != "Mortality")
  hackathon_fss_data <-
    subset(hackathon_fss_data, !is.na(fss_total))

  ##############################################################################
  # User Defined data preperation code starts here


  # deal with a possible missing value in icpyn1
  if (any(hackathon_fss_data$icpyn1)) {
    # if all information about type of monitor is missing then mark icpyn1 as 0
    flags <-
      as.integer( !( (hackathon_fss_data$icptype1 == "" | is.na(hackathon_fss_data$icptype1)) &
                     (hackathon_fss_data$icptype2 == "" | is.na(hackathon_fss_data$icptype2)) &
                     (hackathon_fss_data$icptype3 == "" | is.na(hackathon_fss_data$icptype3)) ))

    idx <- which(is.na(hackathon_fss_data$icpyn1))
    hackathon_fss_data$icpyn1[idx] <- flags[idx]
  }

  # 3x GCS - ED
  # ...gcseyeed
  if (any(hackathon_fss_data$gcseyeed)) {
    na_idx <- which(is.na(hackathon_fss_data$gcseyeed))
    hackathon_fss_data$gcseyeed[na_idx] <- 1
  }
  # ...gcsverbaled
  if (any(hackathon_fss_data$gcsverbaled)) {
    na_idx <- which(is.na(hackathon_fss_data$gcsverbaled))
    hackathon_fss_data$gcsverbaled[na_idx] <- 1
  }
  # ...gcsmotored
  if (any(hackathon_fss_data$gcsmotored)) {
    na_idx <- which(is.na(hackathon_fss_data$gcsmotored))
    hackathon_fss_data$gcsmotored[na_idx] <- 1
  }

  # Deal with possible missing values in gcsed, after having corrected possible NAs in input.
  #  sum gcseyeed + gcsverbaled + gcsmotored (from DD)
  if (any(hackathon_fss_data$gcsed)) {
    gcsed_values <-
      hackathon_fss_data$gcseyeed  +
      hackathon_fss_data$gcsverbaled +
      hackathon_fss_data$gcsmotored 
    na_idx <- which(is.na(hackathon_fss_data$gcsed))
    hackathon_fss_data$gcsed[na_idx] <- gcsed_values[na_idx]
  }

  # 3x GCS - ICU
  # ...gcseyeicu
  if (any(hackathon_fss_data$gcseyeicu)) {
    na_idx <- which(is.na(hackathon_fss_data$gcseyeicu))
    hackathon_fss_data$gcseyeicu[na_idx] <- 1
  }
  # ...gcsverbalicu
  if (any(hackathon_fss_data$gcsverbalicu)) {
    na_idx <- which(is.na(hackathon_fss_data$gcsverbalicu))
    hackathon_fss_data$gcsverbalicu[na_idx] <- 1
  }
  # ...gcsmotoricu
  if (any(hackathon_fss_data$gcsmotoricu)) {
    na_idx <- which(is.na(hackathon_fss_data$gcsmotoricu))
    hackathon_fss_data$gcsmotoricu[na_idx] <- 1
  }

  # ...gcsicu
  #  sum gcseyeicu + gcsverbalicu + gcsmotoricu (from DD)
  if (any(hackathon_fss_data$gcsicu)) {
    gcsicu_values <-
      hackathon_fss_data$gcseyeicu  +
      hackathon_fss_data$gcsverbalicu +
      hackathon_fss_data$gcsmotoricu 
    na_idx <- which(is.na(hackathon_fss_data$gcsicu))
    hackathon_fss_data$gcsicu[na_idx] <- gcsicu_values[na_idx]
  }

  # ...decomcranyn
  if (any(hackathon_fss_data$decomcranyn)) {
    na_idx <- which(is.na(hackathon_fss_data$decomcranyn))
    hackathon_fss_data$decomcranyn[na_idx] <- 1
  }

  # derive this value only once the inputs are fixed
  hackathon_fss_data$gcs_use <-
    ifelse(is.na(hackathon_fss_data$gcsed),
           yes = hackathon_fss_data$gcsicu,
           no  = hackathon_fss_data$gcsed)

  # User Defined Code ends here
  ##############################################################################

  hackathon_fss_data
}

################################################################################
#                                 End of File
################################################################################
