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

user_prepare_mortality_data <- function(df) {

  # deal with a possible missing value in icpyn1
  if (any(df$icpyn1)) {
    # if all information about type of monitor is missing then mark icpyn1 as 0
    flags <- as.integer( !( (df$icptype1 == "" | is.na(df$icptype1)) &
                          (df$icptype2 == "" | is.na(df$icptype2)) &
                          (df$icptype3 == "" | is.na(df$icptype3)) ))

    idx <- which(is.na(df$icpyn1))
    df$icpyn1[idx] <- flags[idx]
  }

  # missing values in 3 GCS-ED fields
  # gcseyeed
  if (any(df$gcseyeed)) {
    na_idx <- which(is.na(df$gcseyeed))
    df$gcseyeed[na_idx] <- 0
  }
  # gcsverbaled
  if (any(df$gcsverbaled)) {
    na_idx <- which(is.na(df$gcsverbaled))
    df$gcsverbaled[na_idx] <- 0
  }
  # gcsmotored
  if (any(df$gcsmotored)) {
    na_idx <- which(is.na(df$gcsmotored))
    df$gcsmotored[na_idx] <- 0
  }

  # Deal with possible missing values in gcsed, after having corrected possible NAs in input.
  #  sum gcseyeed + gcsverbaled + gcsmotored (from DD)
  if (any(df$gcsed)) {
    gcsed_values <-
      df$gcseyeed  +
      df$gcsverbaled +
      df$gcsmotored 
    na_idx <- which(is.na(df$gcsed))
    df$gcsed[na_idx] <- gcsed_values[na_idx]
  }

  # GCS-ICU
  # ...gcseyeicu
  if (any(df$gcseyeicu)) {
    na_idx <- which(is.na(df$gcseyeicu))
    df$gcseyeicu[na_idx] <- 0
  }
  # gcsverbalicu
  if (any(df$gcsverbalicu)) {
    na_idx <- which(is.na(df$gcsverbalicu))
    df$gcsverbalicu[na_idx] <- 0
  }
  # gcsmotoricu
  if (any(df$gcsmotoricu)) {
    na_idx <- which(is.na(df$gcsmotoricu))
    df$gcsmotoricu[na_idx] <- 0
  }

  # gcsicu
  #  sum gcseyeicu + gcsverbalicu + gcsmotoricu (from DD)
  if (any(df$gcsicu)) {
    gcsicu_values <-
      df$gcseyeicu  +
      df$gcsverbalicu +
      df$gcsmotoricu 
    na_idx <- which(is.na(df$gcsicu))
    df$gcsicu[na_idx] <- gcsicu_values[na_idx]
  }

  # decomcranyn
  if (any(df$decomcranyn)) {
    na_idx <- which(is.na(df$decomcranyn))
    df$decomcranyn[na_idx] <- 0
  }

  # derive this value only once the inputs are fixed
  df$gcs_use <- ifelse(is.na(df$gcsed),
                    yes = df$gcsicu,
                    no  = df$gcsed)

    df
}

organizer_prepare_mortality_data <- function(hackathon_mortality_data) {
  # Define the primary outcome
  hackathon_mortality_data$mortality <-
    as.integer(hackathon_mortality_data$hospdisposition == "Mortality")

  # Omit FSS elements - FSS is omitted from this data set.  FSS could not be
  # assessed for patients who died.  To reduce confusion FSS related elements
  # are omitted as missing values for FSS are be highly correlated with
  # mortality.
  hackathon_mortality_data <-
    hackathon_mortality_data[-grep("fss", names(hackathon_mortality_data))]

  return(hackathon_mortality_data)
}

# This reads and prepares the model with the hackathon organizer's code
prepare_mortality_data <- function(training = TRUE) {

  # import the data set
  if (!training & file.exists("./csvs/testing.csv")) {
    hackathon_mortality_data <- read.csv(file = "./csvs/testing.csv")
  } else {
    hackathon_mortality_data <- read.csv(file = "./csvs/training.csv")
  }

  hackathon_mortality_data <- organizer_prepare_mortality_data(hackathon_mortality_data)
  user_prepare_mortality_data(hackathon_mortality_data)
}

################################################################################
#                                 End of File
################################################################################
