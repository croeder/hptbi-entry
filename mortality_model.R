################################################################################
# User Mortality Model
# user model function, returns the model
# Args:
#   data a data.frame resulting from a call to prepare_mortality_data()
# Return:
#   a model
#
user_mortality_model <- function(data) {
    glm(mortality ~ age + female + gcs_use + icpyn1  + gcsed + decomcranyn +
                           cardiacarrested + cardiacarrestor + cardiacarrestprehosp +
			               cardiacarrestyn + cardiacarresticu + cardiacarrestother,
        data = data,
        family = binomial())
}

################################################################################
# Mortality Model
#
# Args:
#   data a data.frame resulting from a call to prepare_mortality_data()
#
# Return:
#   An R object.  This object will have the "hackathon_mortality_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#
mortality_model <- function(data) {
  rtn <- user_mortality_model(data)
  class(rtn) <- c("hackathon_mortality_model", class(rtn))
  rtn
}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_mortality_model
#
# Args:
#   object  a hackathon_mortality_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A character vector of length equal to the nrow(newdata) with values
#   "Mortality" and "Alive"
#
predict.hackathon_mortality_model <- function(object, newdata, ...) {
  
  ##############################################################################
  # User Defined data preparation code starts here

  p <- stats::predict.glm(object, newdata, type = "response", ...)
  ifelse(p > 0.25, "Mortality", "Alive")

}

################################################################################
#                                 End of File
################################################################################
