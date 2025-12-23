#' Preprocess Pancreatic Cancer Data
#'
#' Preprocesses raw pancreatic cancer data by handling missing values,
#' encoding categorical variables, and performing feature engineering
#' including creatinine normalization.
#'
#' @param data A data frame containing pancreatic cancer data. Must contain
#'   columns: age, sex, creatinine, LYVE1, REG1B, TFF1. REG1A is optional.
#' @param normalize Logical indicating whether to normalize biomarker values
#'   by creatinine. Default is TRUE.
#' @param fill_missing Logical indicating whether to fill missing values.
#'   Default is TRUE.
#'
#' @return A preprocessed data frame with additional normalized features.
#'
#' @examples
#' # Create a sample dataset
#' sample_data <- data.frame(
#'   age = c(65, 70, 55),
#'   sex = c("M", "F", "M"),
#'   creatinine = c(0.8, 1.2, 0.9),
#'   LYVE1 = c(2.1, 3.4, 2.8),
#'   REG1B = c(5.6, 7.8, 6.2),
#'   TFF1 = c(1.2, 1.5, 1.3)
#' )
#'
#' # Preprocess the data with default settings
#' preprocessed_data <- preprocess_pancreatic_data(sample_data)
#'
#' # Preprocess without normalization
#' preprocessed_data_no_norm <- preprocess_pancreatic_data(
#'   sample_data,
#'   normalize = FALSE
#' )
#'
#' @export
preprocess_pancreatic_data <- function(data, normalize = TRUE, fill_missing = TRUE) {
  # Check for required columns
  required_cols <- c("age", "sex", "creatinine", "LYVE1", "REG1B", "TFF1")
  missing_cols <- setdiff(required_cols, names(data))

  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Create a copy to avoid modifying original
  processed <- data

  # 1. Process gender encoding
  if(is.character(processed$sex) || is.factor(processed$sex)) {
    processed$sex <- ifelse(toupper(as.character(processed$sex)) == "M", 1, 0)
  }

  # 2. Feature engineering: Creatinine normalization
  if(normalize) {
    processed$LYVE1_norm <- processed$LYVE1 / (processed$creatinine + 0.001)
    processed$REG1B_norm <- processed$REG1B / (processed$creatinine + 0.001)
    processed$TFF1_norm  <- processed$TFF1  / (processed$creatinine + 0.001)
  }

  # 3. Handle missing REG1A (optional feature)
  if(!"REG1A" %in% names(processed)) {
    processed$REG1A <- 0
    warning("REG1A not found in data. Setting all values to 0.")
  }

  # 4. Handle missing values in numeric columns
  if(fill_missing) {
    numeric_cols <- names(processed)[sapply(processed, is.numeric)]

    for(col in numeric_cols) {
      if(any(is.na(processed[[col]]))) {
        median_val <- median(processed[[col]], na.rm = TRUE)
        processed[[col]] <- ifelse(is.na(processed[[col]]), median_val, processed[[col]])
        message("Filled missing values in column '", col, "' with median: ", median_val)
      }
    }
  }

  return(processed)
}

#' Load Trained Pancreatic Cancer Model
#'
#' Loads a pre-trained Random Forest model for pancreatic cancer prediction.
#' If no model is provided, attempts to load the built-in model.
#'
#' @param model_path Path to the saved model file (RDS format). If NULL,
#'   uses the built-in model included with the package.
#'
#' @return A trained caret model object of class 'train'.
#'
#' @export
load_pancreatic_model <- function(model_path = NULL) {
  if(is.null(model_path)) {
    # Try to load built-in model from package data
    model_file <- system.file("data", "pancreatic_model.rds",
                              package = "PancreaticCancerPredictor")

    if(file.exists(model_file)) {
      model <- readRDS(model_file)
      message("Loaded built-in pancreatic cancer model")
      return(model)
    } else {
      # Create synthetic model for demonstration
      warning("Built-in model not found. Creating synthetic model for demonstration.")
      return(create_synthetic_model())
    }
  } else {
    # Load user-provided model
    if(file.exists(model_path)) {
      model <- readRDS(model_path)
      message("Model loaded successfully from: ", model_path)
      return(model)
    } else {
      stop("Model file not found at: ", model_path)
    }
  }
}

#' Predict Pancreatic Cancer Risk for Multiple Patients
#'
#' Uses a trained model to predict pancreatic cancer risk for multiple patients
#' simultaneously. The function handles data preprocessing and feature engineering
#' before making predictions.
#' @param input_data Data frame containing patient features
#' @param model Trained machine learning model (optional)
#' @param threshold Probability threshold for classifying as cancer. Default is 0.25
#' @param preprocess Logical indicating whether to preprocess input data. Default is TRUE
#'
#' @return A data frame with original features and additional prediction columns
#'
#' @export
predict_pancreatic_cancer <- function(input_data, model = NULL, threshold = 0.25,
                                      preprocess = TRUE) {
  # Load model if not provided
  if(is.null(model)) {
    model <- load_pancreatic_model()
  }

  # Check model class
  if(!inherits(model, "train")) {
    warning("Model object may not be a caret 'train' object. Results may be unpredictable.")
  }

  # Preprocess data if requested
  if(preprocess) {
    processed_data <- preprocess_pancreatic_data(input_data)
  } else {
    processed_data <- input_data
  }

  # Get model's expected features
  model_features <- if(!is.null(model$finalModel$xNames)) {
    model$finalModel$xNames
  } else {
    c("age", "sex", "creatinine", "LYVE1", "REG1B", "TFF1", "REG1A",
      "LYVE1_norm", "REG1B_norm", "TFF1_norm")
  }

  # Ensure all required features are present
  missing_features <- setdiff(model_features, names(processed_data))
  if(length(missing_features) > 0) {
    for(feature in missing_features) {
      processed_data[[feature]] <- 0
    }
    warning("Missing features set to 0: ", paste(missing_features, collapse = ", "))
  }

  # Ensure correct order of features
  features_for_prediction <- processed_data[, model_features, drop = FALSE]

  # Ensure all columns are numeric
  features_for_prediction <- features_for_prediction %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  # Make predictions
  tryCatch({
    predictions <- stats::predict(model, features_for_prediction, type = "prob")

    # Extract cancer probability
    if("Cancer" %in% colnames(predictions)) {
      cancer_prob <- predictions[, "Cancer"]
    } else if(ncol(predictions) == 2) {
      # Assume second column is cancer probability
      cancer_prob <- predictions[, 2]
    } else if("Class1" %in% colnames(predictions)) {
      # Some models use generic names
      cancer_prob <- predictions[, "Class1"]
    } else {
      # Use first column as reference
      cancer_prob <- predictions[, 1]
      warning("Cancer probability column not identified. Using first column of predictions.")
    }
  }, error = function(e) {
    stop("Prediction failed: ", e$message)
  })

  # Apply threshold
  predicted_status <- ifelse(cancer_prob > threshold, "Cancer", "Control")

  # Calculate confidence
  confidence <- ifelse(predicted_status == "Cancer",
                       cancer_prob,
                       1 - cancer_prob)
  library(stats)

  # Determine risk level
  risk_level <- cut(cancer_prob,
                    breaks = c(0, 0.3, 0.7, 1),
                    labels = c("Low", "Medium", "High"),
                    include.lowest = TRUE)

  # Create results
  results <- processed_data
  results$Cancer_Probability <- round(cancer_prob, 4)
  results$Predicted_Status <- factor(predicted_status, levels = c("Control", "Cancer"))
  results$Confidence <- round(confidence, 4)
  results$Risk_Level <- risk_level

  return(results)
}

#' Predict Pancreatic Cancer Risk for a Single Patient
#'
#' Provides a convenient interface for predicting pancreatic cancer risk
#' for a single patient. Wraps individual parameters into a data frame and
#' calls the batch prediction function.
#'
#' @param age Patient's age
#' @param sex Patient's sex (M/F)
#' @param creatinine Creatinine level
#' @param LYVE1 LYVE1 biomarker value
#' @param REG1B REG1B biomarker value
#' @param TFF1 TFF1 biomarker value
#' @param REG1A REG1A biomarker value (optional, default 0)
#' @param model Trained machine learning model (optional)
#' @param threshold Probability threshold for classifying as cancer. Default is 0.25
#'
#' @return A list containing prediction details including cancer probability,
#'         predicted status, confidence, risk level, and normalized features
#'
#' @export
predict_single_patient <- function(age, sex, creatinine, LYVE1, REG1B, TFF1,
                                   REG1A = 0, model = NULL, threshold = 0.25) {
  # Create input data frame
  input_df <- data.frame(
    age = as.numeric(age),
    sex = ifelse(toupper(as.character(sex)) %in% c("M", "1", "MALE"), 1, 0),
    creatinine = as.numeric(creatinine),
    LYVE1 = as.numeric(LYVE1),
    REG1B = as.numeric(REG1B),
    TFF1 = as.numeric(TFF1),
    REG1A = as.numeric(REG1A),
    stringsAsFactors = FALSE
  )

  # Make prediction
  prediction <- predict_pancreatic_cancer(input_df, model, threshold)

  # Extract results
  result <- list(
    cancer_probability = prediction$Cancer_Probability[1],
    predicted_status = as.character(prediction$Predicted_Status[1]),
    confidence = prediction$Confidence[1],
    risk_level = as.character(prediction$Risk_Level[1]),
    normalized_features = list(
      LYVE1_norm = prediction$LYVE1_norm[1],
      REG1B_norm = prediction$REG1B_norm[1],
      TFF1_norm = prediction$TFF1_norm[1]
    ),
    threshold_used = threshold
  )

  return(result)
}

