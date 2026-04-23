# Statistical Modeling Functions
# Functions for fitting models and extracting results

#' Fit income category model using quantile mixed effects
#' @param data Analysis dataset
#' @param geography_control Include geography as a control variable
#' @param imputed Include clarification if it's the imputed dataset 
#' @return Model summary object
fit_income_model <- function(data, geography_control = FALSE) {
  message("Fitting income model...")
  
  # Build formula
  if (geography_control == TRUE) {
    
    model <- lqmm(fixed = Normalized_Energy ~ HICDD + Median_Income_cat + 
                    Avg_Household_Size + HICDD*Median_Income_cat + geography,
                  random = ~ 1,
                  group = factor(groups_county),
                  data = data,
                  tau = 0.5,
                  weights = sqrt(data$Households),
                  control = lqmm::lqmmControl(method = "df", 
                                              UP_max_iter = 6000, 
                                              LP_max_iter = 3000,
                                              UP_tol = 1e-4,
                                              LP_tol_ll = 1e-4, LP_tol_theta = 1e-4))
    
  } else {
    
    model <- lqmm(fixed = Normalized_Energy ~ HICDD + Median_Income_cat + 
                    Avg_Household_Size + HICDD*Median_Income_cat,
                  random = ~ 1,
                  group = factor(groups_county),
                  data = data,
                  tau = 0.5,
                  weights = sqrt(data$Households),
                  control = lqmm::lqmmControl(method = "df", 
                                              UP_max_iter = 6000, 
                                              LP_max_iter = 3000,
                                              UP_tol = 1e-4,
                                              LP_tol_ll = 1e-4, LP_tol_theta = 1e-4))
  }
  
  return(summary(model))
  
}
# fit_income_model <- function(data, geography_control = FALSE, imputed = FALSE) {
#   message("Fitting income model...")
#   
#   # Build formula
#   if (geography_control == TRUE && imputed == FALSE) {
#     
#     model <- lqmm(fixed = Normalized_Energy ~ HICDD + Median_Income_cat + 
#            Avg_Household_Size + HICDD*Median_Income_cat + geography,
#          random = ~ 1,
#          group = factor(groups_county),
#          data = data,
#          tau = 0.5,
#          weights = sqrt(data$Households),
#          control = lqmm::lqmmControl(method = "df", 
#                                UP_max_iter = 6000, 
#                                LP_max_iter = 3000,
#                                UP_tol = 1e-4,
#                                LP_tol_ll = 1e-4, LP_tol_theta = 1e-4))
#     
#   } else if(geography_control==FALSE && imputed == FALSE){
#     
#     model <- lqmm(fixed = Normalized_Energy ~ HICDD + Median_Income_cat + 
#                     Avg_Household_Size + HICDD*Median_Income_cat,
#                   random = ~ 1,
#                   group = factor(groups_county),
#                   data = data,
#                   tau = 0.5,
#                   weights = sqrt(data$Households),
#                   control = lqmm::lqmmControl(method = "df", 
#                                         UP_max_iter = 6000, 
#                                         LP_max_iter = 3000,
#                                         UP_tol = 1e-4,
#                                         LP_tol_ll = 1e-4, LP_tol_theta = 1e-4))
#     
#     } else {
#       model <- lqmm(fixed = Normalized_Energy ~ HICDD + Median_Income_cat + 
#                       Avg_Household_Size + HICDD*Median_Income_cat,
#                     random = ~ 1,
#                     group = factor(groups_county),
#                     data = data,
#                     tau = 0.5,
#                     control = lqmm::lqmmControl(method = "df", 
#                                           UP_max_iter = 6000, 
#                                           LP_max_iter = 6000,
#                                           UP_tol = 1e-4,
#                                           LP_tol_ll = 1e-4, LP_tol_theta = 1e-4))
#   }
#   
#  
#  return(summary(model))
#     
# }

#' Fit income model using linear mixed effects
#' @param data Analysis dataset
#' @param weights Optional regression weights
#' @return Model summary object
#' Fit income model using linear mixed effects
#' @param data Analysis dataset
#' @return Model summary object
fit_income_model_lmer <- function(data) {
  message("Fitting linear mixed effects model...")

  # Fit model
  tryCatch({
    model <- lmer(
      Normalized_Energy ~ HICDD + Median_Income_cat +
        Avg_Household_Size + HICDD*Median_Income_cat + as.factor(geography) +
        (1 | groups_county),
      data = data,
      weights = sqrt(Households)
    )

    message("  Model converged successfully")
    return(model)

  }, error = function(e) {
    message("  Model fitting failed: ", e$message)
    return(NULL)
  })
}

#' Compile model results into a formatted table
#' @param model_list List of model summaries
#' @param model_type Type of models (income or ice)
#' @param scale_factor Factor to scale coefficients
#' @return Formatted results table
#' 
compile_model_results <- function(model_list, model_type, scale_factor = 1000 * 245.57) {
  message("Compiling ", model_type, " model results...")
  
  # Define terms of interest
  if (model_type == "income") {
    terms_of_interest <- c(
      "HICDD:Median_Income_cat50k-70k",
      "HICDD:Median_Income_cat70k-90k",
      "HICDD:Median_Income_cat>90k"
    )
    term_labels <- c(
      "HICDD: $50,000-69,999",
      "HICDD: $70,000-89,999",
      "HICDD: ≥$90,000"
    )
  } else if (model_type == "ice") {
    terms_of_interest <- c(
      "HICDD:ICE_black",
      "HICDD:ICE_hisp",
      "HICDD:ICE_asian"
    )
    term_labels <- c(
      "Black",
      "Hispanic/Latino",
      "Asian"
    )
  } else if (model_type == "ice_income") {
    terms_of_interest <- c(
      "HICDD:ICE_income_Black",
      "HICDD:ICE_income_Hispanic",
      "HICDD:ICE_income_Asian"
    )
    term_labels <- c(
      "Black",
      "Hispanic/Latino",
      "Asian"
    )
  }
  
  # Extract results from each model
  results_list <- map(names(model_list), function(model_name) {
    model <- model_list[[model_name]]
    
    if (is.null(model)) {
      return(NULL)
    }
    
    # Extract coefficients depending on model type
    if (inherits(model, "merMod")) {
      # LMER model (raw merMod object)
      model_summary <- summary(model)
      coef_table <- model_summary$coefficients
      estimates <- coef_table[, 1] * scale_factor
      std_error <- coef_table[, 2] * scale_factor
      lower <- estimates - 1.96 * std_error
      upper <- estimates + 1.96 * std_error

      # Calculate p-values
      if (ncol(coef_table) >= 5) {
        p_values <- coef_table[, ncol(coef_table)]
      } else {
        t_values <- coef_table[, 3]
        p_values <- 2 * (1 - pnorm(abs(t_values)))
      }

    } else if ("tTable" %in% names(model)) {
      # LQMM model (returns summary object)
      coef_table <- model$tTable
      estimates <- coef_table[, 1] * scale_factor
      lower <- coef_table[, 3] * scale_factor
      upper <- coef_table[, 4] * scale_factor
      p_values <- coef_table[, 5]

    } else {
      return(NULL)
    }
    
    # Get coefficient names
    coef_names <- rownames(coef_table)
    
    # Filter for terms of interest - MATCH ONLY THE RELEVANT TERMS
    if (model_type %in% c("ice", "ice_income")) {
      # For ICE models, only keep the term that matches this specific model
      relevant_terms <- terms_of_interest[term_labels %in% c("Black", "Hispanic/Latino", "Asian")]
      term_indices <- which(coef_names %in% relevant_terms)
      selected_labels <- term_labels  # We'll filter this after
    } else {
      term_indices <- which(coef_names %in% terms_of_interest)
      selected_labels <- term_labels
    }
    
    if (length(term_indices) == 0) {
      return(NULL)
    }
    
    # Add significance stars
    significance <- case_when(
      p_values[term_indices] < 0.001 ~ "***",
      p_values[term_indices] < 0.01 ~ "**",
      p_values[term_indices] < 0.05 ~ "*",
      TRUE ~ ""
    )
    
    # Create formatted results for this model with actual line break
    results <- tibble(
      Term = selected_labels[1:length(term_indices)],  # Only use as many labels as we have indices
      !!model_name := paste0(
        round(estimates[term_indices], 2), significance, "\n",
        "[", round(lower[term_indices], 2), ", ", 
        round(upper[term_indices], 2), "]"
      )
    )
    
    return(results)
  })
  
  # Remove NULL results
  results_list <- results_list[!sapply(results_list, is.null)]
  
  # For ICE models, we need to handle differently since each model has one term
  if (model_type %in% c("ice", "ice_income")) {
    # Each model should have only one row, combine them properly
    final_table <- tibble(Term = term_labels)
    for (i in seq_along(results_list)) {
      if (!is.null(results_list[[i]])) {
        model_name <- names(results_list[[i]])[2]  # Get the model name column
        # Match by term and add the result
        for (term in term_labels) {
          if (term %in% results_list[[i]]$Term) {
            row_idx <- which(final_table$Term == term)
            final_table[row_idx, model_name] <- results_list[[i]][[model_name]][results_list[[i]]$Term == term]
          }
        }
      }
    }
    # Drop rows with no results (when models are compiled per-race)
    model_cols <- setdiff(names(final_table), "Term")
    if (length(model_cols) > 0) {
      final_table <- final_table %>%
        filter(rowSums(!is.na(pick(all_of(model_cols)))) > 0)
    }
  } else {
    # For income models, join normally
    if (length(results_list) > 0) {
      final_table <- results_list[[1]]
      if (length(results_list) > 1) {
        for (i in 2:length(results_list)) {
          final_table <- final_table %>%
            left_join(results_list[[i]], by = "Term")
        }
      }
    } else {
      return(NULL)
    }
  }
  
  # Rename columns for better display
  col_names <- names(final_table)
  col_names[col_names == "main"] <- "Main Model"
  col_names[col_names == "zcta_only"] <- "ZCTA Only"
  col_names[col_names == "community_only"] <- "Community Only"
  col_names[col_names == "imputed"] <- "Imputed"
  col_names[col_names == "no_2020"] <- "No 2020"
  col_names[col_names == "lmer"] <- "LMER"
  names(final_table) <- col_names
  
  # Rename the first column based on model type
  if (model_type == "income") {
    names(final_table)[1] <- "Variable"
  } else {
    names(final_table)[1] <- "Model"
  }
  
  message("  Compiled results for ", ncol(final_table) - 1, " models")
  
  return(final_table)
}


#' Fit ICE model using quantile mixed effects
#' @param data Analysis dataset
#' @param ice_variable Name of ICE variable to use
#' @param geography_control Include geography as a control variable
#' @param imputed Include clarification if it's the imputed dataset
#' @return Model summary object

fit_ice_model <- function(data, ice_variable, geography_control = FALSE) {
  message("Fitting ICE model for ", ice_variable, "...")
  
  # Build formula based on conditions
  if (geography_control == TRUE) {
    
    formula <- as.formula(paste0(
      "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
      ice_variable, " + HICDD*", ice_variable, " + geography"
    ))
    
    model <- lqmm(
      fixed = formula,
      random = ~ 1,
      group = factor(groups_county),
      data = data,
      tau = 0.5,
      weights = sqrt(data$Households),
      control = lqmm::lqmmControl(
        method = "df",
        UP_max_iter = 6000,
        LP_max_iter = 3000,
        UP_tol = 1e-4,
        LP_tol_ll = 1e-4,
        LP_tol_theta = 1e-4
      )
    )
    
  } else {
    
    formula <- as.formula(paste0(
      "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
      ice_variable, " + HICDD*", ice_variable
    ))
    
    model <- lqmm(
      fixed = formula,
      random = ~ 1,
      group = factor(groups_county),
      data = data,
      tau = 0.5,
      weights = sqrt(data$Households),
      control = lqmm::lqmmControl(
        method = "df",
        UP_max_iter = 6000,
        LP_max_iter = 3000,
        UP_tol = 1e-4,
        LP_tol_ll = 1e-4,
        LP_tol_theta = 1e-4
      )
    )

  }
  
  return(summary(model))
}
# fit_ice_model <- function(data, ice_variable, geography_control = FALSE, imputed = FALSE) {
#   message("Fitting ICE model for ", ice_variable, "...")
#   
#   # Build formula based on conditions
#   if (geography_control == TRUE && imputed == FALSE) {
#     
#     formula <- as.formula(paste0(
#       "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
#       ice_variable, " + HICDD*", ice_variable, " + geography"
#     ))
#     
#     model <- lqmm(
#       fixed = formula,
#       random = ~ 1,
#       group = factor(groups_county),
#       data = data,
#       tau = 0.5,
#       weights = sqrt(data$Households),
#       control = lqmm::lqmmControl(
#         method = "df",
#         UP_max_iter = 6000,
#         LP_max_iter = 3000,
#         UP_tol = 1e-4,
#         LP_tol_ll = 1e-4,
#         LP_tol_theta = 1e-4
#       )
#     )
#     
#   } else if (geography_control == FALSE && imputed == FALSE) {
#     
#     formula <- as.formula(paste0(
#       "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
#       ice_variable, " + HICDD*", ice_variable
#     ))
#     
#     model <- lqmm(
#       fixed = formula,
#       random = ~ 1,
#       group = factor(groups_county),
#       data = data,
#       tau = 0.5,
#       weights = sqrt(data$Households),
#       control = lqmm::lqmmControl(
#         method = "df",
#         UP_max_iter = 6000,
#         LP_max_iter = 3000,
#         UP_tol = 1e-4,
#         LP_tol_ll = 1e-4,
#         LP_tol_theta = 1e-4
#       )
#     )
#     
#   } else {  # imputed == TRUE
#     
#     formula <- as.formula(paste0(
#       "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
#       ice_variable, " + HICDD*", ice_variable
#     ))
#     
#     model <- lqmm(
#       fixed = formula,
#       random = ~ 1,
#       group = factor(groups_county),
#       data = data,
#       tau = 0.5,
#       control = lqmm::lqmmControl(
#         method = "df",
#         UP_max_iter = 6000,
#         LP_max_iter = 6000,
#         UP_tol = 1e-4,
#         LP_tol_ll = 1e-4,
#         LP_tol_theta = 1e-4
#       )
#     )
#   }
#   
#   return(summary(model))
# }

#' Fit ICE model using linear mixed effects
#' @param data Analysis dataset
#' @param ice_variable Name of ICE variable
#' @return Model summary object
fit_ice_model_lmer <- function(data, ice_variable) {
  message("Fitting ICE LMER model for ", ice_variable, "...")

  # Build formula
  formula <- as.formula(paste0(
    "Normalized_Energy ~ HICDD + Avg_Household_Size + ",
    ice_variable, " + HICDD*", ice_variable,
    " + as.factor(geography) + (1 | groups_county)"
  ))

  # Fit model
  tryCatch({
    model <- lmer(
      formula,
      data = data,
      weights = sqrt(Households)
    )

    message("  Model converged successfully")
    return(model)

  }, error = function(e) {
    message("  Model fitting failed: ", e$message)
    return(NULL)
  })
}

#' Compute marginal and conditional R^2 for a list of LMER models
#' @param model_list Named list of merMod objects (or NULL entries)
#' @return Tibble with one row per model and columns for Marginal/Conditional R^2
#' @details Uses MuMIn::r.squaredGLMM (Nakagawa & Schielzeth 2013). For weighted
#'   lmer models the values are approximate and should be interpreted as a
#'   rough goodness-of-fit indicator rather than a strict variance decomposition.
compute_lmer_r2_table <- function(model_list) {
  message("Computing R^2 for LMER models...")

  results <- map_df(names(model_list), function(name) {
    model <- model_list[[name]]

    if (is.null(model) || !inherits(model, "merMod")) {
      message("  Skipping ", name, " (not a merMod object)")
      return(tibble(
        Model = name,
        `Marginal R2` = NA_real_,
        `Conditional R2` = NA_real_
      ))
    }

    r2 <- tryCatch({
      suppressWarnings(MuMIn::r.squaredGLMM(model))
    }, error = function(e) {
      message("  R^2 failed for ", name, ": ", conditionMessage(e))
      matrix(NA_real_, nrow = 1, ncol = 2,
             dimnames = list(NULL, c("R2m", "R2c")))
    })

    # r.squaredGLMM returns a matrix; for Gaussian LMER it is typically 1x2
    tibble(
      Model = name,
      `Marginal R2` = round(as.numeric(r2[1, "R2m"]), 4),
      `Conditional R2` = round(as.numeric(r2[1, "R2c"]), 4)
    )
  })

  message("  Computed R^2 for ", nrow(results), " models")
  return(results)
}
