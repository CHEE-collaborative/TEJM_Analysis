# Output Generation Functions
# Functions for creating tables and figures

#' Create demographic summary table
#' @param dataset_list List of datasets (zcta, community, combined)
#' @return Formatted demographic table
create_demographic_table <- function(dataset_list) {
  message("Creating demographic summary table...")
  
  # Process each dataset
  summaries <- map_df(names(dataset_list), function(dataset_name) {
    data <- dataset_list[[dataset_name]]
    
    # Get most recent year for demographics
    data_recent <- data %>%
      filter(year == max(year))
    
    # Calculate summary statistics - all as character
    summary_stats <- tibble(
      Dataset = dataset_name,
      Observations = as.character(nrow(data)),
      `Number of distinct units` = as.character(n_distinct(data$GEOID)),
      `MWh of electricity use (median (IQR))` = paste0(
        round(median(data$True_Energy, na.rm = TRUE), 2),
        " (",
        round(quantile(data$True_Energy, 0.25, na.rm = TRUE), 2),
        "-",
        round(quantile(data$True_Energy, 0.75, na.rm = TRUE), 2),
        ")"
      ),
      `Heat index cooling degree days (mean (sd))` = paste0(
        round(mean(data$HICDD, na.rm = TRUE), 2),
        " (",
        round(sd(data$HICDD, na.rm = TRUE), 2),
        ")"
      ),
      `Number of households (mean (sd))` = paste0(
        format(round(mean(data_recent$Households, na.rm = TRUE), 2), big.mark = ","),
        " (",
        format(round(sd(data_recent$Households, na.rm = TRUE), 2), big.mark = ","),
        ")"
      ),
      `Total Population (mean (sd))` = paste0(
        format(round(mean(data_recent$Total_pop, na.rm = TRUE), 2), big.mark = ","),
        " (",
        format(round(sd(data_recent$Total_pop, na.rm = TRUE), 2), big.mark = ","),
        ")"
      ),
      `Median Income (mean (sd))` = paste0(
        format(round(mean(data_recent$Median_Income_Imputed, na.rm = TRUE), 2), big.mark = ","),
        " (",
        format(round(sd(data_recent$Median_Income_Imputed, na.rm = TRUE), 2), big.mark = ","),
        ")"
      ),
      `Asian (%)` = paste0(
        round(mean(data_recent$Asian_prop * 100, na.rm = TRUE), 2),
        " (",
        round(sd(data_recent$Asian_prop * 100, na.rm = TRUE), 2),
        ")"
      ),
      `Black (%)` = paste0(
        round(mean(data_recent$Black_prop * 100, na.rm = TRUE), 2),
        " (",
        round(sd(data_recent$Black_prop * 100, na.rm = TRUE), 2),
        ")"
      ),
      `Hispanic (%)` = paste0(
        round(mean(data_recent$Hispanic_prop * 100, na.rm = TRUE), 2),
        " (",
        round(sd(data_recent$Hispanic_prop * 100, na.rm = TRUE), 2),
        ")"
      ),
      `White (%)` = paste0(
        round(mean(data_recent$White_prop * 100, na.rm = TRUE), 2),
        " (",
        round(sd(data_recent$White_prop * 100, na.rm = TRUE), 2),
        ")"
      )
    )
    
    return(summary_stats)
  })
  
  # Transpose to match desired format
  final_table <- summaries %>%
    pivot_longer(cols = -Dataset, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = Dataset, values_from = Value)
  
  # Rename columns to match your format
  names(final_table) <- c("Metric", "ZCTA", "Community", "ZCTA & Community\n(non-overlapping)")
  
  # Reorder rows to match your desired order
  row_order <- c(
    "Observations",
    "Number of distinct units",
    "MWh of electricity use (median (IQR))",
    "Heat index cooling degree days (mean (sd))",
    "Number of households (mean (sd))",
    "Total Population (mean (sd))",
    "Median Income (mean (sd))",
    "Asian (%)",
    "Black (%)",
    "Hispanic (%)",
    "White (%)"
  )
  
  final_table <- final_table %>%
    mutate(Metric = factor(Metric, levels = row_order)) %>%
    arrange(Metric) %>%
    mutate(Metric = as.character(Metric))
  
  # Add section header for racial/ethnic composition
  final_table$Metric[final_table$Metric == "Asian (%)"] <- "Racial/Ethnic Composition (mean % (sd))\n    Asian"
  final_table$Metric[final_table$Metric == "Black (%)"] <- "    Black"
  final_table$Metric[final_table$Metric == "Hispanic (%)"] <- "    Hispanic"
  final_table$Metric[final_table$Metric == "White (%)"] <- "    White"
  
  message("  Created demographic table with ", nrow(final_table), " rows")
  
  return(final_table)
}
#' Plot energy consumption by income group
#' @param data Analysis dataset
#' @param config Analysis configuration
#' @return ggplot object
plot_energy_by_income <- function(data, config) {
  message("Creating energy consumption figure...")
  
  income_colors <- c(
    "< $50,000" = "#E41A1C",      # Red
    "$50,000-$69,999" = "#377EB8",  # Blue
    "$70,000-$89,999" = "#4DAF4A",  # Green
    "≥ $90,000" = "#984EA3"         # Purple
  )
  
  # Aggregate by income group, year, and month
  plot_data <- data %>%
    group_by(Median_Income_cat, year, month) %>%
    summarize(
      mean_energy = mean(Normalized_Energy * 1000, na.rm = TRUE),
      se_energy = sd(Normalized_Energy * 1000, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(
      month_label = factor(month, 
                           levels = 5:9,
                           labels = c("May", "Jun", "Jul", "Aug", "Sep"))
    )
  
  # Update income category labels for plotting
  if (is.factor(plot_data$Median_Income_cat)) {
    levels(plot_data$Median_Income_cat) <- c("< $50,000", "$50,000-$69,999", 
                                             "$70,000-$89,999", "≥ $90,000")
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = month_label, y = mean_energy, 
                             color = Median_Income_cat,
                             group = Median_Income_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = mean_energy - se_energy,
                    ymax = mean_energy + se_energy,
                    fill = Median_Income_cat),
                alpha = 0.2, color = NA) +
    facet_wrap(~ year, ncol = 5) +
    scale_color_manual(values = income_colors, name = "Income Group") +
    scale_fill_manual(values = income_colors, name = "Income Group") +
    labs(
      x = "Month",
      y = "Average Energy Use (kWh/household)",
      caption = "Error bands represent ± 1 standard error"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  message("  Created energy consumption plot")
  
  return(p)
}

#' Plot HICDD distribution and energy response
#' @param data Analysis dataset
#' @param climate_zones Climate zone definitions
#' @param config Analysis configuration
#' @return ggplot object
# plot_hicdd_distribution <- function(data, climate_zones, config) {
#   message("Creating HICDD distribution figure...")
#   
#   # Use only the subset of colors for the two groups shown
#   subset_colors <- c(
#     "< $50,000" = "#E41A1C",      # Red
#     "≥ $90,000" = "#984EA3"        # Purple
#   )
#   
#   # Add climate zone information - join it properly
#   data_with_climate <- data %>%
#     dplyr::select(-Climate_Region) %>%
#     left_join(
#       climate_zones %>% 
#         st_drop_geometry() %>%
#         dplyr::select(county_fips, Climate_Region), 
#       by = "county_fips"
#     ) %>%
#     mutate(climate_label = Climate_Region)
#   
#   # Check if the join worked
#   if (all(is.na(data_with_climate$Climate_Region))) {
#     warning("No climate regions matched. Check county_fips values.")
#   }
#   
#   # Rest of the function continues as before...
#   # Select representative ZCTAs for plotting
#   representative_units <- data_with_climate %>%
#     filter(month == 7, year == 2018) %>%
#     filter(Median_Income_cat %in% c("<=50k", ">90k")) %>%
#     group_by(climate_label, Median_Income_cat) %>%
#     mutate(
#       percentile = percent_rank(Normalized_Energy),
#       dist_to_median = abs(percentile - 0.5)
#     ) %>%
#     filter(dist_to_median == min(abs(dist_to_median))) %>%
#     ungroup() %>%
#     dplyr::select(GEOID)
#   
#   # Filter data for selected units
#   plot_data <- data_with_climate %>%
#     inner_join(representative_units, by = "GEOID") %>%
#     mutate(
#       Normalized_Energy_kwh = Normalized_Energy * 1000
#     ) %>%
#   group_by(GEOID) %>%
#     mutate(
#       Final_Income_cat = if_else(
#         sum(Median_Income_cat == ">90k") >= sum(Median_Income_cat == "<=50k"),
#         "≥ $90,000",
#         "< $50,000"
#       )
#     ) %>%
#     ungroup()
#   
#   # Update labels
#   levels(plot_data$Final_Income_cat) <- c("< $50,000", "≥ $90,000")
#   
#   # Create main scatter plot
#   p_scatter <- ggplot(plot_data, 
#                       aes(x = HICDD, y = Normalized_Energy_kwh, 
#                           color = Final_Income_cat)) +
#     geom_point(alpha = 0.6) +
#     geom_quantile(quantiles = 0.5) +
#     facet_grid(~ climate_label) +
#     #scale_color_brewer(palette = "Set1", name = "Income Group") +
#     scale_color_manual(values = subset_colors, name = "Income Group") +
#     labs(
#       x = "Heat Index Cooling Degree Days (°F-days)",
#       y = "Energy Use (kWh/household)"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "bottom",
#       strip.text = element_text(face = "bold")
#     )
#   
#   # Create histogram
#   p_hist <- ggplot(data_with_climate, aes(x = HICDD)) +
#     geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
#     facet_grid(~ climate_label) +
#     labs(
#       x = "Heat Index Cooling Degree Days (°F-days)",
#       y = "Count (ZCTA-months)"
#     ) +
#     theme_minimal() +
#     theme(
#       strip.text = element_blank()
#     )
#   
#   # Combine plots
#   p_combined <- p_scatter / p_hist +
#     plot_layout(heights = c(3, 1)) +
#     plot_annotation(
#       tag_levels = 'A'
#     )
#   
#   message("  Created HICDD distribution plot")
#   
#   return(p_combined)
# }
plot_hicdd_distribution <- function(data, climate_zones, config) {
  message("Creating HICDD distribution figure...")
  
  # Use only the subset of colors for the two groups shown
  subset_colors <- c(
    "< $50,000" = "#E41A1C",      # Red
    "≥ $90,000" = "#984EA3"        # Purple
  )
  
  # Add climate zone information - join it properly
  data_with_climate <- data %>%
    dplyr::select(-Climate_Region) %>%
    left_join(
      climate_zones %>% 
        st_drop_geometry() %>%
        dplyr::select(county_fips, Climate_Region), 
      by = "county_fips"
    ) %>%
    mutate(climate_label = Climate_Region)
  
  # Check if the join worked
  if (all(is.na(data_with_climate$Climate_Region))) {
    warning("No climate regions matched. Check county_fips values.")
  }
  
  # Select representative ZCTAs for plotting
  representative_units <- data_with_climate %>%
    filter(month == 7, year == 2018) %>%
    filter(Median_Income_cat %in% c("<=50k", ">90k")) %>%
    group_by(climate_label, Median_Income_cat) %>%
    mutate(
      percentile = percent_rank(Normalized_Energy),
      dist_to_median = abs(percentile - 0.5)
    ) %>%
    filter(dist_to_median == min(abs(dist_to_median))) %>%
    ungroup() %>%
    dplyr::select(GEOID, climate_label, Median_Income_cat)
  
  # Filter data for selected units
  plot_data <- data_with_climate %>%
    inner_join(representative_units, by = c("GEOID", "climate_label", "Median_Income_cat")) %>%
    mutate(
      Normalized_Energy_kwh = Normalized_Energy * 1000,
      Final_Income_cat = if_else(
        Median_Income_cat == ">90k",
        "≥ $90,000",
        "< $50,000"
      )
    )
  
  # Manual label positions with climate zones
  labels_manual <- tibble(
    GEOID = c("10007", "10459", "3606912144", "3607912529", "13341", "13502"),
    label_x = c(300, 650, 500, 402, 200, 312),
    label_y = c(700, 300, 420, 1100, 790, 420),
    climate_label = c("Mixed Humid", "Mixed Humid", "Cool Humid", "Cool Humid", "Cold Humid", "Cold Humid")
  )
  
  # Get income categories for the manual labels
  labels_final <- plot_data %>%
    filter(GEOID %in% labels_manual$GEOID) %>%
    group_by(GEOID) %>%
    summarize(
      Final_Income_cat = first(Final_Income_cat),
      .groups = 'drop'
    ) %>%
    inner_join(labels_manual, by = "GEOID")
  
  # Create main scatter plot with labels
  p_scatter <- ggplot(plot_data, 
                      aes(x = HICDD, y = Normalized_Energy_kwh, 
                          color = Final_Income_cat)) +
    geom_point(alpha = 0.6) +
    geom_quantile(quantiles = 0.5) +
    geom_label(data = labels_final, 
               aes(x = label_x, y = label_y, 
                   label = GEOID, color = Final_Income_cat),
               size = 3, show.legend = FALSE) +
    facet_grid(~ climate_label) +
    scale_color_manual(values = subset_colors, name = "Income Group") +
    labs(
      x = "Heat Index Cooling Degree Days (°F-days)",
      y = "Energy Use (kWh/household)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  # Create histogram
  p_hist <- ggplot(data_with_climate, aes(x = HICDD)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    facet_grid(~ climate_label) +
    labs(
      x = "Heat Index Cooling Degree Days (°F-days)",
      y = "Count (ZCTA-months)"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_blank()
    )
  
  # Combine plots
  p_combined <- p_scatter / p_hist +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(
      tag_levels = 'A'
    )
  
  message("  Created HICDD distribution plot")
  
  return(p_combined)
}

#' Plot geographic coverage map
#' @param data Combined dataset
#' @param zcta_boundaries ZCTA boundaries
#' @param community_boundaries Community boundaries
#' @return ggplot object
plot_geographic_coverage <- function(data, zcta_boundaries, community_boundaries) {
  message("Creating geographic coverage map...")
  
  # Get NY state boundary
  ny_state <- states(cb = TRUE) %>%
    filter(STATEFP == "36")
  
  # Filter boundaries to those in analysis
  zctas_in_analysis <- zcta_boundaries %>%
    filter(GEOID %in% unique(filter(data, geography == "ZCTA")$GEOID))
  
  communities_in_analysis <- community_boundaries %>%
    filter(GEOID %in% unique(filter(data, geography == "community")$GEOID))
  
  # Create maps
  p_zcta <- ggplot() +
    geom_sf(data = ny_state, fill = "white", color = "black") +
    geom_sf(data = zctas_in_analysis, fill = "red", alpha = 0.5, color = "gray50", size = 0.1) +
    labs(title = "A. ZCTAs with Energy Data") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  p_community <- ggplot() +
    geom_sf(data = ny_state, fill = "white", color = "black") +
    geom_sf(data = communities_in_analysis, fill = "blue", alpha = 0.5, color = "gray50", size = 0.1) +
    labs(title = "B. Communities with Energy Data") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Combined non-overlapping
  p_combined <- ggplot() +
    geom_sf(data = ny_state, fill = "white", color = "black") +
    geom_sf(data = zctas_in_analysis, aes(fill = "ZCTA"), alpha = 0.5, color = "gray50", size = 0.1) +
    geom_sf(data = communities_in_analysis, aes(fill = "Community"), alpha = 0.5, color = "gray50", size = 0.1) +
    scale_fill_manual(values = c("ZCTA" = "red", "Community" = "blue"),
                      name = "Geography Type") +
    labs(title = "C. Combined Non-overlapping Coverage") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  # Combine all maps
  p_all <- p_zcta / p_community / p_combined +
    plot_annotation(
      title = "Geographic Coverage of Energy Analysis",
      subtitle = "Non-overlapping ZCTAs and communities for unbiased estimation"
    )
  
  message("  Created geographic coverage map")
  
  return(p_all)
}
