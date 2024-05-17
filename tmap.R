# Load Packages
library(tmap)

# Joining the boundary files and converting to sf
Supply_and_Demand_Map <- left_join(Supply_and_Demand_index_table,
                                   Boundaries_LAD[,-c(2:3)],
                                   by = c("LAD23CD"))
Supply_and_Demand_tmap <- st_as_sf(Supply_and_Demand_Map)

Health_Index_Map <- left_join(Health_Index_Normalised,
                              Boundaries_LAD[,-c(2:3)],
                              by = c("LAD23CD"))
Health_Index_tmap <- st_as_sf(Health_Index_Map)

Supply_and_demand_difference_Map <- left_join(Supply_and_demand_difference,
                                              Boundaries_LAD[,-c(2:3)],
                                              by = c("LAD23CD"))
Supply_and_demand_difference_tmap <- st_as_sf(Supply_and_demand_difference_Map)

# Create the mapping function for individual maps
tmap_results <- function(col, 
                         title, 
                         lwd, 
                         main.title, 
                         main.title.size,
                         legend.format = list(digits = x), 
                         x = 2,
                         legend.text.size, 
                         legend.title.size, 
                         text.size,
                         size,
                         credits)
{
  tm_shape(Supply_and_Demand_tmap) +
    tm_polygons(col = col,
                title = title,
                palette = "Blues",
                style = "quantile",
                lwd = lwd) +
    tm_layout(main.title = main.title,
              main.title.position = "centre",
              main.title.size = main.title.size,
              frame = F, 
              legend.outside = F,
              legend.format = legend.format, 
              legend.position = c("left", "top"),
              legend.text.size = legend.text.size,
              legend.title.size = legend.title.size) +
    tm_scale_bar(position = c("right", "bottom"),
                 text.size = text.size,
                 breaks  = c(0, 50, 100, 150)) + 
    tm_compass(position = c("right", "top"),
               size = size) +
    tm_credits(credits, position = c("center", "BOTTOM"))
}

# Create a function to save the Map Outputs
Map_Outputs <- function(x, filename) {
  tmap_save(tm = x,
            filename = filename,
            dpi = 600)
}

## Create one figure for 6 maps
# Apply function to each of the standardised variables
Dentists_10000_arranged <- tmap_results(col = "Dentists per 10,000 Population",
                                        title = "Dentists per 10,000",
                                        lwd = 0.1,
                                        main.title = "Number of Dentists per Local Authority in England",
                                        main.title.size = 0.65,
                                        legend.text.size = 0.5,
                                        legend.title.size = 0.65,
                                        text.size = 0.4,
                                        size = 1.5,
                                        credits = " ")

Contracted_UDA_10000_arranged <- tmap_results(col = "Contracted UDAs per 10,000 Population",
                                              title = "Contracted UDAs per 10,000",
                                              lwd = 0.1,
                                              main.title = "Contracted UDAs per Local Authority in England",
                                              main.title.size = 0.65,
                                              x = 0,
                                              legend.text.size = 0.5,
                                              legend.title.size = 0.65,
                                              text.size = 0.4,
                                              size = 1.5,
                                              credits = " ")

Delivered_UDA_perc_arranged <- tmap_results(col = "Percentage of Delivered UDAs",
                                            title = "Percentage Delivered (%)",
                                            lwd = 0.1,
                                            main.title = "Delivered UDAs per Local Authority in England",
                                            main.title.size = 0.65,
                                            legend.text.size = 0.5,
                                            legend.title.size = 0.65,
                                            text.size = 0.4,
                                            size = 1.5,
                                            credits = " ")

Success_Rate_arranged <- tmap_results(col = "Appointment Success Rate",
                                      title = "Success Rate (%)",
                                      lwd = 0.1,
                                      main.title = "Appointment Success Rates per Local Authority in England",
                                      main.title.size = 0.65,
                                      legend.text.size = 0.5,
                                      legend.title.size = 0.65,
                                      text.size = 0.4,
                                      size = 1.5,
                                      credits = " ")

Average_Income_Score_arranged <- tmap_results(col = "Average Income Deprivation Score",
                                              title = "Average Score",
                                              lwd = 0.1,
                                              main.title = "Income Deprivation Scores per Local Authority in England",
                                              main.title.size = 0.65,
                                              legend.text.size = 0.5,
                                              legend.title.size = 0.65,
                                              text.size = 0.4,
                                              size = 1.5,
                                              credits = " ")

No_Car_Ownership_perc_arranged <- tmap_results(col = "Percentage of Households with No Car",
                                               title = "Households (%)",
                                               lwd = 0.1,
                                               main.title = "Households with No Car per Local Authority in England",
                                               main.title.size = 0.65,
                                               legend.text.size = 0.5,
                                               legend.title.size = 0.65,
                                               text.size = 0.4,
                                               size = 1.5,
                                               credits = " ")

# Save map outputs in 2x3
tmap_save(tm = tmap_arrange(Dentists_10000_arranged,
                            Contracted_UDA_10000_arranged,
                            Delivered_UDA_perc_arranged,
                            Success_Rate_arranged,
                            Average_Income_Score_arranged,
                            No_Car_Ownership_perc_arranged,
                            nrow = 3,
                            ncol = 2),
          filename = "Outputs/Updated/tmap_arranged_std.png",
          width = 3781,
          height = 4665,
          dpi = 600)

## Create individual maps
# Apply function to each of the standardised variables
Dentists_10000_Map <- tmap_results(col = "Dentists per 10,000 Population",
                                   title = "Dentists per 10,000",
                                   lwd = 1,
                                   main.title = "Number of Dentists per Local Authority in England",
                                   main.title.size = 1.3,
                                   legend.text.size = 1,
                                   legend.title.size = 1.2,
                                   text.size = 1,
                                   size = 4,
                                   credits = "(NHS BSA, 2024; ONS, 2023e; 2024)")

Contracted_UDA_10000_Map <- tmap_results(col = "Contracted UDAs per 10,000 Population",
                                         title = "Contracted UDAs per 10,000",
                                         lwd = 1,
                                         main.title = "Contracted UDAs per Local Authority in England",
                                         main.title.size = 1.3,
                                         x = 0,
                                         legend.text.size = 1,
                                         legend.title.size = 1.2,
                                         text.size = 1,
                                         size = 4,
                                         credits = "(NHS BSA, 2024; ONS, 2023e; 2024)")

Delivered_UDA_perc_Map <- tmap_results(col = "Percentage of Delivered UDAs",
                                       title = "Percentage Delivered (%)",
                                       lwd = 1,
                                       main.title = "Delivered UDAs per Local Authority in England",
                                       main.title.size = 1.3,
                                       legend.text.size = 1,
                                       legend.title.size = 1.2,
                                       text.size = 1,
                                       size = 4,
                                       credits = "(NHS BSA, 2024; ONS, 2024)")

Success_Rate_Map <- tmap_results(col = "Appointment Success Rate",
                                 title = "Success Rate (%)",
                                 lwd = 1,
                                 main.title = "Appointment Success Rates per Local Authority in England",
                                 main.title.size = 1.3,
                                 legend.text.size = 1,
                                 legend.title.size = 1.2,
                                 text.size = 1,
                                 size = 4,
                                 credits = "(NHS England, 2024; ONS, 2024)")

Average_Income_Score_Map <- tmap_results(col = "Average Income Deprivation Score",
                                         title = "Average Score",
                                         lwd = 1,
                                         main.title = "Income Deprivation Scores per Local Authority in England",
                                         main.title.size = 1.3,
                                         legend.text.size = 1,
                                         legend.title.size = 1.2,
                                         text.size = 1,
                                         size = 4,
                                         credits = "(MHCLG, 2019; ONS, 2024)")

No_Car_Ownership_perc_Map <- tmap_results(col = "Percentage of Households with No Car",
                                          title = "Households (%)",
                                          lwd = 1,
                                          main.title = "Households with No Car per Local Authority in England",
                                          main.title.size = 1.3,
                                          legend.text.size = 1,
                                          legend.title.size = 1.2,
                                          text.size = 1,
                                          size = 4,
                                          credits = "(ONS, 2023a; 2023d; 2024)")

# Save map outputs
Map_Outputs(Dentists_10000_Map,
            "Outputs/Updated/Dentists_10000_Map.png")
Map_Outputs(Contracted_UDA_10000_Map,
            "Outputs/Updated/Contracted_UDA_10000_Map.png")
Map_Outputs(Delivered_UDA_perc_Map,
            "Outputs/Updated/Delivered_UDA_perc_Map.png")
Map_Outputs(Average_Income_Score_Map,
            "Outputs/Updated/Average_Income_Score_Map.png")
Map_Outputs(Success_Rate_Map,
            "Outputs/Updated/Success_Rate_Map.png")
Map_Outputs(No_Car_Ownership_perc_Map,
            "Outputs/Updated/No_Car_Ownership_perc_Map.png")

# Apply function to the PDAS
Index_Map <- tmap_results(col = "Public Dental Access Score",
                          title = "Public Dental Access Score",
                          lwd = 1,
                          main.title = "Public Dental Access Scores per Local Authority in England",
                          main.title.size = 1.3,
                          x = 0,
                          legend.text.size = 1,
                          legend.title.size = 1.2,
                          text.size = 1,
                          size = 4,
                          credits = "(NHS BSA, 2024; NHS England, 2024; MHCLG, 2019; ONS, 2023a; 2023d; 2023e; 2024)")

# Save the map output
Map_Outputs(Index_Map,
            "Outputs/Updated/Index_Map.png")

# Create difference map between PDAS and HI
Difference_Map <- tm_shape(Supply_and_demand_difference_tmap) +
  tm_polygons(col = "Difference",
              title = "Difference in Scores",
              palette = "RdBu",
              n = 5,
              style = "sd",
              lwd = 1,
              midpoint = 0) +
  tm_layout(main.title = "Difference between PDAI and Health Index per Local Authority in England",
            main.title.position = "centre",
            main.title.size = 1.05,
            frame = F, 
            legend.outside = F,
            legend.format = list(digits = 0), 
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1.2) +
  tm_scale_bar(position = c("right", "bottom"),
               text.size = 1,
               breaks  = c(0, 50, 100, 150)) + 
  tm_compass(position = c("right", "top"),
             size = 4) +
  tm_credits("(NHS BSA, 2024; NHS England, 2024; MHCLG, 2019; ONS, 2023a; 2023b; 2023d; 2023e; 2024)", position = c("center", "BOTTOM"))

# Health Index map
Health_Index_Map <- tm_shape(Health_Index_tmap) +
  tm_polygons(col = "Average Health Score",
              title = "Health Index Scores",
              palette = "Reds",
              style = "quantile",
              lwd = 1) +
  tm_layout(main.title = "Health Index Scores per Local Authority in England",
            main.title.position = "centre",
            main.title.size = 1.3,
            frame = F, 
            legend.outside = F,
            legend.format = list(digits = 0), 
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1.2) +
  tm_scale_bar(position = c("right", "bottom"),
               text.size = 1,
               breaks  = c(0, 50, 100, 150)) + 
  tm_compass(position = c("right", "top"),
             size = 4) +
  tm_credits("(ONS, 2023b; 2024)", position = c("center", "BOTTOM"))

# Save the map outputs
Map_Outputs(Difference_Map, "Outputs/Updated/Difference_Map.png")
Map_Outputs(Health_Index_Map, "Outputs/Updated/Health_Index_Map.png")