# Load required libraries
library("readxl")
library(fitdistrplus)
library(actuar)

# Define file paths (each file represents a lane: H-2, H-3, H-4, H-5)
file_paths <- c(
  "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/Data/H-2.xlsx",
  "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/Data/H-3.xlsx",
  "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/Data/H-4.xlsx",
  "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/Data/H-5.xlsx"
)

legend_position <- "bottomright"    # Options: "topright", "bottomright", etc.
legend_inset <- c(0.0055, 0.122)

# Define headway type labels corresponding to the three sheets:
# Sheet 1: C-C, Sheet 2: C-HV, Sheet 3: HV-C
headway_types <- c("C-C", "C-HV", "HV-C")

# Define the output folder for plots and CSV file
output_folder <- "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/final Plot"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Create an empty data frame to store all results
results_df <- data.frame(
  Lane = character(),
  HeadwayType = character(),
  Distribution = character(),
  KS = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each file (each lane)
for (file in file_paths) {
  # Extract lane number from the filename (e.g., "H-2.xlsx" gives lane "2")
  lane <- gsub(".*H-(\\d+)\\.xlsx", "\\1", file)
  cat("\n====================================\n")
  cat("Processing Lane:", lane, "\n")
  
  # Loop over each headway type (each corresponding sheet)
  for (i in seq_along(headway_types)) {
    # Define sheet number and headway type label
    sheet_number <- as.character(i)
    type_name <- headway_types[i]
    cat("\n------------------------------------\n")
    cat("Results for Lane:", lane, " - Headway type:", type_name, "\n")
    
    # Read data from the current sheet of the current file
    data <- read_excel(file, sheet = sheet_number)
    
    # Clean the data by removing NA values; assumes headway values are in column "Headway"
    headway_data <- na.omit(data$Headway)
    
    # Fit the distributions using fitdist()
    # 1. Gamma distribution
    fg <- fitdist(headway_data, "gamma")
    # 2. Lognormal distribution
    fln <- fitdist(headway_data, "lnorm")
    # 3. Weibull distribution
    fw <- fitdist(headway_data, "weibull")
    # 4. Inverse Gaussian distribution (using starting values: mean of data and shape = 1)
    fig <- fitdist(headway_data, "invgauss", start = list(mean = mean(headway_data), shape = 1))
    
    # Save the CDF comparison plot for the current lane and headway type.
    # Construct the filename (e.g., "CDF_Lane2_C-C.png")
    plot_filename <- paste0("CDF_Lane", lane, "_", type_name, ".png")
    plot_filepath <- file.path(output_folder, plot_filename)
    
    # Open the PNG device with high resolution
    png(filename = plot_filepath, width = 4200, height = 3600, res = 300)
    # Adjust graphical parameters to reduce text size and expand the plot region.
    par(cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, mar = c(4.5, 4.5, 3, 1) +0.1)
    
    # Create the CDF comparison plot.
    # Note: The x-axis label adds 6 to the lane value.
    plot.legend <- c("gamma", "lognormal", "Weibull", "invgauss")
    cdfcomp(list(fg, fln, fw, fig),
            legendtext = plot.legend,
            main = "Empirical and Theoretical CDFs",
            fitcol = c("red", "yellow", "blue", "green"),
            fitlwd = 2.5,
            xlab = paste(type_name, "Headways in Lane", - as.numeric(lane) + 6, "(s)"))
    # Add a manual legend for the empirical data.
    legend(legend_position, inset = legend_inset, 
           legend = c("Empirical"),
           col = c("black"), 
           lty = 1, lwd = 2.2, bty = "n")
    dev.off()  # Close the PNG device
    
    # Compute the empirical CDF from the data
    ecdf_data <- ecdf(headway_data)
    
    # Manually compute the KS test statistic for each fitted distribution:
    # Gamma
    D_gamma <- max(abs(ecdf_data(headway_data) -
                         pgamma(headway_data,
                                shape = fg$estimate["shape"],
                                rate = fg$estimate["rate"])))
    # Lognormal
    D_lnorm <- max(abs(ecdf_data(headway_data) -
                         plnorm(headway_data,
                                meanlog = fln$estimate["meanlog"],
                                sdlog = fln$estimate["sdlog"])))
    # Weibull
    D_weibull <- max(abs(ecdf_data(headway_data) -
                           pweibull(headway_data,
                                    shape = fw$estimate["shape"],
                                    scale = fw$estimate["scale"])))
    # Inverse Gaussian (using pinvgauss from the actuar package)
    D_invgauss <- max(abs(ecdf_data(headway_data) -
                            pinvgauss(headway_data,
                                      mean = fig$estimate["mean"],
                                      shape = fig$estimate["shape"])))
    
    # Display the manually computed KS statistics for each fitted distribution
    cat("\nKolmogorov-Smirnov Test Statistics (D):\n")
    cat("  Gamma:            ", D_gamma, "\n")
    cat("  Lognormal:        ", D_lnorm, "\n")
    cat("  Weibull:          ", D_weibull, "\n")
    cat("  Inverse Gaussian: ", D_invgauss, "\n")
    
    # Print AIC and BIC values using the built-in functions
    cat("\nAkaike's Information Criterion (AIC):\n")
    cat("  Gamma:            ", AIC(fg), "\n")
    cat("  Lognormal:        ", AIC(fln), "\n")
    cat("  Weibull:          ", AIC(fw), "\n")
    cat("  Inverse Gaussian: ", AIC(fig), "\n")
    
    cat("\nBayesian Information Criterion (BIC):\n")
    cat("  Gamma:            ", BIC(fg), "\n")
    cat("  Lognormal:        ", BIC(fln), "\n")
    cat("  Weibull:          ", BIC(fw), "\n")
    cat("  Inverse Gaussian: ", BIC(fig), "\n")
    
    # Add results for each distribution as new rows in the results data frame.
    new_rows <- data.frame(
      Lane = rep(lane, 4),
      HeadwayType = rep(type_name, 4),
      Distribution = c("Gamma", "Lognormal", "Weibull", "Inverse Gaussian"),
      KS = c(D_gamma, D_lnorm, D_weibull, D_invgauss),
      AIC = c(AIC(fg), AIC(fln), AIC(fw), AIC(fig)),
      BIC = c(BIC(fg), BIC(fln), BIC(fw), BIC(fig)),
      stringsAsFactors = FALSE
    )
    
    results_df <- rbind(results_df, new_rows)
    
    cat("------------------------------------\n")
    # Optional: Pause between iterations if desired.
    # readline(prompt = "Press [Enter] to continue")
  }
  cat("====================================\n")
}

# Save the results data frame to a CSV file in the output folder
csv_filename <- "FitResults.csv"
csv_filepath <- file.path(output_folder, csv_filename)
write.csv(results_df, file = csv_filepath, row.names = FALSE)

cat("\nAll plots and results have been saved in:\n", output_folder, "\n")
