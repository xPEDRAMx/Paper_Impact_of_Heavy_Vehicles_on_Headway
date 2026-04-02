# Load required libraries
library(png)
library(grid)
library(gridExtra)
library(stringr)

# Set the folder where the individual plots are stored
output_folder <- "/Users/pedrambeigi/Desktop/Papers/0 Done/2024 TRR - Headway/2023 TRB Headway/final Plot"

# List PNG files following the naming pattern "CDF_Lane{lane}_{type}.png"
file_pattern <- "CDF_Lane.*\\.png"
files <- list.files(path = output_folder, pattern = file_pattern, full.names = TRUE)

# Create a data.frame and extract lane and headway type from the file names.
# The file name pattern is assumed to be: CDF_Lane{lane}_{type}.png
file_info <- data.frame(file = files, stringsAsFactors = FALSE)
file_info$lane <- as.numeric(str_replace(file_info$file, ".*CDF_Lane(\\d+)_.*", "\\1"))
file_info$type <- str_replace(file_info$file, ".*CDF_Lane\\d+_([^\\.]+)\\.png", "\\1")

# Set factor levels to ensure proper ordering of headway types
file_info$type <- factor(file_info$type, levels = c("C-C", "C-HV", "HV-C"))

# Order the data frame by lane (descending order) and then by headway type as given
file_info <- file_info[order(-file_info$lane, file_info$type), ]

# Create a list of grobs from the PNG files
plots <- lapply(file_info$file, function(f) {
  img <- readPNG(f)
  rasterGrob(img, interpolate = TRUE)
})

# Open a PNG device with sufficiently high resolution for the merged plot
merged_plot_file <- file.path(output_folder, "MergedPlotsv2.png")
png(filename = merged_plot_file, width = 4200*1.5, height = 3600*2, res = 300)

# Arrange the grobs in 3 columns (which creates 4 rows given 12 images) and print to device
grid.arrange(grobs = plots, ncol = 3)

# Close the device
dev.off()

cat("Merged plot saved to:", merged_plot_file, "\n")
