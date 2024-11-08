# Seconds in a day
seconds_in_day <- 86400

# Days in each month (as provided, starting from January)
days_in_months <- c(31, 31, 31, 31, 30, 30, 30, 30, 31, 28, 30, 31)  # Adjust to actual month lengths if needed

# Shift the vector to start from July
days_in_months_shifted <- c(days_in_months[7:12], days_in_months[1:6])

# Repeat the shifted pattern for 100 years
num_years <- 100
days_in_100_years_shifted <- rep(days_in_months_shifted, num_years)

# Calculate cumulative seconds starting from July 1st
seconds_per_month <- cumsum(days_in_100_years_shifted * seconds_in_day)

# Print the list, separated by commas
s_list <- cat(paste(seconds_per_month, collapse = ", "))
# Split the list into two halves
split_point <- ceiling(length(seconds_per_month) / 2)
list1 <- cat(paste(seconds_per_month[1:split_point], collapse = ","))
list_part2 <- seconds_per_month[(split_point + 1):length(seconds_per_month)]
print(list_part1)

##################################################################################


# Define the value and the placeholder
high_fe <- 0.0259280357142857 * 4
base_fe <- "_"

# Initialize an empty list to hold the polygons
# remember that id order in original bgm does not correspond to following order.
# here, polygon 0 is 29 for coding purposes. Active polygons are 1 to 25.
polygons_list <- vector("list", 29)  # For 29 polygons
coast_p <- c(1:3, 5, 6) # coastal polygons with Fe enrichment from glacial and terrestrial runoff
sediment_p <- c(10:12, 16:18) # offshore polygons with Fe enrichment from sediment resuspension
offshore_p <- c(7:9, 13:15, 19:25) # offshore polygons with low Fe
icebound_p <- 4 # permanently icebound box
boundary_p <- c(29, 28, 27, 26) # boundary/static polygons

# JANUARY
# ice only on polygon 5
# ice melt in polygon 1
for (i in 1:29) {
  if (i %in% coast_p | i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  # high iron only in areas affected by sed resuspension
  } else if (i %in% c(1)) {
    polygons_list[[i]] <- c(high_fe, rep(base_fe, 10))  # fe input only at surface due to ice melt
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
jan <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  jan[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# FEBRUARY
# ice only on polygon 5
for (i in 1:29) {
  if (i %in% coast_p | i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6)) # high iron only in areas affected by sed resuspension
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
feb <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  feb[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# MARCH
# no ice anywhere
for (i in 1:29) {
  if (i %in% coast_p | i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  # high iron only in areas affected by sed resuspension
  } else if (i %in% icebound_p) {
    polygons_list[[i]] <- c(high_fe, rep(base_fe, 10))  # fe input only at surface
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
mar <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  mar[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# APRIL
# start of annual ice advance
# ice in polygons 2, 3, 4, 6
for (i in 1:29) {
  if (i %in% sediment_p) { # no enhanced coastal input as glacial and terrestrial runoff decrease
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
apr <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  apr[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# MAY
# ice in polygons 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, and 12
for (i in 1:29) { # no enhanced coastal input
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
may <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  may[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# JUNE
# ice in polygons 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18
# 5 still ice-free
for (i in 1:29) {
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
jun <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  jun[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# JULY
# ice in polygons 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20
for (i in 1:29) {
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
jul <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  jul[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# AUGUST
# ice in polygons 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 24, 25
# no ice in p 23
for (i in 1:29) {
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
aug <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  aug[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# SEPTEMBER
# ice maximum extent
# ice in polygons 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26
for (i in 1:29) {
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
sep <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  sep[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# OCTOBER
# ice melting starts
# ice in polygons 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25, 26
# ice melts from polygons 5, 20, 21, 24
for (i in 1:29) {
  if (i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else if (i %in% c(5, 20, 21, 24)) {
    polygons_list[[i]] <- c(high_fe, rep(base_fe, 10))  # fe input only at surface
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
oct <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  oct[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# NOVEMBER
# ice melts from polygons 2, 3, 6, 27, 14, 15, 16, 17, 25
# coastal input starts again
for (i in 1:29) {
  if (i %in% coast_p | i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else if (i %in% c(2, 3, 6, 27, 14, 15, 16, 17, 25)) {
    polygons_list[[i]] <- c(high_fe, rep(base_fe, 10))  # fe input only at surface
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
nov <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  nov[i] <- paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# DECEMBER
# ice melts from polygons 7, 13, 8, 9, 10, 11, 12, 18
for (i in 1:29) {
  if (i %in% coast_p | i %in% sediment_p) {
    polygons_list[[i]] <- c(rep(high_fe, 5), rep(base_fe, 6))  
  } else if (i %in% c(7, 13, 8, 9, 10, 11, 12, 18)) {
    polygons_list[[i]] <- c(high_fe, rep(base_fe, 10))  # fe input only at surface
  } else {  
    polygons_list[[i]] <- rep(base_fe, 11)  
  }
}
dec <- character(length(polygons_list))
for (i in 1:length(polygons_list)) {
  dec[i] <-paste(paste(polygons_list[[i]], collapse = ", "), ",", sep = "")
}

# Create a vector for all months
all_months <- c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

# Concatenate the output from all months into a single string, each row (i.e., polygon layers) separated by a new line
# keeping format for netcdf file...
# Repeat the output 50 times
concat_50y <- paste(rep(all_months, 50), collapse = "\n ")

# Define the output file path
output_file <- "iron_EAAM.txt"

# Save the repeated concatenated output into the text file, preserving format
writeLines(concat_50y, con = output_file)

# Print success message
cat("Output successfully saved to", output_file, "\n")

