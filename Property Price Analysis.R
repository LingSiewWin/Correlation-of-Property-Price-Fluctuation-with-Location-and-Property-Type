#to read the file path
file_path <-"C://Users//User//Documents//APU SEM 3//PFDA//Assignment//Cleaned_KL_Property_Data.csv"
df <- read.csv(file_path)
View(df)

library(dplyr)
library(ggplot2)
panel_color <- "#FFF8DC"
background_color <- "#EEE8CD"

#Locations
provided_locations <- c("Kepong, Kuala Lumpur","Setapak, Kuala Lumpur","Bukit Jalil, Kuala Lumpur", 
                        "Cheras, Kuala Lumpur","Mont Kiara, Kuala Lumpur","Bangsar, Kuala Lumpur", 
                        "Damansara Heights, Kuala Lumpur", "KLCC, Kuala Lumpur", "Country Heights Damansara, Kuala Lumpur")
#Property_Types
provided_property_types <- c("Flat", "Apartment", "Serviced Residence", 
                             "Condominium", "Townhouse", "Cluster House", 
                             "Semi-detached House", "Residential Land", "Bungalow")

top_locations_property_counts <- function(df, property_types) {
  subset_df <- subset(df, Property_Type %in% property_types)  # Filter dataframe by specified Property_Type
  top_locations <- head(sort(table(subset_df$Location), decreasing = TRUE), 10)  # Find top 10 locations
  return(top_locations)  # Return top locations and their counts
}

# Create dataframes for each set of top 10 locations
df_top_flat_apartment <- data.frame(Location = names(top_flat_apartment), Count = as.vector(top_flat_apartment))
df_top_condo_townhouse <- data.frame(Location = names(top_condo_townhouse), Count = as.vector(top_condo_townhouse))
df_top_bungalow_residential <- data.frame(Location = names(top_bungalow_residential), Count = as.vector(top_bungalow_residential))

# Combine dataframes into one
df_top_locations <- rbind(df_top_flat_apartment, df_top_condo_townhouse, df_top_bungalow_residential)

# Order the combined dataframe by count in ascending order
df_top_locations <- df_top_locations %>% arrange(Count)
# View combined dataframe
View(df_top_locations)
#__________________1.Barplot to find the median price of selected property type________________________________
# Define the filtered dataframe
filtered_df <- df[df$Location %in% provided_locations, ]

# Aggregate median prices for the filtered dataframe
filtered_location_median_price <- aggregate(Price_RM ~ Location, data = filtered_df, FUN = median)

# Sort the median prices in ascending order
filtered_location_median_price <- filtered_location_median_price[order(filtered_location_median_price$Price_RM), ]

# Reorder the Location factor based on Price_RM
filtered_location_median_price$Location <- factor(filtered_location_median_price$Location, levels = filtered_location_median_price$Location[order(filtered_location_median_price$Price_RM)])

# Create a bar plot with fill color gradient and x-axis reordered
ggplot(data = filtered_location_median_price, aes(x = Location, y = Price_RM, fill = Price_RM)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#CAFF70", high ="#6E8B3D", labels = function(x) paste0(round(x/1000), "k")) +
  scale_y_continuous(labels = function(x) paste0(round(x/1000), "k"),  # Divide values by 1000, round, and append "k"
                     breaks = seq(0, 6000000, by = 500000)) +  # Adding breaks at 500,000 intervals up to 6,000,000
  labs(title = "Median Prices by Location",
       x = "Location",
       y = "Median Price (RM)",
       fill = "Median Price (RM)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(color = "black", fill = NA),  # Set border color to black
        plot.background = element_rect(fill = background_color, color = "black")) 

#____________________________Analysis 2 perfect_____________________________
#_________________________Relationship between Property Type and Price in Selected Locations____________________________________________

#Label for Y-AXIS
price_breaks <- c(0, 5000000, 10000000, 20000000, 30000000, 40000000, 50000000, 60000000)

# Define colors for each property type
property_colors <- c(
  "Apartment" = "#1f77b4",            # Blue
  "Serviced Residence" = "#ff7f0e",    # Orange
  "Bungalow" = "#2ca02c",             # Green
  "Bungalow Land" = "#d62728",         # Red
  "Condominium" = "#9467bd",           # Purple
  "Cluster House" = "#8c564b",         # Brown
  "Semi-detached House" = "#e377c2",   # Pink
  "Townhouse" = "#7f7f7f",             # Gray
  "Residential Land" = "#bcbd22",      # Olive
  "Flat" = "#17becf")                  # Cyan

# Filter the dataframe to include only the provided Location and Property_Type
df_provided_locations <- df[df$Location %in% provided_locations, ]
df_provided_properties <- df_provided_locations[df_provided_locations$Property_Type %in% provided_property_types, ]

# Create the plot
ggplot(df_provided_properties, aes(x = factor(Property_Type), y = Price_RM, color = factor(Property_Type))) +
  geom_point(shape = 15, size = 3) +  # Use shape 15 (house symbol) and adjust size as needed
  scale_y_continuous(breaks = price_breaks, labels = paste0(price_breaks/1000000, "M")) +
  facet_wrap(~ Location) +  # Use facets to represent Location dimension
  theme(panel.background = element_rect(fill = panel_color),
        plot.background = element_rect(fill = background_color),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = property_colors) +  # Set custom colors
  labs(x = "Property Type", y = "Price (RM)", color = "Property Type") +
  ggtitle("Relationship between Property Type and Price in Selected Locations")

#__________________Analysis 3 Heat map to show the correlation of Property_Type and Location__________________________________
# Define breaks for price ranges

price_breaks <- seq(0, 6000000, by = 500000)

# Create a heatmap with custom color breaks for Price_RM and a black background
ggplot(filtered_df, aes(x = Location, y = Property_Type, fill = Price_RM)) +
  geom_tile() +
  scale_fill_gradient(breaks = price_breaks, limits = c(0, 6000000), 
                      low = "yellow", high = "red", 
                      labels = scales::dollar_format(prefix = "RM"), 
                      name = "Price_RM",
                      na.value = "white") +  # Set color for areas with no data to white
  labs(title = "Density of Price_RM by Property Type and Location",
       x = "Location",
       y = "Property Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),  # Set background color to white
        panel.border = element_rect(color = "black", fill = NA),  # Set border color to black
        plot.background = element_rect(fill = background_color, color = "black"))  # Set background color

#_____________________________4_boxplot _______________________________
#Locations
df_provided_locations <- df[df$Location %in% provided_locations, ]
df_provided_properties <- df_provided_locations[df_provided_locations$Property_Type %in% provided_property_types, ]

#Create the boxplot
ggplot(df_provided_properties, aes(x = Location, y = Price_RM, fill = factor(Property_Type))) +
  geom_boxplot() +
  scale_y_continuous(breaks = price_breaks, labels = paste0(price_breaks/1000000, "M")) +
  facet_wrap(~ Property_Type) +  # Use facets to represent Property_Type dimension
  scale_fill_manual(values = property_colors) +  # Set custom colors
  labs(title = "Price Distribution by Location and Property Type",
       x = "Location",
       y = "Price (RM)",
       fill = "Property Type") +
  theme(panel.background = element_rect(fill = panel_color),
        plot.background = element_rect(fill = background_color),
        axis.text.x = element_text(angle = 45, hjust = 1))
