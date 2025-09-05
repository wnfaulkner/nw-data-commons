# 5-VISUALIZATION --------------------------------------------------------------------------------

##### % Change in Crop Production

# Open data. ### This is the first version I created. The Robinson projection is below.
df <- agriculture.clm.clean.tb
df_filtered <- df %>%     #This selected the specific scenario we want to use and number of year elapsed
  filter(soot.injection.scenario == 37, years.elapsed == 3)
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_joined <- world_sf %>%
  left_join(df_filtered, by = c("iso_a3" = "country.iso3"))

# Define crops (can choose which every crops to from agric.clm.clean.tb(colnames(data)))
colnames(agriculture.clm.clean.tb) #Find how crops are named in output
crop_vars <- c(
  "pct.change.harvest.yield.corn",
  "pct.change.harvest.yield.rice",
  "pct.change.harvest.yield.soy",
  "pct.change.harvest.yield.spring.wheat"
  )
crop_labels <- c("Maize", "Rice", "Soybean", "Wheat") #Choose crop labels in plot)
 
# Plot list with legends, clearly defining how you want the legend to look like
plots <- lapply(1:4, function(i) { ##Define the number of plots
  crop_col <- crop_vars[i]
  crop_label <- crop_labels[i]
  ggplot(world_joined) +
    geom_sf(aes_string(fill = crop_col), color = NA) +
    scale_fill_gradientn(
      colors = c("#5a1f00", "#a0522d", "white", "#4682b4", "#00008b"), #Hex color code, you can access these codes online 
      #values = scales::rescale(c(-100, -50, 0, 50, 100)), #You can use this or the breaks below 
      breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100),
      limits = c(-100, 100),
      name = "% Change in Crop Yields (t/ha)"
    ) +
    labs(title = crop_label) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
})

# Combine with patchwork (This is ised to create a shared legend)
patch_plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"))

# Add title (Removed plot)
#final_plot <- wrap_elements(full = textGrob("37 Tg case year 3", gp = gpar(fontsize = 13))) /
  #patch_plot +
  #plot_layout(heights = c(0.1, 1))

# Plot Show
print(patch_plot)


####### For Robinson Projection   ##########
df <- agriculture.clm.clean.tb
df_filtered <- df %>%
  filter(soot.injection.scenario == 37, years.elapsed == 3)

# Define Robinson projection
robin_crs <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Load and transform world shapefile
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = robin_crs)

# Used this to create curved graticule lines in Robinson projection
graticule_lines <- graticule(
  lons = seq(-180, 180, by = 10),
  lats = seq(-90, 90, by = 10),
  proj = robin_crs
)

# Generate globe outline (convex hull of graticule)
globe_outline <- st_union(st_as_sf(graticule_lines)) %>%
  st_convex_hull() %>%
  st_sf()

# Join filtered crop yield data to spatial world features
world_joined <- world_sf %>%
  left_join(df_filtered, by = c("iso_a3" = "country.iso3"))

# Define crop variables and their display labels
crop_vars <- c(
  "pct.change.harvest.yield.corn",
  "pct.change.harvest.yield.rice",
  "pct.change.harvest.yield.soy",
  "pct.change.harvest.yield.spring.wheat"
)
crop_labels <- c("Maize", "Rice", "Soybean", "Wheat")

# Create list of crop-specific ggplots
plots <- lapply(1:4, function(i) {
  crop_col <- crop_vars[i]
  crop_label <- crop_labels[i]
  
  ggplot(world_joined) +
    geom_sf(data = globe_outline, color = "grey", size = 0.1, fill = NA) +
    geom_sf(aes_string(fill = crop_col), color = NA) +
    coord_sf(crs = robin_crs, clip = "on") +
    scale_fill_gradientn(
      colors = c("#5a1f00", "#a0522d", "white", "#4682b4", "#00008b"),
      values = scales::rescale(c(-100, -50, 0, 50, 100)),
      breaks = seq(-100, 100, by = 20),
      limits = c(-100, 100),
      name = NULL,  ### NB: Suppress default title
      guide = guide_colorbar(
        title = "% Change in Crop Yields (t/ha)",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(12, "cm"),
        barheight = unit(0.7, "cm")
      )
    ) +
    labs(title = crop_label) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
})

# Combine the four crop plots into a 2x2 grid with shared legend at the bottom
patch_plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 8),
    legend.key.width = unit(2.5, "cm"),
    legend.margin = margin(t = 5, b = 5)
  )

# Display the composite plot
print(patch_plot)



#### Sea Ice Thickness 

df <- read_csv("/Users/new/Desktop/nwp/2-outputs/2025-06-20 02.15.42.623653/sea.ice_2025-06-20 02.15.42.623653.csv")

spec(df) #retrieves the full column specifications of the data 

unique(df$port) #names of port in csv file
unique(df$soot.injection.scenario) #names of soot injection scenarios in csv file

##### NW-47Tg
df_47 <- df %>%  #for soot scenario NW-47
  filter(soot.injection.scenario == 47) %>%
  filter(port %in% c("Shanghai", "Rotterdam", "St Petersburg", "Busan", "Hamburg"))
custom_colors <- c(
  "Shanghai" = "#1f77b4",
  "Rotterdam" = "#ff7f0e",
  "St Petersburg" = "#2ca02c",
  "Busan" = "#d62728",
  "Hamburg" = "#9467bd"
)

# Without Error Margins
ggplot(df_47, aes(x = years.elapsed, y = sea.ice.thickness.meters, color = port)) +
  geom_smooth(se = FALSE, span = 0.3, size = 0.8) +  # Remove confidence intervals
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") + #threshold line or reference line
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = seq(0, 15, by = 1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Sea Ice Thickness in NW-47Tg",
    x = "Years",
    y = "Sea Ice Thickness (m)",
    color = NULL
    ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key = element_rect(fill = NA),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# With Error Margins 
ggplot(df_47, aes(x = years.elapsed, y = sea.ice.thickness.meters, color = port, fill = port)) +
  geom_smooth(se = TRUE, span = 0.3, size = 0.8, alpha = 0.2) +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "black") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(breaks = seq(0, 15, by = 1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Sea Ice Thickness in NW-47Tg",
    x = "Years",
    y = "Sea Ice Thickness (m)",
    color = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key = element_rect(fill = NA),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(fill = "none") #Hides legend for error margin

###### NW-150Tg
df_150 <- df %>%  #for soot scenario NW-150
  filter(soot.injection.scenario == 150) %>%
  filter(port %in% c("Shanghai", "Rotterdam", "St Petersburg", "Busan", "Hamburg"))
custom_colors <- c(
  "Shanghai" = "#1f77b4",
  "Rotterdam" = "#ff7f0e",
  "St Petersburg" = "#2ca02c",
  "Busan" = "#d62728",
  "Hamburg" = "#9467bd"
)

# Without Error Margins
ggplot(df_150, aes(x = years.elapsed, y = sea.ice.thickness.meters, color = port)) +
  geom_smooth(se = FALSE, span = 0.3, size = 0.8) +  # Remove confidence intervals
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "black") + #threshold line or reference line
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Sea Ice Thickness in NW-150Tg",
    x = "Years",
    y = "Sea Ice Thickness (m)",
    color = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key = element_rect(fill = NA),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# With Error Margins 
ggplot(df_150, aes(x = years.elapsed, y = sea.ice.thickness.meters, color = port, fill = port)) +
  geom_smooth(se = TRUE, span = 0.3, size = 0.8, alpha = 0.2) +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "black") +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Sea Ice Thickness in NW-150Tg",
    x = "Years",
    y = "Sea Ice Thickness (m)",
    color = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key = element_rect(fill = NA),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(fill = "none") #Hides legend for error margin


#### UV-INdex_NW-150Tg

# Load UV data
df_uv <- clean.tables.ls$uv

# Filter for Year 6 only and summarize
df_year6 <- df_uv %>%
  filter(years.elapsed == 6) %>%
  group_by(country.iso3, soot.injection.scenario) %>%
  summarise(mean_uvmax = mean(uvindexmax, na.rm = TRUE), .groups = "drop")

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join UV data to world map
world_uv <- world %>%
  left_join(df_year6, by = c("iso_a3" = "country.iso3"))

# Define Robinson projection string (EPSG not always recognized directly)
robin_crs <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Reproject joined world map to Robinson
world_uv_robin <- st_transform(world_uv, crs = robin_crs)

# Create globe outline for Robinson view
globe_outline <- st_as_sfc(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = 4326)) %>%
  st_transform(crs = robin_crs)

# Define color scale limits shared across both maps
uv_limits <- range(world_uv_robin$mean_uvmax, na.rm = TRUE)

# NW-150Tg map
p1 <- ggplot() +
  geom_sf(data = world_uv_robin %>% filter(soot.injection.scenario == 150),
          aes(fill = mean_uvmax), color = "black", size = 0.1) +
  geom_sf(data = globe_outline, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis(
    option = "plasma",
    name = "UV Index",
    limits = uv_limits,
    na.value = "white"
  ) +
  labs(title = "NW-150Tg") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Combine both plots with shared legend
NW150_plot <- p1 +
  plot_annotation(
    title = "Country Averaged Annual Mean\nMonthly Maximum Ultraviolet Radiation in Year 6",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ) &
  theme(legend.position = "bottom")
#Display
NW150_plot

##### UV-INdex_NW-Control
# NW-Control map
p2 <- ggplot() +
  geom_sf(data = world_uv_robin %>% filter(soot.injection.scenario == 0),
          aes(fill = mean_uvmax), color = "black", size = 0.1) +
  geom_sf(data = globe_outline, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis(
    option = "plasma",
    name = "UV Index",
    limits = uv_limits,
    na.value = "white"
  ) +
  labs(title = "Control") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
# PLot
Control_plot <- p2 +
  plot_annotation(
    title = "Country Averaged Annual Mean\nMonthly Maximum Ultraviolet Radiation in Year 6",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ) &
  theme(legend.position = "bottom")
#Display
Control_plot

