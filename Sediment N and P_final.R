# ============================
# Load packages
# ============================
library(gsheet)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(ggsci)
library(patchwork)
library(multcompView)
library(viridis)
library(ggtext)
library(truncnorm)
library(sf)
library(dplyr)
library(forcats)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotrix)
library(ggbreak)


# ============================
# Conversion Functions
# ============================
convert_to_MgN_ha <- function(value, unit, dbd, depth_cm) {
  case_when(
    unit == "Mg N ha-1"  ~ value,
    unit == "g N m-2"    ~ value * 0.01, # verified
    unit == "mol N m-2"  ~ value * 14.007 * 0.01, #verified
    unit == "mmol N m-2" ~ value * 1e-3 * 14.007 * 0.01, #verified
    unit == "kg N m-2"   ~ value * 10, #verified
    unit == "g N cm-2"   ~ value * 100, #verified
    unit == "%"           ~ (value / 100) * dbd * depth_cm * 100,
    unit %in% c("ppm", "mg N kg-1") ~ (value * 1e-6) * dbd * depth_cm * 100,
    unit == "ppt"         ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "mg N g-1"    ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "ug N g-1"    ~ (value * 1e-6) * dbd * depth_cm * 100,
    unit == "umol N g-1"  ~ (value * 14.007e-6) * dbd * depth_cm * 100,
    unit == "nmol N g-1"  ~ (value * 14.007e-9) * dbd * depth_cm * 100,
    unit == "g N g-1"     ~ value * dbd * depth_cm * 100,
    unit == "g N kg-1"    ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "kg N m-3"    ~ value * (depth_cm / 100) * 10,
    unit == "g N m-3"     ~ value * (depth_cm / 100) * 0.01,
    unit == "mg N cm-3"   ~ value * depth_cm * 0.1, #verified
    unit == "umol N cm-3" ~ value * 14.007e-6 * depth_cm * 100,
    TRUE ~ NA_real_
  )
}

convertP_to_MgHa <- function(value, unit, dbd, depth_cm) {
  case_when(
    unit == "Mg P ha-1"  ~ value, 
    unit == "g P m-2"    ~ value * 0.01,
    unit == "mg P m-2"   ~ value * 1e-5, #verified
    unit == "mmol P m-2" ~ value * 30.974 * 0.01 * 1e-3,
    unit == "%"          ~ (value / 100) * dbd * depth_cm * 100,
    unit %in% c("ppm", "mg P kg-1") ~ (value * 1e-6) * dbd * depth_cm * 100,
    unit == "ppt"        ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "g P kg-1"   ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "mg P g-1"   ~ (value * 1e-3) * dbd * depth_cm * 100,
    unit == "ug P g-1"   ~ (value * 1e-6) * dbd * depth_cm * 100,
    unit == "nmol P g-1" ~ (value * 1e-9 * 30.974) * dbd * depth_cm * 100,
    unit == "umol P g-1" ~ (value * 30.974e-6) * dbd * depth_cm * 100,
    unit == "mg P2O5 g-1"~ (value * 1e-3 * 0.436) * dbd * depth_cm * 100,
    unit == "umol P cm-3"~ (value * 30.974e-6) * depth_cm * 100,
    TRUE ~ NA_real_
  )
}

# ============================
# Polygon helper function
# ============================

make_closed_polygon <- function(df) {
  if (!all(df[1, ] == df[nrow(df), ])) {
    df <- rbind(df, df[1, ])
  }
  st_polygon(list(as.matrix(df)))
}

# ============================
# Determine back-transformed log normal values
# ============================

logbounds <- function(x) {
  mn  <- mean(log(x), na.rm = TRUE)
  std <- std.error(log(x), na.rm = TRUE)
  
  hibound  <- exp(mn + std)
  lowbound <- exp(mn - std)
  mn       <- exp(mn)
  
  c(hibound = hibound, lowbound = lowbound, mn = mn)
}

# ============================
# Build bioregions after Short et al. (2007)
# ============================
coords_TropicalAtlantic <- data.frame(
  x = c(-81.48, -64.70, -15.21,  23.38,  11.92, -47.92, -77.28, -81.48,
        -88.19, -93.22, -98.54, -106.37, -100.21, -96.30),
  y = c( 31.38,  33.04,  20.44,   0.43, -16.79, -25.60,   5.78,   6.06,
         11.29,  13.76,  14.32,  18.17,  23.40,  31.11)
)

coords_TemperateNorthAtlantic <- data.frame(
  x = c(-78.96, -60.51,  -6.56,   6.60,  56.10,  36.80, -21.92, -90.43,
        -101.05, -106.92),
  y = c( 33.04,  38.54,  36.87,  47.35,  67.45,  74.33,  74.05,  72.13,
         71.58,  65.52)
)

coords_Med <- data.frame(
  x = c(-12.69, -17.44, -19.68, -18.56, -15.21,  33.73,  52.18,
        60.01,  54.14,   5.76,  -6.54),
  y = c( 37.06,  33.04,  27.53,  22.57,  19.55,  30.28,  33.31,
         39.64,  53.41,  47.35,  36.89)
)

coords_Southern <- data.frame(
  x = c(-179.63,  -70.57,   18.35,   29.53,   35.40,  114.26,
        153.41,  180.00,  180.00,  -66.94,  -71.41, -180.00),
  y = c( -25.60,  -25.33,  -31.66,  -24.50,  -23.95,  -23.95,
         -26.70,  -26.70,  -49.28,  -52.86,  -53.68,  -49.00)
)

coords_TropIndoPac1 <- data.frame(
  x = c( 35.40,  32.05, 121.25, 180.00, 180.00, 153.68, 114.26),
  y = c(-23.95,  29.73,  31.38,  32.48, -26.70, -26.70, -23.95)
)

coords_TropIndoPac2 <- data.frame(
  x = c(-180.00, -156.42, -144.39, -139.64, -148.31, -179.63),
  y = c(  30.56,   30.56,   13.76,  -10.19,  -25.60,  -25.60)
)

coords_TempNorthPacific1 <- data.frame(
  x = c(121.25, 114.82, 135.51, 164.59, 180.00, 180.00),
  y = c( 31.11,  38.82,  58.91,  64.14,  64.97,  32.48)
)

coords_TempNorthPacific2 <- data.frame(
  x = c(-106.37, -102.45, -122.30, -150.83, -165.09, -180.00,
        -180.00, -137.68, -110.84),
  y = c(  19.55,   20.10,   52.86,   67.17,   68.00,   69.10,
          30.56,   30.56,   19.00)
)


regions <- list(
  TropicalAtlantic          = coords_TropicalAtlantic,
  TemperateNorthAtlantic   = coords_TemperateNorthAtlantic,
  Mediterranean            = coords_Med,
  TemperateNorthPacific_1  = coords_TempNorthPacific1,
  TemperateNorthPacific_2  = coords_TempNorthPacific2,
  TropicalIndoPacific_1    = coords_TropIndoPac1,
  TropicalIndoPacific_2    = coords_TropIndoPac2,
  Southern                 = coords_Southern
)

polygons_sf <- lapply(regions, make_closed_polygon)
polygons_sf <- st_sfc(polygons_sf, crs = 4326)

regions_sf <- st_sf(
  Region   = c(
    "Tropical Atlantic",
    "Temperate North Atlantic",
    "Mediterranean",
    "Temperate North Pacific",
    "Temperate North Pacific",
    "Tropical Indo-Pacific",
    "Tropical Indo-Pacific",
    "Temperate Southern Oceans"
  ),
  geometry = polygons_sf
)


world <- ne_countries(scale = "medium", returnclass = "sf")

rm(coords_Med,coords_Southern,coords_TemperateNorthAtlantic,coords_TempNorthPacific1,coords_TempNorthPacific2,coords_TropicalAtlantic,coords_TropIndoPac1,coords_TropIndoPac2,regions,polygons_sf)


# ============================
# Graph color pallets
# ============================
npg_colors <- pal_npg("nrc")(10)  
npg_colors<-npg_colors[c(3,5)]
names(npg_colors) <- c("N", "P")

cont_colors <- pal_npg("nrc")(10)
cont_colors <-cont_colors[c(1,2,6,7,9,10)]

# ============================
# Import and reshape data
# ============================
TerrestrialData <- read_excel("TerrestrialCompareN.xlsx") %>% rename(Element = element, Stock_Mg_ha_100cm = stock_Mg_ha_100cm) %>%
  mutate(`Geographic Location` = NA, Country = NA, Continent = NA,
         Latitude = NA, Longitude = NA, Species = NA) %>%
  select(`Geographic Location`, Country, Continent, Latitude, Longitude, Genus, Species,
         Element, Stock_Mg_ha_100cm, everything())

Cores <- gsheet2tbl('https://docs.google.com/spreadsheets/d/118XI3mzbjAosWdcBREMtPC7ENFLwTcLlrvIqx0a7bdI/edit?gid=0') %>%
  select(-1) %>% select(3:5, 7:9, 12:21, 23:34)

long_dat <- Cores %>%
  pivot_longer(cols = matches("Species \\d+|NS\\d+|PS\\d+|DBD\\d+"),
               names_to = c(".value", "species_num"),
               names_pattern = "([A-Za-z ]+)(\\d+)") %>%
  rename(Species = `Species `, N_stock = NS, P_stock = PS, DryBulkDensity = DBD) %>%
  filter(!is.na(Species)) %>%
  mutate(
    DryBulkDensity = if_else(is.na(DryBulkDensity), 1.03, DryBulkDensity),
    `Core Depth (cm)` = if_else(is.na(`Core Depth (cm)`), 10, `Core Depth (cm)`),
    N_stock_Mg_ha_raw = convert_to_MgN_ha(N_stock, `Stock units`, DryBulkDensity, `Core Depth (cm)`),
    N_stock_Mg_ha_100cm = N_stock_Mg_ha_raw * (100 / `Core Depth (cm)`),
    P_stock_Mg_ha_raw = convertP_to_MgHa(P_stock, `P Type`, DryBulkDensity, `Core Depth (cm)`),
    P_stock_Mg_ha_100cm = P_stock_Mg_ha_raw * (100 / `Core Depth (cm)`),
    NP = (N_stock_Mg_ha_100cm/14)/(P_stock_Mg_ha_100cm/30.9)
  ) %>%
  select(`Geographic Location`, Country, Continent, Latitude, Longitude, Species,
         N_stock_Mg_ha_100cm, P_stock_Mg_ha_100cm,NP)
wide_dat <- long_dat
long_dat<- long_dat %>% pivot_longer(cols = c(N_stock_Mg_ha_100cm, P_stock_Mg_ha_100cm),
               names_to = "Element", values_to = "Stock_Mg_ha_100cm") %>%
  drop_na(Stock_Mg_ha_100cm) %>%
  separate(Species, into = c("Genus", "Species"), sep = " ", extra = "merge", fill = "right")%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
long_dat$Latitude<-st_coordinates(long_dat)[,2]
long_dat$Longitude<-st_coordinates(long_dat)[,1]


long_dat <- st_join(long_dat, regions_sf, join = st_nearest_feature)

long_dat<- bind_rows(long_dat, TerrestrialData)


long_dat$Element<-ifelse(long_dat$Element == "P_stock_Mg_ha_100cm","P","N")

long_dat <- long_dat %>%
  group_by(Region) %>%
  mutate(mean_lat = mean(Latitude, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Region = fct_reorder(Region, mean_lat, .na_rm = TRUE))

rm(Cores,TerrestrialData)


# =========================
# Analysis: How do stock sizes vary across genera and regions?
# =========================


FigS3a<-ggplot(long_dat, aes(x = Genus, y = Stock_Mg_ha_100cm, fill = Element)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 21, width = 0.7,
               color = "black", alpha = 0.8) +
  scale_fill_manual(values = npg_colors, labels = c("N", "P")) +
  scale_color_manual(values = npg_colors, guide = "none") +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.position = "none")+scale_y_sqrt()+labs(y = expression("Nutrient Stock (Mg " ~ N ~ ha^-1 ~ ")"),tag = "A")

FigS3b<-ggplot(long_dat %>% filter(is.na(Type)==TRUE), aes(x = Region, y = Stock_Mg_ha_100cm, fill = Element)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 21, width = 0.7,
               color = "black", alpha = 0.8) +
  scale_fill_manual(values = npg_colors, labels = c("N", "P")) +
  scale_color_manual(values = npg_colors, guide = "none") +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.position = "none")+scale_y_sqrt()+labs(y = expression("Nutrient Stock (Mg " ~ N ~ ha^-1 ~ ")"),tag = "B")
FigS3a/FigS3b
ggsave("Products//Nitrogen//genus_compare.png",width = 14, height = 8, units = "in")


# =========================
# Analysis: How do stock sizes vary between species within a region?
# =========================
FigureS4 <- ggplot(long_dat %>% filter(is.na(Region) == FALSE), aes(y = ifelse(is.na(Species), Genus, paste(Genus, Species)), x = Stock_Mg_ha_100cm, fill = Element)) +
  facet_wrap(~Region,scales="free_y")+
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 21, width = 0.7,
               color = "black", alpha = 0.8) +
  scale_fill_manual(values = npg_colors, labels = c("N", "P")) +
  scale_color_manual(values = npg_colors, guide = "none") +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.position = "none")+scale_x_sqrt()
FigureS4
ggsave("Products//Nitrogen//region_compare.png",width = 14, height = 8, units = "in")


# =========================
# Analysis: Data distribution
# =========================
FigureS1a<-ggplot() +
  geom_sf(data = regions_sf, aes(fill = Region), color = NA, alpha = 0.6) +
  geom_sf(data = world, fill = "gray90", color = "gray90", linewidth = 0.3) +
  geom_sf(data = long_dat, size = 1.5, color = "black", alpha = 0.7) +
  scale_fill_manual(values = cont_colors) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.2)
  )+labs(tag = "A")
FigureS1a


sample_counts <- long_dat %>% filter(is.na(Name) == TRUE) %>% 
  group_by(Element) %>%
  summarise(n_samples = sum(!is.na(Stock_Mg_ha_100cm)), .groups = "drop")

FigureS1b<-ggplot(long_dat %>% filter(is.na(Name) == TRUE)) + 
  annotate("text", x = Inf, y = Inf, label = paste("n =",sample_counts$n_samples[1]), hjust = 1.1, vjust = 1, size = 8, color = npg_colors[1])+
  annotate("text", x = Inf, y = Inf, label = paste("n =",sample_counts$n_samples[2]), hjust = 1.1, vjust = 2.5, size = 8, color = npg_colors[2])+
  geom_bar(aes(x = Continent, fill = Element),
           stat = "count",position = "dodge") + scale_fill_manual(values = npg_colors)+
  theme_minimal(base_size = 12)+ylab("Number of Stock Estimates")+xlab("Continent")+theme(legend.position = "") + labs(tag = "B")
FigureS1b
FigureS1a|FigureS1b
ggsave("Products//Nitrogen//map.png",width = 14, height = 8, units = "in")



# =========================
# Analysis: Stock Sizes
# =========================
seagrass_LN <- aggregate(Stock_Mg_ha_100cm ~ Region + Element, data = long_dat, FUN = logbounds) %>%
  { cbind(.[,-c(3)], data.frame(.$Stock_Mg_ha)) }
  #Seagrass distribution data from McKenzie et al. (2020), converted from km2 to ha
temp <- data.frame(
  Region = c(
    "Temperate North Atlantic",
    "Tropical Atlantic",
    "Mediterranean",
    "Temperate North Pacific",
    "Tropical Indo-Pacific",
    "Temperate Southern Oceans"
  ),
  high_conf_area = c(3229, 44222, 14167, 1866, 87791, 9112) * 100,
  low_conf_area  = (c(0, 65231, 10862, 0, 30082, 0) +
            c(3229, 44222, 14167, 1866, 87791, 9112)) * 100
)

seagrass_LN<-merge(seagrass_LN,temp) %>%
  pivot_longer(
    cols = c(high_conf_area, low_conf_area),
    names_to = "Run",
    values_to = "Area"
  ) %>%
  mutate(Run = ifelse(Run == "high_conf_area", "Seagrass - High Conf.", "Seagrass - Low Conf."),
         HighStock = hibound * Area,
         LowStock = lowbound * Area,
         MeanStock = mn * Area) %>%
  group_by(Element, Run) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


terrestrial_LN <- aggregate(Stock_Mg_ha_100cm ~ Genus + Element, data = long_dat %>% filter(is.na(Region) == TRUE), FUN = logbounds) %>%
  { cbind(.[,-c(3)], data.frame(.$Stock_Mg_ha))}
  #Marsh and mangrove distribution data from McKenzie et al. (2020), terrestrial from Soil: The Skin of the Planet Earth (Kutilek and Nielsen - 2015) converted from km2 to ha
temp <- data.frame(
  Genus = c(
    "Mangrove",
    "Marsh",
    "Terrestrial"),
  high_conf_area = c(147359, 52880, 143330000) * 100
)

terrestrial_LN<-merge(terrestrial_LN,temp) %>%
  mutate(Area = high_conf_area,
         HighStock = hibound * Area,
         LowStock = lowbound * Area,
         MeanStock = mn * Area) %>%
         select(-high_conf_area)

colnames(terrestrial_LN)[1] <- "Run"
seagrass_LN <- bind_rows(seagrass_LN, terrestrial_LN)



Figure2<-ggplot(seagrass_LN, aes(x = Run, y = MeanStock/1e6, fill = Element)) +
  geom_bar(stat="identity",color = "black", width = 0.7, alpha = 0.8,position = 'dodge') +
  scale_fill_manual(values = npg_colors) +
  theme_classic(base_size = 12) +geom_errorbar(
    aes(ymin = LowStock/1e6, ymax = HighStock/1e6),
    width = 0.2,
    position = position_dodge(width = 0.8)
  )+
  labs(x = NULL, y = expression(paste("Global Nutrient Stock (Tg)"))) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )+scale_y_break(c(300, 5e4), scales = 0.4,space = 0.5, expand = FALSE)

Figure2
ggsave("Products//Nitrogen//grandTotals.png",width = 14, height = 8, units = "in")

# =========================
# Analysis: NP Ratios
# =========================

temp_wide<-wide_dat %>% filter(NP < 100, NP > 0.1)
median(temp_wide$NP)


continent_summary <- wide_dat %>%
  group_by(Continent) %>%
  summarise(
    mean_N = mean(N_stock_Mg_ha_100cm / 14, na.rm = TRUE),
    sd_N   = sd(N_stock_Mg_ha_100cm / 14, na.rm = TRUE),
    mean_P = mean(P_stock_Mg_ha_100cm / 30.9, na.rm = TRUE),
    sd_P   = sd(P_stock_Mg_ha_100cm / 30.9, na.rm = TRUE)
  )

extreme_sites <- wide_dat %>%
  filter(
    N_stock_Mg_ha_100cm > quantile(N_stock_Mg_ha_100cm, 0.975, na.rm = TRUE) |
      P_stock_Mg_ha_100cm > quantile(P_stock_Mg_ha_100cm, 0.975, na.rm = TRUE)
)

cont_colors <- pal_npg("nrc")(10)
cont_colors <-cont_colors[c(1,2,6,7,9,10)]

FigureS2<-ggplot() +
  # Scatter of all data
  geom_point(
    data = wide_dat,
    aes(x = P_stock_Mg_ha_100cm / 30.9,
        y = N_stock_Mg_ha_100cm / 14,
        color = Continent),
    alpha = 0.5
  ) +
  
  # Means for each continent
  geom_point(
    data = continent_summary,
    aes(x = mean_P, y = mean_N, color = Continent),
    size = 5
  ) +
  
  # Vertical error bars
  geom_errorbar(
    data = continent_summary,
    aes(x = mean_P, ymin = mean_N - sd_N, ymax = mean_N + sd_N, color = Continent),
    width = 0
  ) +
  
  # Horizontal error bars
  geom_errorbarh(
    data = continent_summary,
    aes(y = mean_N, xmin = mean_P - sd_P, xmax = mean_P + sd_P, color = Continent),
    height = 0
  ) +
  geom_abline(slope = 11, intercept = 0, color = "black", linetype = "dashed") +
  theme_classic(base_size = 12) + scale_color_manual(values=cont_colors)+labs(
    x = expression("P Stock (Mmol " ~ P ~ ha^-1 ~ ")"),
    y = expression("N Stock (Mmol " ~ N ~ ha^-1 ~ ")")
  )
FigureS2
ggsave("Products//Nitrogen//NP.png",width = 8, height = 8, units = "in")