#load needed libraries
library(readr)
library(meta)

#set to 2 decimal places
settings.meta(digits = 2, method.tau = "PM")

#load the data
dat_rhe <- read_csv("meta_rhe.csv")

#observe structure of the data
str(dat_rhe)

#Create the meta object
meta_obj <- metaprop(
  event = dat_rhe$event,
  n = dat_rhe$denom,
  studlab = paste0(dat_rhe$study, " (", dat_rhe$year, ")", " (", dat_rhe$location, ")"),  # Study labels
  data = dat_rhe,             # Data frame
  sm = "PLOGIT",              # Summary measure, using logit transformation for proportions
  method = "Inverse",         # Method for pooling (e.g., "Inverse" or "DL" for DerSimonian-Laird)
  method.tau = "REML",        # Method for estimating between-study variance (e.g., "REML")
  hakn = TRUE,                # Use Hartung-Knapp adjustment
  title = "Meta-analysis of Proportions"
)

#Print the summary of the meta object
print(summary(meta_obj))

#generate the forest plot and save in jpeg and pdf formats

forest(meta_obj,prediction=TRUE,
       file= "figure1.pdf", width=12)

jpeg("figure1.jpeg", width=14, height=10, units="in", res=300)
forest(meta_obj,sortvar = year, prediction=TRUE)
dev.off()

#Generate the map for the study locations

#load the required libraries for the Map
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load the map of Africa
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Specify the countries you want to highlight
study_areas <- c("Democratic Republic of the Congo",
                         "Djibouti","Eritrea", "Ethiopia",
                         "Kenya","Libya","Malawi","Morocco","Mozambique",
                         "Rwanda","Senegal","Sudan","Tanzania",
                         "Uganda","Zambia","Zimbabwe","Nigeria", 
                         "South Africa", "Kenya")
                         

# Create a new column to flag the countries to be highlighted
africa$studyareas <- ifelse(africa$name %in% study_areas, "Study Areas", "Other")


# Plot the map
map <- ggplot(data = africa) +
  geom_sf(aes(fill = studyareas), color = "black") +
  geom_text(data = subset(africa, studyareas == "Study Areas"),
            aes(label = name, geometry = geometry),
            stat = "sf_coordinates",
            size = 2, color = "black") +
  scale_fill_manual(values = c("Study Areas" = "green", "Other" = "gray80")) +
  theme_minimal() +
  labs(title = "Location of Studies", fill = "Study Locations") +
  theme(legend.position = "bottom")



# Export the map as a JPEG image
ggsave("study_areas.jpeg", map, width = 10, height = 8, units = "in", dpi = 350)


#Perform the sub-group analysis
m.sub = update(meta_obj, subgroup = region, print.subgroup.name = FALSE)
m.sub


#forest plot for sub-group analysis
forest(m.sub, sortvar = year,
       xlim = c(0.1, 100), at = c(0.1, 0.3, 1, 3, 10, 30, 100),
       test.subgroup.common = FALSE,
       label.test.subgroup.random = "Test for subregion differences:",
       file = "figure2.pdf", width = 14)


#Save sub-group image to jpeg
jpeg("figure2.jpeg", width=15, height=16, units="in", res=300)
forest(m.sub, sortvar = year, 
       xlim = c(0.1, 100), at = c(0.1, 0.3, 1, 3, 10, 30, 100),
       test.subgroup.common = FALSE,
       label.test.subgroup.random = "Test for subregion differences:")
dev.off()

#update for github
