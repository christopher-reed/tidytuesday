## BACKGROUND -----

# This week we're exploring the (New York) MTA Permanent Art Catalog!

# For each station, find the total works of art segmented by material

## PACKAGES -----

library(tidyverse)
library(tidytuesdayR)

## DATA -----

tuesdata <- tidytuesdayR::tt_load(2025, week = 29)

df_mta_art <- tuesdata$mta_art
df_station_lines <- tuesdata$station_lines

## TIDY DATA -----

# Find the top 3 agencies by total works of art
# Segment art by material

# Top 3 by works of art, considering ties
top_agencies <- df_mta_art %>%
  group_by(agency) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice_max(order_by = n, n = 3, with_ties = TRUE) %>%
  pull(agency)

# For top 3, find num works of art made of each material
  # Create material field
  # Find count by agency and new material field
  # Use `complete` to account for agencies that have 0 works of art made from that material
df_agency_material <- df_mta_art %>%
  filter(agency %in% top_agencies) %>%
  mutate(
    material = case_when(
      str_detect(str_to_lower(art_material), "mosaic|tile") ~ "Mosaic & Tile",
      str_detect(str_to_lower(art_material), "bronze|brass|copper") ~ "Bronze & Copper",
      str_detect(str_to_lower(art_material), "glass|stained") ~ "Glass",
      str_detect(str_to_lower(art_material), "ceramic|porcelain|terra") ~ "Ceramic",
      str_detect(str_to_lower(art_material), "steel|iron|metal") ~ "Steel & Iron",
      str_detect(str_to_lower(art_material), "stone|granite|marble") ~ "Stone",
      str_detect(str_to_lower(art_material), "paint|acrylic|oil") ~ "Paint & Pigments",
      TRUE ~ "Other Materials"
    )
  ) %>%
  group_by(agency, material) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  complete(agency, material, fill = list(n = 0)) %>%
  mutate(agency = factor(agency, levels = top_agencies))

material_labels <- df_agency_material %>%
  distinct(material) %>%
  pull(.)

ggplot(df_agency_material, aes(x = material, y = n, fill = agency)) +
  geom_col() +
  geom_text(aes(label = n), vjust = 0.1) +
  facet_wrap(~agency, ncol = 1, scales = "free_x") +
  scale_x_discrete(breaks = material_labels) +
  labs(
    title = "NYC Transit Has the Most Public Art, with Glass and Mosaic & Tile as Popular Materials",
    subtitle = "Analysis of material preferences across NYC's three largest transit art collections",
    caption = "Source: MTA Permanent Art Catalog\nCreator:christopher-reed",
    x = "",
    y = "Works of Art"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks = element_line(),
    axis.text.x = element_text(angle = 30, hjust = 1))
