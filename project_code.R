############################################################
# TOPIC: Which pitch types and locations are most likely to
# generate an additional strike?
# FILE: project_code.R
# BY: Frances Van Looveren and Andrew Wilson
############################################################

########################
# SETUP
########################

# Packages
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(sf)

########################
# LOAD & CLEAN DATA
########################

# Load 3 years of MLB pitch data (2022-2024)
pitches_22 <- read_csv("updated_pitches_23.csv")
pitches_23 <- read_csv("updated_pitches_22.csv")
pitches_24 <- read_csv("updated_pitches_24.csv")


# Clean data



# Divide into Fastballs, Breaking Balls, and Changeups



########################
# MODELING
########################


# Filters the dataset to one specific situation
# Based on:
# - handedness (R/L)
# - k2 (0 = pre-two-strike, 1 = two-strike)
# - and restricts location to the strike zone window
# NOTE: pitchtype is already a filtered dataset (e.g. only Fastballs)
filterdata <- function(pitchtype, handedness, k2) {
  data <- pitchtype %>%
    filter(
      platelocside < 2,
      platelocside > -2,
      platelocheight < 5,
      platelocheight > 0,
      batterside == handedness,
      strike2 == k2
    )
  return(data)
}

# Fits a GAM on a 10,000-row sample of the filtered dataset
# Used for smoothing parameter (sp) extraction for the full model
sample_gam <- function(data) {
  set.seed(123)  # reproducible sampling
  gam <- gam(
    add_strike ~ 
      te(platelocside, platelocheight) +
      te(inducedvertbreak, horzbreak) +
      s(relspeed) +
      s(scoredif) +
      as.factor(balls) +
      as.factor(outs),
    data = data[sample(nrow(data), 10000), ], # fit model on a random 10,000-row sample
    family = binomial("logit"),
    method = "REML",
    select = TRUE
  )
  return(gam)
}

# Fits the GAM on the full dataset, but uses the smoothing
# parameters (sp) estimated from sample_gam()
full_gam <- function(data) {
  gam <- gam(
    add_strike ~ 
      te(platelocside, platelocheight) +
      te(inducedvertbreak, horzbreak) +
      s(relspeed) +
      s(scoredif) +
      as.factor(balls) +
      as.factor(outs),
    data = data, # fit model on full dataset
    sp = sample_gam(data)$sp, # using sp from the sample gam
    family = binomial("logit"),
    method = "REML",
    select = TRUE
  )
  return(gam)
}

# Fastball Model
model1 <- full_gam(filterdata(fastballs, "L", 0))
model2 <- full_gam(filterdata(fastballs, "R", 0))
model3 <- full_gam(filterdata(fastballs, "L", 1))
model4 <- full_gam(filterdata(fastballs, "R", 1))

# Breaking ball Models
model5 <- full_gam(filterdata(breaking_balls, "L", 0))
model6 <- full_gam(filterdata(breaking_balls, "R", 0))
model7 <- full_gam(filterdata(breaking_balls, "L", 1))
model8 <- full_gam(filterdata(breaking_balls, "R", 1))
 
# Changeup Models
model9 <- full_gam(filterdata(changeups, "L", 0))
model10 <- full_gam(filterdata(changeups, "R", 0))
model11 <- full_gam(filterdata(changeups, "L", 1))
model12 <- full_gam(filterdata(changeups, "R", 1))


    


      # [create R0, R1, L0, L1...]
      # holding at means, creating grid, predict strike prob at location in grid





########################
# VISUALIZATIONS
########################


## R0: Right-Handed Batter, 0 or 1 strikes (pre-two-strike)

# For each location in R0:
# - determine which pitch type has the highest strike probability
# - store that as best_pitch
# - store the corresponding max probability as best_pred
# - rank all locations by best_pred (highest to lowest)
# - create an alpha (transparency) scale so higher-probability locations appear more opaque
R0 <- R0 %>%
  mutate(
    best_pitch = case_when(
      fastball_pred >= bb_pred & fastball_pred >= changeup_pred ~ "Fastball",
      bb_pred >= fastball_pred & bb_pred >= changeup_pred ~ "Breaking Ball",
      TRUE ~ "Changeup"
    ),
    best_pitch = factor(best_pitch,
                        levels = c("Fastball", "Breaking Ball", "Changeup"))
  ) %>%
  mutate(
    best_pred = case_when(
      best_pitch == "Fastball" ~ fastball_pred,
      best_pitch == "Breaking Ball" ~ bb_pred,
      best_pitch == "Changeup" ~ changeup_pred
    )
  ) %>%
  arrange(desc(best_pred)) %>%
  mutate(rank = row_number()) %>%
  mutate(
    alpha = rescale(max(rank) - rank, to = c(0.25, 1))
  )


# Keep the top 50 locations (highest strike probability) to define the "optimal" region
top_R0 <- R0 %>% filter(rank <= 50)
# Compute convex hull around top 50 points to get the optimal polygon region
hull_idx_R0 <- chull(top_R0$platelocside, top_R0$platelocheight)
hull_R0 <- top_R0[hull_idx_R0, ]


# Plot best pitch type by location for R0
ggplot(R0,
       aes(x = platelocside,
           y = platelocheight,
           fill = best_pitch)) +
  # Tile grid showing best pitch type at each location, shaded by alpha (rank-based)
  geom_tile(aes(fill = best_pitch, alpha = alpha),
            color = "#555555", size = 0.15) +
  coord_fixed(ratio = 0.6) +
  # Draw the plate as a thick horizontal segment at y=0
  geom_segment(
    aes(x = -0.7083, xend = 0.7083, y = 0, yend = 0),
    color = "#555555",
    linewidth = 2
  ) +
  # Draw convex hull outlining top 50 location (top ~10%)
  geom_polygon(data = hull_R0,
               aes(x = platelocside, y = platelocheight),
               color = "white", fill = NA, linewidth = 1.2) +
  scale_fill_manual(
    name = "Pitch Type",
    values = c("Fastball" = "#092C5C",
               "Breaking Ball" = "#8FBCE6",
               "Changeup" = "#F5D130")
  ) +
  # Traditional strike zone rectangle overlay
  annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.5, 
           fill = NA, linewidth = 1.2, color = "black"
  ) +
  labs(
    x = "Horizontal location (ft)",
    y = "Vertical location (ft)",
    title = "  RH"
  ) +
  scale_alpha(guide = "none") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )





## L0: Left-Handed Batter, 0 or 1 strikes (pre-two-strike)

L0 <- L0 %>%
  mutate(
    best_pitch = case_when(
      fastball_pred >= bb_pred & fastball_pred >= changeup_pred ~ "Fastball",
      bb_pred >= fastball_pred & bb_pred >= changeup_pred ~ "Breaking Ball",
      TRUE ~ "Changeup"
    ),
    best_pitch = factor(best_pitch,
                        levels = c("Fastball", "Breaking Ball", "Changeup"))
  ) %>%
  mutate(
    best_pred = case_when(
      best_pitch == "Fastball" ~ fastball_pred,
      best_pitch == "Breaking Ball" ~ bb_pred,
      best_pitch == "Changeup" ~ changeup_pred
    )
  ) %>%
  arrange(desc(best_pred)) %>%
  mutate(rank = row_number()) %>%
  mutate(
    alpha = rescale(max(rank) - rank, to = c(0.25, 1))
  )


top_L0 <- L0 %>% filter(rank <= 50)
hull_idx_L0 <- chull(top_L0$platelocside, top_L0$platelocheight)
hull_L0 <- top_L0[hull_idx_L0, ]


ggplot(L0,
       aes(x = platelocside,
           y = platelocheight,
           fill = best_pitch)) +
  geom_tile(aes(fill = best_pitch, alpha = alpha),
            color = "#555555", size = 0.15) +
  coord_fixed(ratio = 0.6) +
  geom_segment(
    aes(x = -0.7083, xend = 0.7083, y = 0, yend = 0),
    color = "#555555",
    linewidth = 2
  ) +
  geom_polygon(data = hull_L0,
               aes(x = platelocside, y = platelocheight),
               color = "white", fill = NA, linewidth = 1.2
  ) +
  annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.5, 
           fill = NA, linewidth = 1.2, color = "black"
  ) +
  scale_fill_manual(
    name = "Pitch Type",
    values = c("Fastball" = "#092C5C",
               "Breaking Ball" = "#8FBCE6",
               "Changeup" = "#F5D130")
  ) +
  labs(
    x = "Horizontal location (ft)",
    y = "Vertical location (ft)",
    title = "  LH"
  ) +
  scale_alpha(guide = "none") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )




## R1: Right-Handed Batter, 2 strikes

R1 <- R1 %>%
  mutate(
    best_pitch = case_when(
      fastball_pred >= bb_pred & fastball_pred >= changeup_pred ~ "Fastball",
      bb_pred >= fastball_pred & bb_pred >= changeup_pred ~ "Breaking Ball",
      TRUE ~ "Changeup"
    ),
    best_pitch = factor(best_pitch,
                        levels = c("Fastball", "Breaking Ball", "Changeup"))
  ) %>%
  mutate(
    best_pred = case_when(
      best_pitch == "Fastball" ~ fastball_pred,
      best_pitch == "Breaking Ball" ~ bb_pred,
      best_pitch == "Changeup" ~ changeup_pred
    )
  ) %>%
  arrange(desc(best_pred)) %>%
  mutate(rank = row_number()) %>%
  mutate(
    alpha = rescale(max(rank) - rank, to = c(0.25, 1))
  )


top_R1 <- R1 %>% filter(rank <= 50)
hull_idx_R1 <- chull(top_R1$platelocside, top_R1$platelocheight)
hull_R1 <- top_R1[hull_idx_R1, ]


ggplot(R1,
       aes(x = platelocside,
           y = platelocheight,
           fill = best_pitch)) +
  geom_tile(aes(fill = best_pitch, alpha = alpha),
            color = "#555555", size = 0.15) +
  coord_fixed(ratio = 0.6) +
  geom_segment(
    aes(x = -0.7083, xend = 0.7083, y = 0, yend = 0),
    color = "#555555",
    linewidth = 2
  ) +
  geom_polygon(data = hull_R1,
               aes(x = platelocside, y = platelocheight),
               color = "white", fill = NA, linewidth = 1.2
  ) +
  annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.5, 
           fill = NA, linewidth = 1.2, color = "black"
  ) +
  scale_fill_manual(
    name = "Pitch Type",
    values = c("Fastball" = "#092C5C",
               "Breaking Ball" = "#8FBCE6",
               "Changeup" = "#F5D130")
  ) +
  labs(
    x = "Horizontal location (ft)",
    y = "Vertical location (ft)",
    title = "  RH"
  ) +
  scale_alpha(guide = "none") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )




## L1: Left-Handed Batter, 2 strikes

L1 <- L1 %>%
  mutate(
    best_pitch = case_when(
      fastball_pred >= bb_pred & fastball_pred >= changeup_pred ~ "Fastball",
      bb_pred >= fastball_pred & bb_pred >= changeup_pred ~ "Breaking Ball",
      TRUE ~ "Changeup"
    ),
    best_pitch = factor(best_pitch,
                        levels = c("Fastball", "Breaking Ball", "Changeup"))
  ) %>%
  mutate(
    best_pred = case_when(
      best_pitch == "Fastball" ~ fastball_pred,
      best_pitch == "Breaking Ball" ~ bb_pred,
      best_pitch == "Changeup" ~ changeup_pred
    )
  ) %>%
  arrange(desc(best_pred)) %>%
  mutate(rank = row_number()) %>%
  mutate(
    alpha = rescale(max(rank) - rank, to = c(0.25, 1))
  )


top_L1 <- L1 %>% filter(rank <= 50)
hull_idx_L1 <- chull(top_L1$platelocside, top_L1$platelocheight)
hull_L1 <- top_L1[hull_idx_L1, ]


# Split hull into two groups and use lower hull (hull1)
set.seed(123)
kfit <- kmeans(top_L1[, c("platelocside", "platelocheight")], centers = 2)
top_L1$cluster <- factor(kfit$cluster)
hulls_L1 <- top_L1 %>%
  group_by(cluster) %>%
  slice(chull(platelocside, platelocheight)) %>%
  ungroup()
hull1 <- hulls_L1 %>% dplyr::filter(cluster == 1)


ggplot(L1,
       aes(x = platelocside,
           y = platelocheight,
           fill = best_pitch)) +
  geom_tile(aes(fill = best_pitch, alpha = alpha),
            color = "#555555", size = 0.15) +
  coord_fixed(ratio = 0.6) +
  geom_segment(
    aes(x = -0.7083, xend = 0.7083, y = 0, yend = 0),
    color = "#555555",
    linewidth = 2
  ) +
  geom_polygon(data = hulls_L1,
               aes(x = platelocside, y = platelocheight, group = cluster),
               color = "white", fill = NA, linewidth = 1.2
  ) +
  annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.5, 
           fill = NA, linewidth = 1.2, color = "black"
  ) +
  scale_fill_manual(
    name = "Pitch Type",
    values = c("Fastball" = "#092C5C",
               "Breaking Ball" = "#8FBCE6",
               "Changeup" = "#F5D130")
  ) +
  labs(
    x = "Horizontal location (ft)",
    y = "Vertical location (ft)",
    title = "  LH"
  ) +
  scale_alpha(guide = "none") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )


























