## Data Preprocessing

### Packages

library(tidyverse)
library(knitr)
library(kableExtra)
library(depmixS4)

### Loading in Groove MIDI

all_hits <- read_csv("all_hits.csv")
all_hits <- all_hits %>%
  mutate(drummer = factor(drummer, levels = paste0("drummer", 1:10)))

cat("Total hits loaded:", nrow(all_hits), "\n\n")

kable(all_hits %>% 
        count(drummer) %>% 
        arrange(desc(n)))

cat("\n\nColumn names:\n")
print(colnames(all_hits))

cat("\n\nSample data:\n")
kable(head(all_hits, 10))

cat("\n\nVelocity summary:\n")
print(summary(all_hits$velocity))

cat("\n\nPitch (drum types) present:\n")
print(sort(unique(all_hits$pitch)))

# Number of hits per drummer
kable(all_hits %>% 
        count(drummer) %>% 
        arrange(desc(n)),
      caption = "Number of Hits per Drummer")

#Number of recordings per drummer
recordings_per_drummer <- all_hits %>%
  group_by(drummer) %>%
  summarise(num_recordings = n_distinct(id)) %>%
  arrange(desc(num_recordings))

#combined Table for display in "Methods" section
kable(all_hits %>%
        group_by(drummer) %>%
        summarise(Recordings = n_distinct(id),Hits = n()) %>%
        mutate(`% Rec.` = round(100 * Recordings / sum(Recordings), 1),
               `% Hits` = round(100 * Hits / sum(Hits), 1)) %>%
        arrange(desc(Recordings)) %>% 
        bind_rows(summarise(.,
                            drummer = "**Total**",
                            Recordings = sum(Recordings),
                            Hits = sum(Hits),
                            `% Rec.` = 100.0,
                            `% Hits` = 100.0)),
      caption = "Data Distribution per Drummer",
      col.names = c("Drummer", "Rec.", "Hits", "% Rec.", "% Hits"),
      format.args = list(big.mark = ","),
      align = c("l", "r", "r", "r", "r")
)

kable(recordings_per_drummer,
      caption = "Number of Recordings per Drummer",
      col.names = c("Drummer", "Recordings"))

# Hits per drummer bar chart
p1 <- ggplot(all_hits %>% count(drummer), aes(x = reorder(drummer, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_bw() +
  labs(title = "Total Hits per Drummer",
       x = "Drummer",
       y = "Number of Hits") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p1

# Recordings per drummer bar chart
p2 <- ggplot(recordings_per_drummer <- all_hits %>%
               group_by(drummer) %>%
               summarise(num_recordings = n_distinct(id)) %>%
               arrange(desc(num_recordings)), aes(x = reorder(drummer, num_recordings), y = num_recordings)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_bw() +
  labs(title = "Total Recordings per Drummer",
       x = "Drummer",
       y = "Number of Recordings") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p2

## Feature Engineering

# Per drummer features
drummer_features <- all_hits %>%
  group_by(drummer) %>%
  summarise(
    # Velocity features
    mean_velocity = mean(velocity, na.rm = TRUE),
    sd_velocity = sd(velocity, na.rm = TRUE),
    median_velocity = median(velocity, na.rm = TRUE),
    velocity_range = max(velocity) - min(velocity),
    velocity_cv = sd(velocity) / mean(velocity),
    # Timing features  
    mean_ioi = mean(inter_onset_interval, na.rm = TRUE),
    sd_ioi = sd(inter_onset_interval, na.rm = TRUE),
    median_ioi = median(inter_onset_interval, na.rm = TRUE),
    cv_ioi = sd(inter_onset_interval, na.rm = TRUE) / mean(inter_onset_interval, na.rm = TRUE),
    # Density
    total_hits = n(),
    # Drum voice usage (22 MIDI pitches)
    prop_pitch_22 = sum(pitch == 22) / n(),  
    prop_pitch_26 = sum(pitch == 26) / n(),
    prop_pitch_36 = sum(pitch == 36) / n(), 
    prop_pitch_37 = sum(pitch == 37) / n(),  
    prop_pitch_38 = sum(pitch == 38) / n(),  
    prop_pitch_40 = sum(pitch == 40) / n(), 
    prop_pitch_42 = sum(pitch == 42) / n(),  
    prop_pitch_43 = sum(pitch == 43) / n(),  
    prop_pitch_44 = sum(pitch == 44) / n(), 
    prop_pitch_45 = sum(pitch == 45) / n(), 
    prop_pitch_46 = sum(pitch == 46) / n(),  
    prop_pitch_47 = sum(pitch == 47) / n(), 
    prop_pitch_48 = sum(pitch == 48) / n(), 
    prop_pitch_49 = sum(pitch == 49) / n(),  
    prop_pitch_50 = sum(pitch == 50) / n(), 
    prop_pitch_51 = sum(pitch == 51) / n(), 
    prop_pitch_52 = sum(pitch == 52) / n(),  
    prop_pitch_53 = sum(pitch == 53) / n(), 
    prop_pitch_55 = sum(pitch == 55) / n(),  
    prop_pitch_57 = sum(pitch == 57) / n(),  
    prop_pitch_58 = sum(pitch == 58) / n(), 
    prop_pitch_59 = sum(pitch == 59) / n()  
  )

kable(drummer_features)

# Velocity dist. by drummer
p3 <- ggplot(all_hits, aes(x = velocity, fill = drummer)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(title = "Velocity Distributions by Drummer",
       x = "Velocity (0-127)",
       y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p3

# Inter-onset interval dist. by drummer
p4 <- all_hits %>%
  filter(!is.na(inter_onset_interval), 
         inter_onset_interval > 0,
         inter_onset_interval < 1) %>%  # Remove outliers
  ggplot(aes(x = inter_onset_interval, fill = drummer)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(title = "Inter-Onset Interval Distributions by Drummer",
       x = "Time Between Hits (seconds)",
       y = "Density") +
  theme(legend.position = "bottom")  +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p4

# Pitch dist. by drummer
p5 <- ggplot(all_hits, aes(x = pitch, fill = drummer)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(title = "Pitch (Drum Type) Distributions by Drummer",
       x = "MIDI Pitch Number",
       y = "Density") +
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p5

# Time dist. by drummer
p6 <- all_hits %>%
  group_by(drummer) %>%
  mutate(time_normalized = time / max(time)) %>%  # Normalize to 0-1
  ggplot(aes(x = time_normalized, fill = drummer)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(title = "Hit Density Over Time by Drummer",
       x = "Normalized Time (0-1)",
       y = "Density") +
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )
p6

## PCA on Drummer Features

#Feature matrix for PCA
X <- drummer_features %>%
  dplyr::select(-drummer, -total_hits) %>%
  as.matrix()

rownames(X) <- drummer_features$drummer

# Running PCA
pca_result <- prcomp(X, scale. = TRUE)

pca_scores <- data.frame(
  drummer = drummer_features$drummer,
  PC1 = pca_result$x[, 1],
  PC2 = -pca_result$x[, 2],
  PC3 = pca_result$x[, 3]
)

# Variance explained
summary(pca_result)

#Variance info from PCA summary
pca_summary <- summary(pca_result)$importance
variance_df <- as.data.frame(t(pca_summary))  

# Component name column
variance_df <- data.frame(
  Component = rownames(variance_df),
  variance_df
)
rownames(variance_df) <- NULL

# Table
kable(variance_df, 
      digits = 3, 
      caption = "Variance Explained by Principal Components",
      col.names = c("Component", "Standard Deviation", "Proportion of Variance", "Cumulative Proportion"),
) %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Top contributors to each PC
top_pc <- function(rotation, pc_index, k = 5) {
  sort(abs(rotation[, pc_index]), decreasing = TRUE)[1:k]
}

top_pc1 <- top_pc(pca_result$rotation, 1)
top_pc2 <- top_pc(pca_result$rotation, 2)
top_pc3 <- top_pc(pca_result$rotation, 3)

top_loadings_df <- data.frame(
  PC = c(rep("PC1", 5), rep("PC2", 5), rep("PC3", 5)),
  Feature = c(names(top_pc1), names(top_pc2), names(top_pc3)),
  Loading = c(top_pc1, top_pc2, top_pc3)
)

# Table
kable(top_loadings_df, caption = "Top 5 Feature Loadings for PC1–PC3",
      col.names = c("Principal Component", "Feature", "Absolute Loading"),
      align = "c") %>%
  group_rows("PC1", 1, 5) %>%
  group_rows("PC2", 6, 10) %>%
  group_rows("PC3", 11, 15) %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Scree plot
plot(pca_result$sdev, type='b', 
     xlab='Principal Component', 
     ylab='Standard Deviation', 
     main='Scree Plot')

# PC scores
kable(pca_scores, digits = 2, caption = "Principal Component Scores by Drummer") %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Pairs plot
pairs(pca_result$x[, 1:3], main="PCA: First 3 Components")

# PC1 vs PC2 plot
ggplot(pca_scores, aes(x = PC1, y = PC2, label = drummer)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.8) +
  geom_text(vjust = -0.7, size = 3, family = "serif") +
  theme_bw() +
  labs(
    title = "Drummers in PCA Space (PC1 vs PC2)",
    x = "PC1",
    y = "PC2"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )

kable(pca_result$rotation[,1:3], digits = 3, 
      caption = "Feature Loadings on First 3 PCs")

## KDE

# Calculating the bandwidth for velocity
bw_vel <- 1.06 * sd(all_hits$velocity) * length(all_hits$velocity)^(-1/5)

#Calculating the bandwidth for IOI
all_hits_ioi <- all_hits %>%
  filter(!is.na(inter_onset_interval), inter_onset_interval < 1)

bw_ioi <- 1.06 * sd(all_hits_ioi$inter_onset_interval) * 
  length(all_hits_ioi$inter_onset_interval)^(-1/5)

# IOI KDE by drummer
ggplot(all_hits_ioi, aes(x = inter_onset_interval, color = drummer)) +
  geom_density(bw = bw_ioi) +
  theme_bw() +
  labs(title = "Inter-Onset Interval Distributions by Drummer (Gaussian KDE)",
       x = "Inter-Onset Interval (seconds)", y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )

# Velocity KDE - faceted
ggplot(all_hits, aes(x = velocity)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~drummer, scales = "free_y", ncol = 5) +
  theme_bw() +
  labs(title = "Velocity Distributions by Drummer (Individual)",
       x = "Velocity", y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )

# Extract KDE features
kde_summary <- all_hits %>%
  group_by(drummer) %>%
  summarise(
    # Velocity
    vel_mean = mean(velocity),
    vel_sd = sd(velocity),
    vel_range = max(velocity) - min(velocity),
    vel_cv = sd(velocity) / mean(velocity),
    # IOI
    ioi_mean = mean(inter_onset_interval, na.rm = TRUE),
    ioi_sd = sd(inter_onset_interval, na.rm = TRUE),
    ioi_cv = sd(inter_onset_interval, na.rm = TRUE) / 
      mean(inter_onset_interval, na.rm = TRUE)
  )

# Merging with PCA
integration_df <- pca_scores %>%
  left_join(kde_summary, by = "drummer")

# Integration Table
kable(integration_df, digits = 2,
      caption = "Integration: PC Scores and Velocity/Timing Characteristics", 
) %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Calculating correlations
cor_pc2_vel_mean <- cor(integration_df$PC2, integration_df$vel_mean, use = "complete.obs")
cor_pc2_vel_cv <- cor(integration_df$PC2, integration_df$vel_cv, use = "complete.obs")
cor_pc3_ioi_cv <- cor(integration_df$PC3, integration_df$ioi_cv, use = "complete.obs")
cor_pc1_vel_mean <- cor(integration_df$PC1, integration_df$vel_mean, use = "complete.obs")

#Correlation table
correlation_results <- data.frame(
  Relationship = c("PC2 vs Mean Velocity",
                   "PC2 vs Velocity CV",
                   "PC3 vs IOI CV",
                   "PC1 vs Mean Velocity"),
  Correlation = c(cor_pc2_vel_mean,
                  cor_pc2_vel_cv,
                  cor_pc3_ioi_cv,
                  cor_pc1_vel_mean),
  stringsAsFactors = FALSE
)

# Table
kable(correlation_results,
      digits = 3,
      caption = "Correlations Between PC Scores and Velocity/Timing Statistics",
      col.names = c("Relationship", "Correlation (r)"),
      align = c("l", "c"),
      ,
      row.names = FALSE) %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Velocity KDE by drummer
ggplot(all_hits, aes(x = velocity, color = drummer)) +
  geom_density(bw = bw_vel) +
  theme_bw() +
  labs(title = "Velocity Distributions by Drummer (Gaussian KDE)",
       x = "Velocity", y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )

# IOI KDE - faceted
all_hits_ioi %>%
  ggplot(aes(x = inter_onset_interval)) +
  geom_density(fill = "coral", alpha = 0.6) +
  facet_wrap(~drummer, scales = "free_y", ncol = 5) +
  theme_bw() +
  labs(title = "IOI Distributions by Drummer (Individual)",
       x = "Inter-Onset Interval (seconds)", y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"),
  )

## Hidden Markov Models

# Subsetting to one rock recording per drummer
rock_recordings <- all_hits %>%
  filter(grepl("rock", style, ignore.case = TRUE)) %>%
  group_by(drummer) %>%
  arrange(desc(n())) %>%
  slice(1) %>%
  distinct(drummer, id)

# Fitting a 3-state HMM for each drummer
# the 3 states are soft, medium, loud dynamics
hmm_results <- list()

for (i in 1:nrow(rock_recordings)) {
  d <- rock_recordings$drummer[i]
  rec_id <- rock_recordings$id[i]
  
  rec_data <- all_hits %>%
    filter(id == rec_id) %>%
    arrange(time) %>%
    dplyr::select(velocity)
  
  if (nrow(rec_data) > 100) {
    set.seed(123)
    hmm_model <- depmix(velocity ~ 1, 
                        data = rec_data,
                        nstates = 3, 
                        family = gaussian())
    
    hmm_fit <- tryCatch(
      fit(hmm_model, verbose = FALSE),
      error = function(e) { return(NULL) }
    )
    
    if (!is.null(hmm_fit)) {
      # Extracting parameters
      trans_matrix <- matrix(NA, nrow = 3, ncol = 3)
      for (j in 1:3) {
        trans_matrix[j, ] <- hmm_fit@transition[[j]]@parameters$coefficients
      }
      
      state_means <- numeric(3)
      for (j in 1:3) {
        state_means[j] <- hmm_fit@response[[j]][[1]]@parameters$coefficients[1]
      }
      
      state_order <- order(state_means)
      state_means_sorted <- state_means[state_order]
      trans_sorted <- trans_matrix[state_order, state_order]
      
      # Getting our results here to display
      hmm_results[[as.character(d)]] <- list(
        drummer = as.character(d),
        n_hits = nrow(rec_data),
        state_means = state_means_sorted,
        transition_matrix = trans_sorted,
        persistence = mean(diag(trans_sorted)),
        switching = mean(trans_sorted[row(trans_sorted) != col(trans_sorted)])
      )
    }
  }
}

# summary table (Not displaying this, putting it into a kable)
hmm_summary <- data.frame(
  drummer = sapply(hmm_results, function(x) x$drummer),
  n_hits = sapply(hmm_results, function(x) x$n_hits),
  soft_state = sapply(hmm_results, function(x) round(x$state_means[1], 1)),
  medium_state = sapply(hmm_results, function(x) round(x$state_means[2], 1)),
  loud_state = sapply(hmm_results, function(x) round(x$state_means[3], 1)),
  persistence = sapply(hmm_results, function(x) round(x$persistence, 3)),
  switching = sapply(hmm_results, function(x) round(x$switching, 3))
)

# HMM Summary Table
kable(hmm_summary, 
      caption = "HMM Characteristics: Rock Performances",
      col.names = c("Drummer", "Hits", "Soft", "Medium", "Loud", 
                    "Persistence", "Switching"),
      digits = 2) %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

# Merging the HMM results with PCA/KDE integration
integration_hmm <- integration_df %>%
  left_join(hmm_summary, by = "drummer")

kable(integration_hmm %>%
        dplyr::select(drummer, PC2, vel_mean, vel_cv, loud_state, persistence),
      digits = 2,
      caption = "HMM Integration with PCA and KDE Results") %>%
  kable_styling(latex_options=c("hold_position"), font_size=8)

#Using the same colors for consistency
steelblue_pal <- colorRampPalette(c("white", "steelblue"))(20)

# Plotting a single transition matrix for drummer1 
par(mar = c(4, 4, 3, 2), family = "serif")
if ("drummer1" %in% names(hmm_results)) {
  A <- hmm_results[["drummer1"]]$transition_matrix 
  
  # Heatmap 
  image(1:3, 1:3, t(A)[,3:1], col = steelblue_pal, 
        main = "Transition Matrix: Drummer 1", 
        xlab = "To State", 
        ylab = "From State", axes = FALSE) 
  axis(1, at = 1:3, labels = c("Soft", "Med", "Loud"), cex.axis = 1) 
  axis(2, at = 1:3, labels = c("Loud", "Med", "Soft"), cex.axis = 1) 
  
  # Probability values 
  for (i in 1:3) { 
    for (j in 1:3) {
      text(j, 4-i, sprintf("%.2f", A[i,j]), col = "black", cex = 1.2, font = 2) 
    } 
  } 
  
  # Caption
  mtext("Diagonal values = state persistence | Off-diagonal = switching probability", 
        side = 1, line = 4, cex = 0.9) 
}

## Appendix C Visualizations

kable(all_hits %>% 
        count(drummer) %>% 
        arrange(desc(n)),
      caption = "Number of Hits per Drummer")

recordings_per_drummer <- all_hits %>%
  group_by(drummer) %>%
  summarise(num_recordings = n_distinct(id)) %>%
  arrange(desc(num_recordings))

kable(recordings_per_drummer,
      caption = "Number of Recordings per Drummer",
      col.names = c("Drummer", "Recordings"))

#turning the steel blue color to a gradient (one-sided)
steelblue_pal <- colorRampPalette(c("white", "steelblue"))(20)

# Plotting the transition matrices
par(mfrow = c(3, 4),
    mar = c(3, 3, 2, 1),
    family = "serif")   # match your ggplot serif theme

for (d in names(hmm_results)) {
  A <- hmm_results[[d]]$transition_matrix
  
  # Heatmap
  image(1:3, 1:3, t(A)[,3:1], 
        col = steelblue_pal,
        main = d,
        xlab = "", ylab = "",
        axes = FALSE)
  
  axis(1, at = 1:3, labels = c("Soft", "Med", "Loud"),
       cex.axis = 0.8)
  axis(2, at = 1:3, labels = c("Loud", "Med", "Soft"),
       cex.axis = 0.8)
  
  # Probability values
  for (i in 1:3) {
    for (j in 1:3) {
      text(j, 4-i, sprintf("%.2f", A[i,j]),
           col = "black", cex = 0.9)
    }
  }
}

#Legend
plot.new()
text(0.5, 0.75, "Transition Probabilities", cex = 1.3, font = 2)
text(0.5, 0.55, "Rows = From State",       cex = 1)
text(0.5, 0.45, "Columns = To State",     cex = 1)
text(0.5, 0.35, "Diagonal = Persistence", cex = 1)
text(0.5, 0.25, "Off-diagonal = Switching", cex = 1)