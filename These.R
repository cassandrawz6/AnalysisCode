setwd("~/Desktop/Master/These/R/")

# 1. Charger les packages nécessaires
library(tidyverse)   # ggplot2 + dplyr inclus
library(readxl)      # pour importer le fichier Excel
library(rstatix)     # pour les tests Shapiro, Levene, Kruskal, etc.
library(ggpubr)      # pour ggboxplot

# Lire les données
data <- read_excel("Biel 2022-2024 phenotypic data.xlsx")

### Region - Wet Weight

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_clean <- data %>%
  dplyr::select(Region, `Wet weight (g)`) %>%
  filter(!is.na(Region), !is.na(`Wet weight (g)`)) %>%
  rename(Wet_weight = `Wet weight (g)`) %>%
  mutate(Wet_weight = as.numeric(Wet_weight))

# 🔹 1. Visualisation
ggboxplot(data_clean, x = "Region", y = "Wet_weight", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Wet Weight by Region", y = "Wet Weight (g)", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_clean %>%
  group_by(Region) %>%
  shapiro_test(Wet_weight)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_clean %>%
  levene_test(Wet_weight ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Wet_weight ~ Region, data = data_clean)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_clean %>%
    kruskal_test(Wet_weight ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_clean %>%
    dunn_test(Wet_weight ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}

### Region - Liver Weight

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_liver <- data %>%
  dplyr::select(Region, `Liver weight (g)`) %>%
  filter(!is.na(Region), !is.na(`Liver weight (g)`)) %>%
  rename(Liver_weight = `Liver weight (g)`) %>%
  mutate(Liver_weight = as.numeric(Liver_weight))

# 🔹 1. Visualisation
ggboxplot(data_liver, x = "Region", y = "Liver_weight", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Liver Weight by Region", y = "Liver Weight (g)", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_liver %>%
  group_by(Region) %>%
  shapiro_test(Liver_weight)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_liver %>%
  levene_test(Liver_weight ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Liver_weight ~ Region, data = data_liver)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_liver %>%
    kruskal_test(Liver_weight ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_liver %>%
    dunn_test(Liver_weight ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}


### Region - Total Length

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_length <- data %>%
  dplyr::select(Region, `Total length (cm)`) %>%
  filter(!is.na(Region), !is.na(`Total length (cm)`)) %>%
  rename(Total_length = `Total length (cm)`) %>%
  mutate(Total_length = as.numeric(Total_length))

# 🔹 1. Visualisation
ggboxplot(data_length, x = "Region", y = "Total_length", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Length by Region", y = "Total Length (mm)", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_length %>%
  group_by(Region) %>%
  shapiro_test(Total_length)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_length %>%
  levene_test(Total_length ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Total_length ~ Region, data = data_length)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_length %>%
    kruskal_test(Total_length ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_length %>%
    dunn_test(Total_length ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}


### Region - Gill Rakers

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_gill <- data %>%
  dplyr::select(Region, `Gill rakers`) %>%
  filter(!is.na(Region), !is.na(`Gill rakers`)) %>%
  rename(Gill_rakers = `Gill rakers`) %>%
  mutate(Gill_rakers = as.numeric(Gill_rakers))

# 🔹 1. Visualisation
ggboxplot(data_gill, x = "Region", y = "Gill_rakers", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gill Rakers by Region", y = "Number of Gill Rakers", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_gill %>%
  group_by(Region) %>%
  shapiro_test(Gill_rakers)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_gill %>%
  levene_test(Gill_rakers ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Gill_rakers ~ Region, data = data_gill)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_gill %>%
    kruskal_test(Gill_rakers ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_gill %>%
    dunn_test(Gill_rakers ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}


### Region - Gonad Weight

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_gonad <- data %>%
  dplyr::select(Region, `Gonad weight (g)`) %>%
  filter(!is.na(Region), !is.na(`Gonad weight (g)`)) %>%
  rename(Gonad_weight = `Gonad weight (g)`) %>%
  mutate(Gonad_weight = as.numeric(Gonad_weight))

# 🔹 1. Visualisation
ggboxplot(data_gonad, x = "Region", y = "Gonad_weight", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gonad Weight by Region", y = "Gonad Weight (g)", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_gonad %>%
  group_by(Region) %>%
  shapiro_test(Gonad_weight)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_gonad %>%
  levene_test(Gonad_weight ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Gonad_weight ~ Region, data = data_gonad)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_gonad %>%
    kruskal_test(Gonad_weight ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_gonad %>%
    dunn_test(Gonad_weight ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}


### Region - Gonad/Wet Weight Ratio

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_ratio <- data %>%
  dplyr::select(Region, `Gonad/WetWeight`) %>%
  filter(!is.na(Region), !is.na(`Gonad/WetWeight`)) %>%
  rename(Gonad_Wet_ratio = `Gonad/WetWeight`) %>%
  mutate(Gonad_Wet_ratio = as.numeric(Gonad_Wet_ratio))

# 🔹 1. Visualisation
ggboxplot(data_ratio, x = "Region", y = "Gonad_Wet_ratio", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gonad/Wet Weight Ratio by Region", y = "Gonad/Wet Weight Ratio", x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_ratio %>%
  group_by(Region) %>%
  shapiro_test(Gonad_Wet_ratio)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_ratio %>%
  levene_test(Gonad_Wet_ratio ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Gonad_Wet_ratio ~ Region, data = data_ratio)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_ratio %>%
    kruskal_test(Gonad_Wet_ratio ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_ratio %>%
    dunn_test(Gonad_Wet_ratio ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}


### Region - Liver/Wet Weight Ratio

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_ratio_liver <- data %>%
  dplyr::select(Region, `Liver/WetWeight`) %>%
  filter(!is.na(Region), !is.na(`Liver/WetWeight`)) %>%
  rename(Liver_Wet_ratio = `Liver/WetWeight`) %>%
  mutate(Liver_Wet_ratio = as.numeric(Liver_Wet_ratio))

# 🔹 1. Visualisation
ggboxplot(data_ratio_liver, x = "Region", y = "Liver_Wet_ratio", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Liver/Wet Weight Ratio by Region", 
       y = "Liver/Wet Weight Ratio", 
       x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_ratio_liver %>%
  group_by(Region) %>%
  shapiro_test(Liver_Wet_ratio)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_ratio_liver %>%
  levene_test(Liver_Wet_ratio ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Liver_Wet_ratio ~ Region, data = data_ratio_liver)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_ratio_liver %>%
    kruskal_test(Liver_Wet_ratio ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_ratio_liver %>%
    dunn_test(Liver_Wet_ratio ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}

### Region - Sum Milt

# Nettoyer les données (garder seulement les colonnes nécessaires)
data_milt <- data %>%
  dplyr::select(Region, `Sum Milt (contaminated and non contaminated)`) %>%
  filter(!is.na(Region), !is.na(`Sum Milt (contaminated and non contaminated)`)) %>%
  rename(Sum_milt = `Sum Milt (contaminated and non contaminated)`) %>%
  mutate(Sum_milt = as.numeric(Sum_milt))

# 🔹 1. Visualisation
ggboxplot(data_milt, x = "Region", y = "Sum_milt", 
          color = "Region", palette = "jco", add = "jitter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sum Milt by Region", 
       y = "Sum Milt", 
       x = "Region")

# 🔹 2. Tester les hypothèses

# a. Normalité (Shapiro-Wilk)
normality_test <- data_milt %>%
  group_by(Region) %>%
  shapiro_test(Sum_milt)
print(normality_test)

# b. Homogénéité des variances (Levene)
levene_result <- data_milt %>%
  levene_test(Sum_milt ~ Region)
print(levene_result)

# 🔹 3 & 4. Choisir le test et l’exécuter
if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
  # ANOVA
  anova_result <- aov(Sum_milt ~ Region, data = data_milt)
  summary(anova_result)
  
  # Post-hoc si significatif
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
} else {
  # Kruskal-Wallis
  kruskal_result <- data_milt %>%
    kruskal_test(Sum_milt ~ Region)
  print(kruskal_result)
  
  # Post-hoc pairwise si besoin
  posthoc <- data_milt %>%
    dunn_test(Sum_milt ~ Region, p.adjust.method = "bonferroni")
  print(posthoc)
}

####

analyze_by_region <- function(data, variable, region_col = "Region") {
  library(tidyverse)
  library(rstatix)
  library(ggpubr)
  
  # Vérifie si variable existe
  if (!(variable %in% colnames(data))) {
    stop(paste("La variable", variable, "n'existe pas dans les données."))
  }
  
  # Préparation des données
  df <- data %>%
    dplyr::select(all_of(region_col), all_of(variable)) %>%
    filter(!is.na(.data[[region_col]]), !is.na(.data[[variable]])) %>%
    rename(Region = all_of(region_col),
           Value = all_of(variable)) %>%
    mutate(Value = as.numeric(Value))
  
  # 🔹 1. Visualisation
  print(
    ggboxplot(df, x = "Region", y = "Value",
              color = "Region", palette = "jco", add = "jitter") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste(variable, "by Region"), y = variable, x = "Region")
  )
  
  # 🔹 2. Tester les hypothèses
  normality_test <- df %>%
    group_by(Region) %>%
    shapiro_test(Value)
  print(normality_test)
  
  levene_result <- df %>%
    levene_test(Value ~ Region)
  print(levene_result)
  
  # 🔹 3 & 4. Test global + Post-hoc
  if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
    cat("\n✅ Conditions normales remplies → ANOVA\n")
    anova_result <- aov(Value ~ Region, data = df)
    print(summary(anova_result))
    
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
  } else {
    cat("\n⚠️ Conditions non remplies → Kruskal-Wallis\n")
    kruskal_result <- df %>%
      kruskal_test(Value ~ Region)
    print(kruskal_result)
    
    posthoc <- df %>%
      dunn_test(Value ~ Region, p.adjust.method = "bonferroni")
    print(posthoc)
  }
}

analyze_by_region(data, "Wet weight (g)")
analyze_by_region(data, "Gonad/WetWeight")
analyze_by_region(data, "Liver/WetWeight")
analyze_by_region(data, "Sum Milt (contaminated and non contaminated")
analyze_by_region(data, "Total length (cm)")
analyze_by_region(data, "Gill rakers")
analyze_by_region(data, "Gonad weight (g)")
analyze_by_region(data, "Liver weight (g)")
