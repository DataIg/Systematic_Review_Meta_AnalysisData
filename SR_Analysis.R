
# Install and load necessary packages
install.packages("meta")
install.packages("readxl")
library(meta)
library(readxl)

# Load the HbA1c data
hba1c_data <- read_excel("meta_analysis_data.xlsx", sheet = "HbA1c")

# Load the FBG data
fbg_data <- read_excel("meta_analysis_data.xlsx", sheet = "FBG")

# Perform meta-analysis on HbA1c data using only random effects model
meta_hba1c <- metacont(
  n.e = hba1c_data$Total_Intervention,
  mean.e = hba1c_data$Mean_Intervention,
  sd.e = hba1c_data$SD_Intervention,
  n.c = hba1c_data$Total_Control,
  mean.c = hba1c_data$Mean_Control,
  sd.c = hba1c_data$SD_Control,
  studlab = hba1c_data$Author,
  data = hba1c_data,
  sm = "MD",
  comb.fixed = FALSE,       # Ensure fixed effects model is not considered
  comb.random = TRUE,       # Only use random effects model
  method.tau = "REML"       # Restricted Maximum-Likelihood Estimation
)

# Perform meta-analysis on FBG data using only random effects model
meta_fbg <- metacont(
  n.e = fbg_data$Total_Intervention,
  mean.e = fbg_data$Mean_Intervention,
  sd.e = fbg_data$SD_Intervention,
  n.c = fbg_data$Total_Control,
  mean.c = fbg_data$Mean_Control,
  sd.c = fbg_data$SD_Control,
  studlab = fbg_data$Author,
  data = fbg_data,
  sm = "MD",
  comb.fixed = FALSE,       # Ensure fixed effects model is not considered
  comb.random = TRUE,       # Only use random effects model
  method.tau = "REML"       # Restricted Maximum-Likelihood Estimation
)

# Display summary of HbA1c meta-analysis
summary(meta_hba1c)

# Display summary of FBG meta-analysis
summary(meta_fbg)
# Forest plot for HbA1c data - Only Random effects model with dotted line
forest(meta_hba1c, 
       comb.fixed = FALSE,            # Ensure fixed effects model is not displayed
       comb.random = TRUE,            # Show random effects model
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue",   # Left side label color
       lty.random = 2,                # Set random effects line to dotted
       overall = TRUE,                # Display only the random effects model summary
       overall.fixed = FALSE,         # Remove overall fixed effects MD at bottom
       print.I2 = TRUE,               # Show I² for random effects model only
       print.tau2 = TRUE,             # Show tau² for random effects model only
       squaresize = 1                 # Adjust the scaling of the squares by weight
)

# Forest plot for FBG data - Only Random effects model with dotted line
forest(meta_fbg, 
       comb.fixed = FALSE,            # Ensure fixed effects model is not displayed
       comb.random = TRUE,            # Show random effects model
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue",   # Left side label color
       lty.random = 2,                # Set random effects line to dotted
       overall = TRUE,                # Display only the random effects model summary
       overall.fixed = FALSE,         # Remove overall fixed effects MD at bottom
       print.I2 = TRUE,               # Show I² for random effects model only
       print.tau2 = TRUE,             # Show tau² for random effects model only
       squaresize = 1                 # Adjust the scaling of the squares by weight
)


# Sensitivity analysis for HbA1c data with consistent forest plot colors
meta_hba1c_sensitivity <- metainf(meta_hba1c)
print(meta_hba1c_sensitivity)
forest(meta_hba1c_sensitivity, 
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue"    # Left side label color
)

# Sensitivity analysis for FBG data with consistent forest plot colors
meta_fbg_sensitivity <- metainf(meta_fbg)
print(meta_fbg_sensitivity)
forest(meta_fbg_sensitivity, 
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue"    # Left side label color
)



# Funnel plot and trim and fill for HbA1c data
funnel(meta_hba1c, 
       main = "Funnel Plot for HbA1c", 
       xlab = "Mean Difference", 
       ylab = "Standard Error", 
       col = "blue")

# Trim and fill analysis for HbA1c data
trimfill_hba1c <- trimfill(meta_hba1c)
print(trimfill_hba1c)
funnel(trimfill_hba1c, 
       main = "Trim and Fill Funnel Plot for HbA1c", 
       xlab = "Mean Difference", 
       ylab = "Standard Error", 
       col = "red")

# Funnel plot and trim and fill for FBG data
funnel(meta_fbg, 
       main = "Funnel Plot for FBG", 
       xlab = "Mean Difference", 
       ylab = "Standard Error", 
       col = "blue")

# Trim and fill analysis for FBG data
trimfill_fbg <- trimfill(meta_fbg)
print(trimfill_fbg)
funnel(trimfill_fbg, 
       main = "Trim and Fill Funnel Plot for FBG", 
       xlab = "Mean Difference", 
       ylab = "Standard Error", 
       col = "red")




# Subgroup analysis by Age Group for HbA1c
meta_hba1c_age <- metacont(
  n.e = hba1c_data$Total_Intervention,
  mean.e = hba1c_data$Mean_Intervention,
  sd.e = hba1c_data$SD_Intervention,
  n.c = hba1c_data$Total_Control,
  mean.c = hba1c_data$Mean_Control,
  sd.c = hba1c_data$SD_Control,
  studlab = hba1c_data$Author,
  data = hba1c_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = hba1c_data$Age_Group  # Subgroup by Age Group
)

forest(meta_hba1c_age,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)

# Subgroup analysis by Study Duration for HbA1c
meta_hba1c_duration <- metacont(
  n.e = hba1c_data$Total_Intervention,
  mean.e = hba1c_data$Mean_Intervention,
  sd.e = hba1c_data$SD_Intervention,
  n.c = hba1c_data$Total_Control,
  mean.c = hba1c_data$Mean_Control,
  sd.c = hba1c_data$SD_Control,
  studlab = hba1c_data$Author,
  data = hba1c_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = hba1c_data$Study_Duration  # Subgroup by Study Duration
)

forest(meta_hba1c_duration,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)

# Subgroup analysis by Type of Intervention for HbA1c
meta_hba1c_intervention <- metacont(
  n.e = hba1c_data$Total_Intervention,
  mean.e = hba1c_data$Mean_Intervention,
  sd.e = hba1c_data$SD_Intervention,
  n.c = hba1c_data$Total_Control,
  mean.c = hba1c_data$Mean_Control,
  sd.c = hba1c_data$SD_Control,
  studlab = hba1c_data$Author,
  data = hba1c_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = hba1c_data$Intervention_Type  # Subgroup by Type of Intervention
)

forest(meta_hba1c_intervention,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)


# Subgroup analysis by Age Group for FBG
meta_fbg_age <- metacont(
  n.e = fbg_data$Total_Intervention,
  mean.e = fbg_data$Mean_Intervention,
  sd.e = fbg_data$SD_Intervention,
  n.c = fbg_data$Total_Control,
  mean.c = fbg_data$Mean_Control,
  sd.c = fbg_data$SD_Control,
  studlab = fbg_data$Author,
  data = fbg_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = fbg_data$Age_Group  # Subgroup by Age Group
)

forest(meta_fbg_age,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)

# Subgroup analysis by Study Duration for FBG
meta_fbg_duration <- metacont(
  n.e = fbg_data$Total_Intervention,
  mean.e = fbg_data$Mean_Intervention,
  sd.e = fbg_data$SD_Intervention,
  n.c = fbg_data$Total_Control,
  mean.c = fbg_data$Mean_Control,
  sd.c = fbg_data$SD_Control,
  studlab = fbg_data$Author,
  data = fbg_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = fbg_data$Study_Duration  # Subgroup by Study Duration
)

forest(meta_fbg_duration,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)

# Subgroup analysis by Type of Intervention for FBG
meta_fbg_intervention <- metacont(
  n.e = fbg_data$Total_Intervention,
  mean.e = fbg_data$Mean_Intervention,
  sd.e = fbg_data$SD_Intervention,
  n.c = fbg_data$Total_Control,
  mean.c = fbg_data$Mean_Control,
  sd.c = fbg_data$SD_Control,
  studlab = fbg_data$Author,
  data = fbg_data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  byvar = fbg_data$Intervention_Type  # Subgroup by Type of Intervention
)

forest(meta_fbg_intervention,
       col.square = "blue",
       col.square.border = "black",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.label.right = "darkblue",
       col.label.left = "darkblue"
)


# Egger's test for HbA1c meta-analysis
egger_hba1c <- metabias(meta_hba1c, method.bias = "linreg")
print(egger_hba1c)
summary(egger_hba1c)

# Egger's test for FBG meta-analysis
egger_fbg <- metabias(meta_fbg, method.bias = "linreg")
print(egger_fbg)
summary(egger_fbg)

# Forest plot for HbA1c data with Trim and Fill - Fixing overlap
forest(trimfill_hba1c, 
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue",   # Left side label color
       lty.random = 2,                # Set random effects line to dotted
       overall = TRUE,                # Display only the random effects model summary
       overall.fixed = FALSE,         # Remove overall fixed effects MD at bottom
       print.I2 = TRUE,               # Show I² for random effects model only
       print.tau2 = TRUE,             # Show tau² for random effects model only
       squaresize = 1,                # Adjust the scaling of the squares by weight
       cex = 1.2,                     # Adjust text size for labels
       mar = c(6, 8, 4, 2)            # Adjust margins: bottom, left, top, right
)

# Forest plot for FBG data with Trim and Fill - Fixing overlap
forest(trimfill_fbg, 
       col.square = "blue",           # Color of the weight boxes
       col.square.border = "black",   # Border color of the boxes
       col.diamond = "red",           # Color of the diamond
       col.diamond.lines = "black",   # Border color of the diamond
       col.label.right = "darkblue",  # Right side label color
       col.label.left = "darkblue",   # Left side label color
       lty.random = 2,                # Set random effects line to dotted
       overall = TRUE,                # Display only the random effects model summary
       overall.fixed = FALSE,         # Remove overall fixed effects MD at bottom
       print.I2 = TRUE,               # Show I² for random effects model only
       print.tau2 = TRUE,             # Show tau² for random effects model only
       squaresize = 1,                # Adjust the scaling of the squares by weight
       cex = 1.2,                     # Adjust text size for labels
       mar = c(6, 8, 4, 2)            # Adjust margins: bottom, left, top, right
)
