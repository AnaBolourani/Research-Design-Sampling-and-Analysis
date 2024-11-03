# #__________________________________________________________________________________________________________________
# ### STAT 202A: HW3
# ### Problem 1 

# ### Ana Bolourani
# #__________________________________________________________________________________________________________________

# ================================================
# Brownie Factorial Design Analysis Script
# ================================================

# #Problem 1_Part (a)__________________________________________________________________________________________________________________

# 1. Prompt User to Select the Output Directory
print("Please select the directory where you want to save the output files.\n")

output_dir <- choose.dir()

# Set the output file path
output_file <- file.path(output_dir, "analysis_output.txt")

# Open a connection to the output file
sink(output_file)

cat("======================================================\n")
cat("2³ Factorial Design Results for Brownie Scrumptiousness\n")
cat("======================================================\n\n")
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 2. Prompt User to Select the Data File
cat("Please select the 'brownie.csv' data file.\n")
data_file <- file.choose()

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 3. Read the Data
data <- read.csv(data_file, header = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 4. Data Preparation

# a. Identify the columns that contain test panel scores (assumed to start with 'T')
test_columns <- grep("^T", names(data), value = TRUE)

# b. Convert factor levels to numeric values (-1 and +1)
data$A <- ifelse(data$A == "-", -1, 1)
data$B <- ifelse(data$B == "-", -1, 1)
data$C <- ifelse(data$C == "-", -1, 1)

# c. Construct the design matrix with interaction terms
data$AB <- data$A * data$B
data$AC <- data$A * data$C
data$BC <- data$B * data$C
data$ABC <- data$A * data$B * data$C

# d.Calculate Mean Scrumptiousness per Batch
data$Mean_Scrumptiousness <- rowMeans(data[, test_columns])

# e. Calculate Sum of Scrumptiousness per Batch
data$Sum_Scrumptiousness <- rowSums(data[, test_columns])

# f. Calculate Variance of Scrumptiousness per Batch
data$Variance_Scrumptiousness <- apply(data[, test_columns], 1, var)

# g. Calculate Natural Logarithm of Variance
data$Ln_Variance_Scrumptiousness <- log(data$Variance_Scrumptiousness)

cat("Data Summary\n")
print(data)
cat("__________________________________________________________________________ \n\n")


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 5. Design Matrix:
DesignMatrix <- data[, c("A", "B", "C", "AB", "AC", "BC", "ABC")]

cat("Design Matrix\n")
print(DesignMatrix)
cat("__________________________________________________________________________ \n\n")

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6. Factorial Effects:
effects <- colSums(data[, c("A", "B", "C", "AB", "AC", "BC", "ABC")] * data$Mean_Scrumptiousness) / 4
cat("Main Effects:\n")
cat("A:", effects["A"], "\n")
cat("B:", effects["B"], "\n")
cat("C:", effects["C"], "\n\n")

cat("Interaction Effects_2 factors:\n")
cat("AB:", effects["AB"], "\n")
cat("AC:", effects["AC"], "\n")
cat("BC:", effects["BC"], "\n\n")

cat("Interaction Effects_3 factors:\n")
cat("ABC:", effects["ABC"], "\n\n")

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 7. Ln(s^2) for each Effect:
Ln_effect <- colSums(data[, c("A", "B", "C", "AB", "AC", "BC", "ABC")] * data$Ln_Variance_Scrumptiousness) / 4

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 8. Factorial Effects Table:
Factorial_Table <- data.frame( 
    Effects=c("A", "B", "C", "AB", "AC", "BC", "ABC"),
    Mean=effects,
    Ln_S2=Ln_effect
)

print(Factorial_Table)
cat("__________________________________________________________________________ \n\n")
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 9. Perform 2³ Factorial ANOVA

cat("======================================================\n")
cat("2³ Factorial ANOVA Results for Brownie Scrumptiousness\n")
cat("======================================================\n\n")

# Number of observations per treatment (n)
n <- length(test_columns)

# Total number of observations
N <- nrow(data) * n

# Grand Mean
Grand_Mean <- mean(as.matrix(data[, test_columns]))
cat("Grand Mean:", Grand_Mean, "\n\n")

#  Calculate Sum of Squares for Each Effect
SS_effects <- 2*n*(effects^2) 

# Total Sum of Squares (SSTotal)
Y <- as.matrix(data[, test_columns])
SSTotal <- sum((Y - Grand_Mean)^2)

# Compute Residual Sum of Squares
SS_residual <- SSTotal - sum(SS_effects)

# Degrees of Freedom
DF_total <- N - 1
DF_effects <- 1  # Each effect has 1 degree of freedom
DF_residual <- DF_total - 7  # 7 effects

# Mean Squares
MS_effects <- SS_effects / DF_effects
MS_residual <- SS_residual / DF_residual

# F-values
F_values <- MS_effects / MS_residual

# p-values
p_values <- 1 - pf(F_values, DF_effects, DF_residual)

# Compile ANOVA Table
ANOVA_Table <- data.frame(
  Source = c("A", "B", "C", "AB", "AC", "BC", "ABC", "Residual", "Total"),
  DF = c(rep(DF_effects, 7), DF_residual, DF_total),
  SS = c(SS_effects, SS_residual, SSTotal),
  MS = c(MS_effects, MS_residual, NA),
  F = c(F_values, NA, NA),
  `p-value` = c(p_values, NA, NA)
)

# Display ANOVA Table
cat("ANOVA Table:\n")
print(ANOVA_Table)
cat("__________________________________________________________________________ \n\n")


# 15. Interpret Results
cat("Interpretation of Results:\n\n")

significant_effects <- ANOVA_Table$Source[which(ANOVA_Table$`p-value` < 0.05)]
if ("A" %in% significant_effects) {
  cat("Factor A (Pan Material) has a significant effect on scrumptiousness.\n")
} else {
  cat("Factor A (Pan Material) does not have a significant effect.\n")
}

if ("B" %in% significant_effects) {
  cat("Factor B (Stirring Method) has a significant effect on scrumptiousness.\n")
} else {
  cat("Factor B (Stirring Method) does not have a significant effect.\n")
}

if ("C" %in% significant_effects) {
  cat("Factor C (Brand of Mix) has a significant effect on scrumptiousness.\n")
} else {
  cat("Factor C (Brand of Mix) does not have a significant effect.\n")
}

if (any(c("AB", "AC", "BC", "ABC") %in% significant_effects)) {
  cat("Some interaction effects are significant.\n")
} else {
  cat("No interaction effects are significant.\n")
}
cat("__________________________________________________________________________ \n\n")

# Compute the error variance (MSE)
variances <- apply(data[, 5:12], 1, var)
MSE <- sum(variances * (8 - 1)) / (8 * 7)

# Compute the standard error of effects
SE_effect <- sqrt(MSE / 16)

# Compute t-values
t_values <- effects / SE_effect

# Degrees of freedom
df_error <- 8 * 7

# Critical t-value
alpha <- 0.05
t_critical <- qt(1 - alpha / 2, df = df_error)

# p-values
p_values <- 2 * (1 - pt(abs(t_values), df = df_error))

# Prepare results table
results <- data.frame(
  Effect = c("A", "B", "C", "AB", "AC", "BC", "ABC"),
  Estimate = effects,
  StdError = SE_effect,
  tValue = t_values,
  pValue = p_values
)

# Output the results
print(results)


# e. Close the Sink
sink()

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Generate and Save Main Effects and Interaction Plots

# Convert factor codes to factor variables with labels
data$A_factor <- factor(data$A, levels = c(-1, 1), labels = c("Glass", "Aluminum"))
data$B_factor <- factor(data$B, levels = c(-1, 1), labels = c("Spoon", "Mixer"))
data$C_factor <- factor(data$C, levels = c(-1, 1), labels = c("Expensive", "Cheap"))

# 10.1. Main Effects Plot

# Create a data frame for plotting
plot_data <- data.frame(
  Mean_Scrumptiousness = data$Mean_Scrumptiousness,
  A = data$A_factor,
  B = data$B_factor,
  C = data$C_factor
)

# Main Effects Plot
png(filename = file.path(output_dir, "Main_Effects_Plot.png"))
par(mfrow = c(1, 1)) # Ensure one plot per page
plot.design(Mean_Scrumptiousness ~ A + B + C, data = plot_data,
            main = "Main Effects Plot for Brownie Scrumptiousness")
dev.off()

# plot main effect similar to lecture 4 (char4)
# Assuming 'data' is your dataframe that includes factors A, B, and C along with Mean_Scrumptiousness
# Calculating means for each level of the factors A, B, and C
mean_A <- mean(data$Mean_Scrumptiousness[data$A == 1])
mean_B <- mean(data$Mean_Scrumptiousness[data$B == 1])
mean_C <- mean(data$Mean_Scrumptiousness[data$C == 1])

mean_A_neg <- mean(data$Mean_Scrumptiousness[data$A == -1])
mean_B_neg <- mean(data$Mean_Scrumptiousness[data$B == -1])
mean_C_neg <- mean(data$Mean_Scrumptiousness[data$C == -1])

# Creating a vector for plot
# Creating vectors for plot
factors_A <- c(mean_A_neg, mean_A)
factors_B <- c(mean_B_neg, mean_B)
factors_C <- c(mean_C_neg, mean_C)
labels <- c("- A", "+ A", "- B", "+ B", "- C", "+ C")
factors_all <- c(factors_A, factors_B, factors_C)

# Plotting and saving the file
png(filename = file.path(output_dir, "Main_Effect.png"))  # Open the PNG device
plot(NA, xlim = c(1, 6), ylim = c(min(factors_all) - 0.2, max(factors_all) + 0.2), type = "n", xaxt = 'n', yaxt = 's', ylab = "", xlab = "Factors", main = "Main Effects Plot")
axis(1, at = c(1, 1.5, 3, 3.5, 5, 5.5), labels = labels, las = 1)  # Custom x-axis labels
points(c(1, 1.5), factors_A, pch = 19, type = "b", lty = 1)  # Plot points and lines for factor A
points(c(3, 3.5), factors_B, pch = 19, type = "b", lty = 1)  # Plot points and lines for factor B
points(c(5, 5.5), factors_C, pch = 19, type = "b", lty = 1)  # Plot points and lines for factor C
box()  # Add a box around the plot
dev.off()

#--------------------------------------------------------
# 10.2. Two-Factor Interaction Plots

# Define all pairs for interaction plots
interaction_pairs <- list(
  c("A", "B"),
  c("A", "C"),
  c("B", "C")
)

# Function to create and save interaction plots
create_interaction_plot <- function(factor1, factor2, response, title, x_label, trace_label, filename) {
  png(filename = file.path(output_dir, filename))
  interaction.plot(x.factor = factor1,
                   trace.factor = factor2,
                   response = response,
                   fun = mean,
                   type = "b",
                   pch = c(1,19),
                   col = c("blue", "red"),
                   main = title,
                   xlab = x_label,
                   ylab = "Mean Scrumptiousness",
                   trace.label = trace_label)
  dev.off()
}

# Loop over each pair to create interaction plots
for (pair in interaction_pairs) {
  factor1_name <- pair[1]
  factor2_name <- pair[2]
  
  factor1 <- data[[paste0(factor1_name, "_factor")]]
  factor2 <- data[[paste0(factor2_name, "_factor")]]
  
  title <- paste("Interaction Plot:", factor1_name, "vs", factor2_name)
  x_label <- factor1_name
  trace_label <- factor2_name
  filename <- paste0("Interaction_", factor1_name, "_", factor2_name, ".png")
  
  create_interaction_plot(
    factor1 = factor1,
    factor2 = factor2,
    response = data$Mean_Scrumptiousness,
    title = title,
    x_label = x_label,
    trace_label = trace_label,
    filename = filename
  )
}

#- - - - - - - - - - - - - - - - - - - - - - - - 
# 10.3. Three-Factor Interaction Plot

# Three-way interaction plot using ggplot2
library(ggplot2)

# Create the plot
three_way_plot <- ggplot(data, aes(x = A_factor, y = Mean_Scrumptiousness, color = B_factor, group = B_factor)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ C_factor) +
  labs(title = "Three-Way Interaction Plot: A and B at Each Level of C",
       x = "Pan Material (A)",
       y = "Mean Scrumptiousness",
       color = "Stirring Method (B)") +
  theme_minimal()

# Save the plot
ggsave(filename = file.path(output_dir, "Three_Way_Interaction_Plot.png"), plot = three_way_plot)

#- - - - - - - - - - - - - - - - - - - - - - - - 

#11. Inform the User of Completion
cat("Analysis complete!\n")
cat(paste("Analysis results saved to:", output_file, "\n"))
cat("Main effects and interaction plots saved to the selected output directory.\n")
# ================================================
# End of Script
# ================================================


