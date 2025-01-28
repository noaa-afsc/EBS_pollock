Explanation of the Code

1.	Generate Posterior Predictive Samples: posterior_predict(fit) generates the predictive samples matrix.
2.	Convert to Data Frame: as.data.frame(pp_samples) turns the matrix into a data frame for easier handling with dplyr.
3.	Combine with Observed Data: mutate is used to add the observed data to the same data frame. This aligns each observed value with its corresponding set of predictive samples.
4.	Reshape Data for Percentile Calculation: pivot_longer transforms the wide format of predictive samples into a long format, making it easier to compute percentiles.
5.	Calculate Percentiles: The group_by(row_number()) and summarise operations calculate the percentile for each observation. row_number() provides a unique identifier for each row, corresponding to each observation in the dataset.
6.	Visualization: A histogram is plotted using ggplot2 to visualize the distribution of the calculated percentiles.

By using dplyr and tidyverse functions, this approach makes the code more readable and takes full advantage of the tidy data principles, making it easier to manipulate and analyze your data.

library(brms)
library(tidyverse)

# Assuming `fit` is your fitted brms model and `data` is your observed data

# Generate posterior predictive samples
pp_samples <- posterior_predict(fit)

# Convert the posterior predictive samples to a data frame
pp_df <- as.data.frame(pp_samples)

# Convert observed data to a data frame and ensure it has the same number of rows as pp_samples
observed_df <- data.frame(observed = data$observed)

# Combine the predictive samples and the observed data
combined_df <- pp_df %>%
  mutate(observed = observed_df$observed)

# Calculate percentiles for each observation
percentiles <- combined_df %>%
  pivot_longer(cols = starts_with("V"), names_to = "sample", values_to = "predicted") %>%
  group_by(row_number()) %>%
  summarise(percentile = mean(predicted <= observed)) %>%
  pull(percentile)

# Visualize percentiles using a histogram
percentile_df <- data.frame(percentile = percentiles)
ggplot(percentile_df, aes(x = percentile)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Percentiles",
       x = "Percentile",
       y = "Count")