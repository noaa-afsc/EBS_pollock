# Assuming 'data' is a data frame with columns 'S' (spawning stock) and 'R' (recruits)

# Transform the data
M$pred_rec 
M<-read_admb(here::here("2024","runs","test","pm"))
M<-read_fit(here::here("2024","runs","test","pm"))
names(M)
R <- M$est[M$names=="pred_rec"][2:60]
S <- M$est[M$names=="SSB"][1:59]
yc <- 1964:2022
length(S)

df <- data.frame(S = S, R = R)

df$log_R_S <- log(df$R / df$S)

# Fit the linear model
ricker_full  <- lm(log_R_S ~ S, data = df)
ricker_short <- lm(log_R_S ~ S, data = df |> filter(year_class>1977))

# Summary of the model
names(ricker_full)
summary(ricker_full)
summary(ricker_short)
exp(ricker_full$coefficients)
exp(ricker_short$coefficients)
df$predicted_log_R_S <- predict(ricker_full, df)
df$predicted_R <- exp(df$predicted_log_R_S) * df$S
df$year_class <- yc
df <- df |> mutate(era=ifelse(year_class < 1978, "pre-1978", "1978 and later"))

# Plot the data and the fitted line
ggplot(df, aes(x = S, y = R, color=era)) +
  geom_point() +
  geom_line(aes(y = predicted_R), color = "blue") +
  labs(title = "Ricker Stock-Recruitment Model",
       x = "Spawning Stock Biomass (S)",
       y = "Recruits (R)") +
  ylim(0, NA) + xlim(0, NA) +
  theme_minimal()

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Example data
set.seed(123)
data <- data.frame(
  S = runif(100, 0, 10),  # Spawning stock biomass
  R = rnorm(100, 50, 10)  # Number of recruits
)
df |> filter(year_class>1977) |> count()


#---Simulate data---
data<-NULL
for (isim in 1:100){
  Rdevs <- rnorm(45, 0, .05)
  data <- rbind(data, df |> filter(year_class>1977) |> 
                  transmute(sim=isim,  S=jitter(S,400),
                            R= R*exp(Rdevs), log_R_S = log(R/S)  ) )
}
data |> ggplot(aes(x=S,y=R,color=sim)) + geom_point() + ylim(c(0,NA)) + xlim(c(0,NA))

# Function to run the simulation and estimation loop with plotting
simulation_loop_with_plot <- function(data, n_sim = 100, train_fraction = 0.8) {
  estimates <- data.frame(intercept = numeric(n_sim), slope = numeric(n_sim))
  
  # Plot setup
  plot <- ggplot(data, aes(x = S, y = R)) +
    geom_point(alpha = 0.6,size=.8) +
    labs(title = "Ricker Stock-Recruitment Model with Simulated Fits",
         x = "Spawning Stock Biomass (S)",
         y = "Recruits (R)") +
    theme_minimal() + ylim(c(0,NA)) + xlim(c(0,NA))
  i=20
  for (i in 1:n_sim) {
    # Split the data into training and testing sets
    set.seed(i)  # For reproducibility
    #train_indices <- sample(1:nrow(data), size = train_fraction * nrow(data))
    train_data <- data[data$sim==i,]
    # For replacement
    train_data <- train_data[sample(1:nrow(train_data), size = nrow(train_data), replace = TRUE), ]
    
    # Fit the model on the training data
    model  <- lm(log_R_S ~ S, data = train_data)
    #model <- lm(R ~ S, data = train_data)
    
    # Save the estimates
    estimates$slope_orig[i] <- exp(coef(model)[1])
    estimates$intercept[i] <- coef(model)[1]
    estimates$slope[i] <- coef(model)[2]
    
    # Add the fitted line to the plot
    prediction_line <- data.frame(S = c(seq(1,1000,by = 50), data$S) )
    #prediction_line$R <- predict(model, newdata = prediction_line)
    predicted_log_R_S <- predict(model, prediction_line)
    prediction_line$R <- exp(predicted_log_R_S) * prediction_line$S
    plot <- plot +
      geom_line(data = prediction_line, aes(x = S, y = R), color = "blue", alpha = 0.2)
  }
  prediction_line$log_R_S <- predict(ricker_short, prediction_line)
  prediction_line$predicted_R <- exp(prediction_line$log_R_S) * prediction_line$S
  plot <- plot +
    geom_line(data = prediction_line, aes(x = S, y = R), size=2,color = "red", alpha = 0.8)
  
  print(plot)
  return(estimates)
}
estimates <- simulation_loop_with_plot(data, n_sim = 100)
ggsave("doc/srr_sim.pdf")
estimates |> ggplot(aes(slope_orig)) + geom_density() + theme_minimal() + xlim(c(0,NA)) +
  geom_vline(xintercept = 56.3031030  )


predicted_log_R_S <- predict(ricker_full, df)
df$predicted_R <- exp(df$predicted_log_R_S) * df$S

median(estimates$slope_orig)
mean(estimates$slope_orig)

# Save the estimates to a CSV file
write.csv(estimates, "estimates.csv", row.names = FALSE)

# View the estimates
head(estimates)
