# ******** Wildfire and Opioid Use Model v4 - 9/19/2023 *********
# R model adapted from excel. See excel for notes on data inputs.

library(dplyr)

# Define fixed parameters
opioid_no_anxiety <- 0.047
prevalence_anxiety <- 0.066
rr_opioid_given_fire_induced_anxiety <- 5
rr_opioid_given_anxiety_potentiation <- 5

# Define model as a function
fire2opioid <- function(or_opioid_given_anxiety, prev_anxiety_given_fire) {
  
  # Base scenario
  
  opioid_w_anxiety <- opioid_no_anxiety * or_opioid_given_anxiety
  overall_opioid <- opioid_no_anxiety * (1 - prevalence_anxiety) + opioid_w_anxiety * prevalence_anxiety
  
  attributable_risk <- (overall_opioid - opioid_no_anxiety) / overall_opioid
  
  rr_anxiety_given_fire <- prev_anxiety_given_fire / prevalence_anxiety
  opioid_given_fire <- opioid_no_anxiety* (1 - prev_anxiety_given_fire) + opioid_w_anxiety * prev_anxiety_given_fire
  rr_opioid_given_fire <- opioid_given_fire / overall_opioid
  
  # Exploratory 1: 
  
  opioid_given_fire_1 <- opioid_no_anxiety * (1 - prev_anxiety_given_fire) + 
    opioid_w_anxiety * prevalence_anxiety + 
    rr_opioid_given_fire_induced_anxiety * opioid_no_anxiety * (prev_anxiety_given_fire - prevalence_anxiety)
  rr_opioid_given_fire_1 <- opioid_given_fire_1 / overall_opioid
  
  #  Exploratory 2: 
  
  opioid_given_fire_2 <- opioid_no_anxiety * (1-prev_anxiety_given_fire) + 
    opioid_no_anxiety * rr_opioid_given_anxiety_potentiation * prev_anxiety_given_fire
  rr_opioid_given_fire_2 <- opioid_given_fire_2 / overall_opioid
  
  return(data.frame(rr_anxiety_given_fire = rr_anxiety_given_fire,
                    overall_opioid = overall_opioid,
                    opioid_given_fire = opioid_given_fire,
                    rr_opioid_given_fire = rr_opioid_given_fire, 
                    opioid_given_fire_1 = opioid_given_fire_1,
                    rr_opioid_given_fire_1 = rr_opioid_given_fire_1, 
                    opioid_given_fire_2 = opioid_given_fire_2,
                    rr_opioid_given_fire_2 = rr_opioid_given_fire_2))
}

# Create a summary function to present results
my_summary <- function(data_matrix) {
  
  # Calculate summary statistics for each column
  summary_stats <- apply(data_matrix, 2, function(column_data) {
    mean_value <- mean(column_data)
    sd_value <- sd(column_data)
    ci <- t.test(column_data)$conf.int
    uncertainty_range <- quantile(column_data, c(0.025, 0.975))
    c(Mean = mean_value, SD = sd_value, CI_lower = ci[1], CI_upper = ci[2],
      UI_lower = uncertainty_range[1], UI_upper = uncertainty_range[2])
  })
  
  # Convert the result to a data frame
  summary_df <- as.data.frame(summary_stats)
  rownames(summary_df) <- c("Mean", "SD", "CI_lower", "CI_upper", "UI_lower", "UI_upper")
  summary_df <- t(summary_df)
  summary_df <- data.frame(Output = rownames(summary_df), summary_df)
  rownames(summary_df) <- NULL
  
  return(summary_df)
}


# Set up Monte Carlo
my_simulation <- function(n_simulations, prev_cutoff_flag=FALSE) {
  # Create empty vectors to store samples for both parameters at each run
  run_or <- numeric(n_simulations)
  run_prev <- numeric(n_simulations)
  
  # Create empty data frame to store model results after each run
  model_results <- as.data.frame(matrix(nrow = n_simulations, ncol = 8))
  rownames(model_results) = 1:n_simulations
  colnames(model_results) = c("rr_anxiety_given_fire",
                              "overall_opioid",
                              "opioid_given_fire",
                              "rr_opioid_given_fire", 
                              "opioid_given_fire_1",
                              "rr_opioid_given_fire_1", 
                              "opioid_given_fire_2",
                              "rr_opioid_given_fire_2")
  
  for (i in 1:n_simulations) {
    # Sample values for 'or_opioid_given_anxiety' and 'prev_anxiety_given_fire' from distributions
    run_or[i] <- rlnorm(1, meanlog = log(3.04), sdlog = 0.065)
    run_prev[i] <- rbeta(1, shape1 = 10.5, shape2 = 70)
    
    if (prev_cutoff_flag) {
      run_prev[i] <- max(prevalence_anxiety, run_prev[i])
    }
    
    # Calculate the results using  sampled parameters
    model_results[i, ] <- fire2opioid(or_opioid_given_anxiety = run_or[i], prev_anxiety_given_fire = run_prev[i])
  }
  
  summary_result <- my_summary(model_results)
  return(list(model_results, summary_result))
}


#Run Monte Carlo

set.seed(55555)
set.seed(Sys.time())

sim1 <- my_simulation(50000)
sim1_results <- sim1[[1]]
sim1_summary <- sim1[[2]]

set.seed(55555)

sim2 <- my_simulation(50000)
sim2_results <- sim2[[1]]
sim2_summary <- sim2[[2]]

# set.seed(55555)
# sim2 <- my_simulation(50000, prev_cutoff_flag = TRUE)
# sim2_results <- sim2[[1]]


library(ggplot2)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(reshape)
library(ggpubr)


my_figures <- function(results) {
  
  
  theme_set(theme_few(base_size=12))
  pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  fig_prev <- ggplot(melt(results[ ,c(3,5,7)])) +
    stat_density(aes(x = value, color = variable),
                 geom = "line", position = "identity", size = 0.8) +
    scale_color_manual(NULL, values = pal, labels=c("Scenario 1 (Base Case)", "Scenario 2", "Scenario 3")) +
    labs(x="Prevalence", y="Probability Density") +
    geom_vline(xintercept = 0.053, linetype = "dotted")
  
  fig_rr <- ggplot(melt(results[ ,c(4,6,8)]), aes(x = value, color = variable)) +
    stat_density(aes(x = value, color = variable),
                 geom = "line", position = "identity", size = 0.8) +
    scale_color_manual(values = pal, name = NULL, 
                       labels = c("Scenario 1", "Scenario 2", "Scenario 3")) +
    labs(x="Prevalence Ratio", y="Probability Density") +
    geom_vline(xintercept = 1.0, linetype = "dotted") +
    scale_x_continuous(breaks = c(1.0, 1.3, 1.6, 1.9),
                       labels = c("1.0\n(5.3%)", "1.3\n(6.9%)", "1.6\n(8.5%)", "1.9\n(10.1%)"))
  
  
  ggarrange(fig_prev, fig_rr, ncol=2, common.legend=TRUE, legend="bottom")
}


# Sim1 has RRs set to 5
sim1_no_cutoff <- my_figures(sim1_results)
sim1_no_cutoff


# Sim2 has RRs set to 10
sim2_no_cutoff <- my_figures(sim2_results)
sim2_no_cutoff
