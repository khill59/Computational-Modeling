library(tidyverse)
> library(gganimate)
> library(gifski)
> 
> # Define the DDM trial function
> ddm_trial <- function(drift, threshold, non_decision_time,
 starting_point, dt = 0.001, max_time = 10) {
+ time <- 0
+ evidence <- starting_point
+ evidence_path <- c(evidence)
+
+ while (time < max_time) {
+ evidence <- evidence + drift * dt + sqrt(dt) * rnorm(1)
+ time <- time + dt
+ evidence_path <- c(evidence_path, evidence)
+         
+ if (evidence >= threshold || evidence <= 0) {
+ break
+  }
+  }
+     
+ choice <- ifelse(evidence >= threshold, 1, 0)
+ rt <- time + non_decision_time
+     
+ return(list(
+ rt = rt,
+ choice = choice,
+ evidence_path = evidence_path,
+ time = seq(0, time, length.out = length(evidence_path))
+     ))
+ }
> 
> # Set up simulation parameters
> threshold <- 1.0
> non_decision_time <- 0.2
> starting_point <- 0.5
> num_trials <- 20
> 
> priming_conditions <- c("Unprimed", "Primed")
> drift_rates <- c(0.3, 0.7)
> 
> # Run the simulation
> results <- tibble()
> 
> for (condition in 1:2) {
+ for (i in 1:num_trials) {
+ # Determine whether the word appeared on the study list
+ list_status <- ifelse(i %% 2 == 1, "Listed", "Not listed")
+         
+ # Set drift rate based on priming condition
+ drift <- drift_rates[condition]
+         
+ # Evidence moves toward the expected decision boundary
+ if (list_status == "Not listed") {
+ drift <- -drift
+         }
+         
# Use the same random sequence for matching trials
+ set.seed(1000 + i)
+         
+ trial_result <- ddm_trial(
+ drift = drift,
+ threshold = threshold,
+ non_decision_time = non_decision_time,
+ starting_point = starting_point
                      +         )
                    +         
                      +         # Record the correct response
                      +         correct_response <- ifelse(list_status == "Listed", 1, 0)
                      +         
                        +         # Store trial information
                        +         trial_data <- tibble(
                          +             trial = i,
                          +             condition = priming_conditions[condition],
                          +             list_status = list_status,
                          +             time = trial_result$time,
                          +             evidence = trial_result$evidence_path,
                          +             rt = trial_result$rt,
                          +             choice = trial_result$choice,
                          +             correct = as.integer(trial_result$choice == correct_response)
                          +         )
                        +         
                          +         results <- bind_rows(results, trial_data)
                          +     }
            + }
        > 
          > # Calculate average response time and accuracy
          > summary_results <- results %>%
            +     distinct(condition, trial, list_status, rt, correct) %>%
            +     group_by(condition) %>%
            +     summarise(
              +         average_response_time = mean(rt),
              +         accuracy = mean(correct) * 100,
              +         .groups = "drop"
              +     )
          > 
            > print(summary_results)
          # A tibble: 2 × 3
          condition average_response_time accuracy
          <chr>                     <dbl>    <dbl>
            1 Primed                    0.450       70
          2 Unprimed                  0.525       65
          > 
            > # Create the base plot
            > p <- ggplot(
              +     results,
              +     aes(
                +         x = time,
                +         y = evidence,
                +         group = interaction(condition, trial)
                +     )
              + ) +
            +     geom_line(aes(color = factor(choice))) +
            +     geom_hline(
              +         yintercept = c(0, threshold),
              +         linetype = "dashed"
              +     ) +
            +     labs(
              +         title = "Priming and Recognition Response Time",
              +         subtitle = "Condition: {closest_state}",
              +         x = "Time (seconds)",
              +         y = "Evidence",
              +         color = "Decision (1 = Yes, 0 = No)"
              +     ) +
            +     theme_minimal() +
            +     coord_cartesian(ylim = c(-0.1, threshold + 0.1))
          > 
            > # Add animation
            > anim <- p +
            +     transition_states(
              +         condition,
              +         transition_length = 2,
              +         state_length = 1
              +     ) +
            +     enter_fade() +
            +     exit_fade()
          > 
            > # Render the GIF
            > gif <- animate(
              +     anim,
              +     nframes = 200,
              +     fps = 10,
              +     width = 800,
              +     height = 600,
              +     renderer = gifski_renderer()
              + )