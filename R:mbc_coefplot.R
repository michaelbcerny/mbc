#' @export
mbc_coefplot <- function(models, vars = NULL) {
  # Validate the input
  if (length(models) > 5) {
    stop("This function supports up to 5 models.")
  }
  if (!all(sapply(models, function(x) "lm" %in% class(x)))) {
    stop("All inputs in `models` must be `lm` objects.")
  }
  
  # Process each model
  tidy_models <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- paste("Model", i) # Assign model labels
    
    # Extract 95% and 90% confidence intervals
    tidy_model95 <- tidy(model, conf.int = TRUE, conf.level = 0.95) %>% 
      filter(term != "(Intercept)") %>% 
      rename(conf.low95 = conf.low, conf.high95 = conf.high)
    
    tidy_model90 <- tidy(model, conf.int = TRUE, conf.level = 0.90) %>% 
      filter(term != "(Intercept)") %>% 
      rename(conf.low90 = conf.low, conf.high90 = conf.high) %>% 
      dplyr::select(term, conf.low90, conf.high90)
    
    # Combine intervals and add model name
    tidy_combined <- tidy_model95 %>% 
      left_join(tidy_model90, by = c("term")) %>% 
      filter(term %in% vars) %>% 
      mutate(model = model_name)
    
    return(tidy_combined)
  })
  
  # Combine all models into one data frame
  tidy_all <- bind_rows(tidy_models)
  
  # Create the plot
  tidy_plot <- tidy_all %>% 
    ggplot(aes(x = model, y = estimate), color = "black") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90), width = 0, alpha = 1, size = 1) +
    geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = 0.75, size = 0.5) +
    theme_bw() +
    labs(
      x = "Model",
      y = "Estimate",
      color = "Term"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(-1.25, 0.25)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(tidy_plot)
}
