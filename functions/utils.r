# Capturing the first number group from survey responses
return_numeric <- function(x) {as.numeric(gsub("([0-9]+).*$", "\\1", x))} # regular expression

# Recode binary responses 
reverse_dummy <- function(data){
    data[data == 1] <- 2
    data[data == 0] <- 1
    data[data == 2] <- 0
    return(data)
}

# Running factor analysis 
run_fa <- function(df){
    
    fa(df, 
       nfactors = 2, # two factors  
       rotate = 'oblimin', 
       fm = 'ml') # ML estimation 
    
}

# Turn factor loadings as a dataframe
df_fa_loadings <- function(fa_res){
    
    # Extract factor loadings 
    fa_df <- fa_res$loadings %>%
        unclass() %>%
        as.data.frame()
    
    # Turn rownames into a column 
    fa_df <- fa_df %>%
        dplyr::add_rownames(var = "Measures")
    
    # Rename columns and reshape the dataframe 
    fa_df %>%
        rename("Micro-aggression" = "ML1",
               "Discrimination" = "ML2") %>%
        pivot_longer(
            c(2:3), 
            names_to = "Factor", 
            values_to = "Loading")
}

# Visualize factor loadings 
visualize_fa_loadings <- function(df){
    
    # The order of the variables 
    var_order <- c(
        
        # discrimination
        "promotion_denied", "unfairly_fired", "job_rejected", "police_brutality", "housing_discrimination", "neighbor_hostility", 
                   
        # micro-aggression
         "service_unfriendly", "english_proficiency", "afraid_of_you", "thought_dishonest", "insulted", "threatened", "name_mispronounced")
    
    df %>%
        ggplot(aes(x = factor(Measures, levels = var_order), y = Loading, fill = Loading)) +
            geom_col() +
            coord_flip() +
            scale_fill_gradient2(name = "Loading",
                high = "blue", mid = "white", low = "red", midpoint = 0, guide = F) +
            labs(y = "Loading Strength", x = "Measures",
                title = "Factor Analysis Results",
                caption = "Source: National Asian American Survey (2016)")
}

# Turn estimated factor scores as a dataframe
df_fa_weights <- function(fa_res) {
    fa_res$weights %>%
        unclass() %>%
        as.data.frame() %>%
        dplyr::add_rownames(var = "Measures") %>%
            rename("Micro-aggression" = "ML1",
                   "Discrimination" = "ML2") %>%
            pivot_longer(
                c(2:3), 
                names_to = "Factor", 
                values_to = "Weights")
}

}

# Calculate standard errors 
std_mean <- function(x) sd(x)/sqrt(length(x))

# Extract weights 
extract_weights <- function(nested, var_list, var_condition){
    nested %>%
        select(race, fa_weights) %>%
        unnest(fa_weights) %>%
        filter(Measures %in% var_list) %>%
        filter(Factor == var_condition) %>%
        rename("Variables" = "Measures") %>%
        select(-Factor)
}

# Summarize key information 
summarize_scores <- function(df) {
    df %>%
        summarise(mean = mean(Scores), # summarize mean, standard deviation, and n 
                  sd = sd(Scores),
                  n = n()) %>%
            mutate(se = sd / sqrt(n), # calculate standard errors and confidence intervals 
                   lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                   upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 
}

summarize_weighted_scores <- function(df) {
    df %>%
        summarise(mean = Hmisc::wtd.mean(Scores, Weights), # summarize mean, standard deviation, and n 
                  sd = sqrt(Hmisc::wtd.var(Scores, Weights)),
                  n = n()) %>%
        mutate(se = sd / sqrt(n), # calculate standard errors and confidence intervals 
               lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 
}


# Split, summarise, then bind
group_summarize <- function(base, type) {

    disc <- base %>% filter(Variables %in% c("promotion_denied", "unfairly_fired", "job_rejected", "police_brutality", "housing_discrimination", "neighbor_hostility"))

    microagg <- base %>% filter(Variables %in% c("service_unfriendly", "english_proficiency", "afraid_of_you", "thought_dishonest", "insulted", "threatened", "name_mispronounced"))

    bind_rows(
        disc %>% ungroup(Variables) %>% 
        summarize_scores() %>% 
        mutate(
            dimension = "Discrimination",
            Type = type
        ),
    
        microagg %>% ungroup(Variables) %>% 
        summarize_scores() %>% 
        mutate(
            dimension = "Micro-aggression",
            Type = type
        ))
}

group_summarize_weight <- function(base, vars_nested, type) {
    
    disc <- base %>% filter(Variables %in% c("promotion_denied", "unfairly_fired", "job_rejected", "police_brutality", "housing_discrimination", "neighbor_hostility"))
    
    microagg <- base %>% filter(Variables %in% c("service_unfriendly", "english_proficiency", "afraid_of_you", "thought_dishonest", "insulted", "threatened", "name_mispronounced"))
    
    disc_weighted <- extract_weights(vars_nested, c("promotion_denied", "unfairly_fired", "job_rejected", "police_brutality", "housing_discrimination", "neighbor_hostility"), "Discrimination")
    
    microagg_weighted <- extract_weights(vars_nested, c("service_unfriendly", "english_proficiency", "afraid_of_you", "thought_dishonest", "insulted", "threatened", "name_mispronounced"), "Micro-aggression")
    
    disc <- left_join(disc, disc_weighted)
    microagg <- left_join(microagg, microagg_weighted) 
    
    bind_rows(
        disc %>% ungroup(Variables) %>% 
            summarize_weighted_scores() %>% 
            mutate(
                dimension = "Discrimination",
                Type = type
            ),
        
        microagg %>% ungroup(Variables) %>% 
            summarize_weighted_scores() %>% 
            mutate(
                dimension = "Micro-aggression",
                Type = type
            ))
}