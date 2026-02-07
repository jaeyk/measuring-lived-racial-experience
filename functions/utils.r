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
    
    df %>%
        ggplot(aes(x = Measures, y = Loading, fill = Loading)) +
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

# Calculate standard errors 
std_mean <- function(x) sd(x)/sqrt(length(x))

# Extract weights 
extract_weights <- function(nested, var_list, var_condition){
    nested %>%
        select(race, fa_weights) %>%
        unnest(fa_weights) %>%
        filter(str_detect(Measures, var_list)) %>%
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

    disc <- base %>% filter(str_detect(Variables, "disc"))

    microagg <- base %>% filter(str_detect(Variables, "micro"))

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
    
    disc <- base %>% filter(str_detect(Variables, "disc"))
    
    microagg <- base %>% filter(str_detect(Variables, "micro"))
    
    disc_weighted <- extract_weights(vars_nested, "disc", "Discrimination")
    
    microagg_weighted <- extract_weights(vars_nested, "micro", "Micro-aggression")
    
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

# Build regression models 
ols <- function(df, dv){
    
    lm_out <- lm_robust(df[[dv]] ~ discrimination + micro_aggression + age + educ6 + income + ownhome + male + forborn + democrat + republican, data = df, fixed_effects = states)
    
    tidy(lm_out, conf.int = TRUE)
}


# Visualize regression models 
viz_ols <- function(df, dv_type) {
    
    df %>%
        mutate(term = recode(term, 
                             "discrimination" = "Discrimination",
                             "micro_aggression" = "Micro-aggression")) %>%
        ggplot(aes(x = race, y = estimate, ymax = conf.high, ymin = conf.low, col = term)) +
            geom_pointrange() +
            coord_flip() +
            facet_wrap(~group, ncol = 2) +
            ggrepel::geom_text_repel(aes(label = round(estimate, 2))) +
            scale_color_discrete(labels = c("Discrimination", "Micro-aggression")) +
            labs(title = glue("DV: {dv_type}"),
                 subtitle = glue("Covariates: Age, Education, Income, Homeownership, Gender, Foreign born status, Partisanship
                                 Including state fixed effects"),
                 x = "Race",
                 y = "Estimate",
                 color = "IVs",
                 caption = "Source: National Asian American Survey (2016)") +
            geom_hline(yintercept = c(0), linetype = "dotted")
    
}

viz_ols_no_facet <- function(df, dv_type) {
    
    df %>%
        ggplot(aes(x = race, y = estimate, ymax = conf.high, ymin = conf.low, color = term)) +
        geom_pointrange(size = 1) +
        coord_flip() +
        scale_color_discrete(labels = c("Discrimination", "Micro-aggression")) +
        labs(title = glue("DV: {dv_type}"),
             subtitle = glue("Covariates: Age, Education, Income, Homeownership, Gender, Foreign born status, Partisanship
                             Including state fixed effects"),
             x = "Race",
             y = "Estimate",
             color = "IVs",
             caption = "Source: National Asian American Survey (2016)") +
        geom_hline(yintercept = c(0), linetype = "dotted")
    
}

# Unnest model results 
unnest_model <- function(df, nested_obj, group_label) {
    
    df %>%
        dplyr::select("race", {{nested_obj}}) %>%
        unnest({{nested_obj}}) %>%
        filter(str_detect(term, "disc|micro")) %>%
        filter(race != "Multiracial") %>%
        mutate(group = group_label)

}

# Pooled interaction model (race x IV) with state fixed effects
ols_interaction <- function(df, dv, ref_level = "Black") {

    # Filter out Multiracial and relevel race
    df_sub <- df %>%
        filter(race != "Multiracial") %>%
        mutate(race = relevel(factor(race), ref = ref_level))

    # Build formula
    fml <- as.formula(paste0(
        dv, " ~ discrimination * race + micro_aggression * race + ",
        "age + educ6 + income + ownhome + male + forborn + democrat + republican"
    ))

    lm_robust(fml, data = df_sub, fixed_effects = ~states)
}

# Compute marginal effects (total effect = main + interaction) for each race/IV
compute_marginal_effects <- function(model) {

    coefs <- coef(model)
    vcov_mat <- vcov(model)

    # Identify race levels from interaction terms
    race_terms <- grep("^race", names(coefs), value = TRUE)
    race_levels <- unique(gsub(".*race", "", race_terms))

    results <- list()

    for (iv in c("discrimination", "micro_aggression")) {
        # Reference group: main effect only
        est_ref <- coefs[iv]
        se_ref <- sqrt(vcov_mat[iv, iv])
        results[[length(results) + 1]] <- tibble(
            race = levels(model$model$race)[1],  # reference level
            term = iv,
            estimate = est_ref,
            std.error = se_ref,
            conf.low = est_ref - 1.96 * se_ref,
            conf.high = est_ref + 1.96 * se_ref
        )

        # Other groups: main + interaction
        for (rl in race_levels) {
            int_term <- paste0(iv, ":race", rl)
            if (!int_term %in% names(coefs)) {
                int_term <- paste0("race", rl, ":", iv)
            }
            if (!int_term %in% names(coefs)) next

            est <- coefs[iv] + coefs[int_term]
            se <- sqrt(vcov_mat[iv, iv] + vcov_mat[int_term, int_term] +
                           2 * vcov_mat[iv, int_term])
            results[[length(results) + 1]] <- tibble(
                race = rl,
                term = iv,
                estimate = est,
                std.error = se,
                conf.low = est - 1.96 * se,
                conf.high = est + 1.96 * se
            )
        }
    }

    bind_rows(results) %>%
        mutate(term = recode(term,
                             "discrimination" = "Discrimination",
                             "micro_aggression" = "Micro-aggression"))
}

# Wald test: discrimination = micro_aggression within a racial group
test_disc_vs_micro <- function(model, race_level) {

    coefs <- coef(model)
    vcov_mat <- vcov(model)
    ref_level <- levels(model$model$race)[1]

    if (race_level == ref_level) {
        # Reference group: test discrimination - micro_aggression = 0
        diff <- coefs["discrimination"] - coefs["micro_aggression"]
        se <- sqrt(vcov_mat["discrimination", "discrimination"] +
                       vcov_mat["micro_aggression", "micro_aggression"] -
                       2 * vcov_mat["discrimination", "micro_aggression"])
    } else {
        # Other groups: (disc + disc:raceX) - (micro + micro:raceX) = 0
        disc_int <- paste0("discrimination:race", race_level)
        micro_int <- paste0("micro_aggression:race", race_level)

        # Try alternative ordering if needed
        if (!disc_int %in% names(coefs))
            disc_int <- paste0("race", race_level, ":discrimination")
        if (!micro_int %in% names(coefs))
            micro_int <- paste0("race", race_level, ":micro_aggression")

        terms <- c("discrimination", disc_int, "micro_aggression", micro_int)
        # Contrast vector: disc + disc:race - micro - micro:race
        contrast <- c(1, 1, -1, -1)

        diff <- sum(contrast * coefs[terms])
        se <- sqrt(as.numeric(t(contrast) %*% vcov_mat[terms, terms] %*% contrast))
    }

    z <- diff / se
    p <- 2 * pnorm(-abs(z))

    tibble(
        race = race_level,
        estimate_diff = diff,
        se_diff = se,
        z_stat = z,
        p_value = p
    )
}