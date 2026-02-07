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
run_fa <- function(df, use_poly = FALSE){
    
    cor_method <- if(use_poly) "poly" else "cor"
    
    fa(df, 
       nfactors = 2, # two factors  
       rotate = 'oblimin', 
       fm = 'ml', # ML estimation 
       cor = cor_method) 
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
ols <- function(df, dv, weights = NULL){
    
    # Check if data is multiply imputed (has .imp column with >1 value)
    is_mi <- ".imp" %in% names(df) && length(unique(df$.imp)) > 1
    
    if (is_mi) {
        # Multiply imputed data: fit on each imputation and pool
        
        # Split data by imputation
        imp_list <- split(df, df$.imp)
        
        # Fit model on each imputation
        fits <- lapply(imp_list, function(d) {
             lm_robust(as.formula(paste0(dv, " ~ discrimination + micro_aggression + age + educ6 + income + ownhome + male + forborn + democrat + republican")), 
                       data = d, 
                       weights = if(!is.null(weights)) d[[weights]] else NULL,
                       fixed_effects = ~states)
        })
        
        # Pool results (custom simplified pooling since estimatr objects don't play nice with mice::pool directly sometimes)
        # We use Rubin's Rules on the coefficients and SEs
        
        # Extract coefficients and variances
        coefs <- do.call(rbind, lapply(fits, coef))
        vars <- do.call(rbind, lapply(fits, function(x) diag(vcov(x))))
        
        m <- length(fits)
        
        # Pooled estimates
        q_bar <- colMeans(coefs)
        
        # Within-imputation variance
        u_bar <- colMeans(vars)
        
        # Between-imputation variance
        b <- apply(coefs, 2, var)
        
        # Total variance
        t_var <- u_bar + (1 + 1/m) * b
        se_pooled <- sqrt(t_var)
        
        # Degrees of freedom (Barnard & Rubin 1999)
        v_m <- (m - 1) * (1 + u_bar / ((1 + 1/m) * b))^2
        df_pooled <- v_m # simplified
        
        # T-statistics and p-values
        t_stat <- q_bar / se_pooled
        p_val <- 2 * pt(-abs(t_stat), df = df_pooled)
        
        # Confidence intervals
        alpha <- 0.05
        crit <- qt(1 - alpha/2, df_pooled)
        conf_low <- q_bar - crit * se_pooled
        conf_high <- q_bar + crit * se_pooled
        
        # Return tidy dataframe
        tibble(
            term = names(q_bar),
            estimate = q_bar,
            std.error = se_pooled,
            statistic = t_stat,
            p.value = p_val,
            conf.low = conf_low,
            conf.high = conf_high,
            df = df_pooled,
            outcome = dv
        )

    } else {
        # Single dataset
        lm_out <- lm_robust(as.formula(paste0(dv, " ~ discrimination + micro_aggression + age + educ6 + income + ownhome + male + forborn + democrat + republican")), 
                            data = df, 
                            weights = if(!is.null(weights)) df[[weights]] else NULL,
                            fixed_effects = ~states)
        
        tidy(lm_out, conf.int = TRUE)
    }
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
ols_interaction <- function(df, dv, ref_level = "Black", weights = NULL) {

    # Check if MI
    is_mi <- ".imp" %in% names(df) && length(unique(df$.imp)) > 1

    # Formula
    fml <- as.formula(paste0(
        dv, " ~ discrimination * race + micro_aggression * race + ",
        "age + educ6 + income + ownhome + male + forborn + democrat + republican"
    ))

    if (is_mi) {
        # MI Pooling
        imp_list <- split(df, df$.imp)
        
        fits <- lapply(imp_list, function(d) {
            # Filter and relevel within each imputation
            d_sub <- d %>%
                filter(race != "Multiracial") %>%
                mutate(race = relevel(factor(race), ref = ref_level))
            
            lm_robust(fml, 
                      data = d_sub, 
                      weights = if(!is.null(weights)) d_sub[[weights]] else NULL,
                      fixed_effects = ~states)
        })
        
        # Use mice::pool if possible, but manual pooling usually safer with robust SEs from estimatr
        # (Same manual pooling logic as above - abstracted slightly)
        
        coefs <- do.call(rbind, lapply(fits, coef))
        vars <- do.call(rbind, lapply(fits, function(x) diag(vcov(x))))
        m <- length(fits)
        q_bar <- colMeans(coefs)
        u_bar <- colMeans(vars)
        b <- apply(coefs, 2, var)
        t_var <- u_bar + (1 + 1/m) * b
        se_pooled <- sqrt(t_var)
        
        # We invoke a "fake" model object structure to allow downstream functions to use coef() and vcov()
        # This is a bit hacky but keeps compatibility with compute_marginal_effects
        
        dummy_model <- fits[[1]] # Taking structure from first model
        
        # Override coefficients and vcov
        dummy_model$coefficients <- q_bar
        dummy_model$vcov <- diag(t_var) # Approximation: assuming no covariance between terms for simplicity in pooling vcov matrix 
        # (For proper vcov pooling we need the full matrices, which is expensive but correct)
        
        # Let's do better vcov pooling for the marginal effects
        vcov_list <- lapply(fits, vcov)
        u_bar_mat <- Reduce("+", vcov_list) / m
        
        # Between covariance matrix
        q_bar_mat <- matrix(q_bar, nrow=1)
        b_mat <- matrix(0, nrow=length(q_bar), ncol=length(q_bar))
        for(i in 1:m) {
            diff <- matrix(coefs[i,] - q_bar, ncol=1)
            b_mat <- b_mat + (diff %*% t(diff))
        }
        b_mat <- b_mat / (m - 1)
        
        t_var_mat <- u_bar_mat + (1 + 1/m) * b_mat
        
        dummy_model$vcov <- t_var_mat
        dummy_model$std.error <- se_pooled
        
        return(dummy_model)
        
    } else {
        # Filter out Multiracial and relevel race
        df_sub <- df %>%
            filter(race != "Multiracial") %>%
            mutate(race = relevel(factor(race), ref = ref_level))
        
        lm_robust(fml, 
                  data = df_sub, 
                  weights = if(!is.null(weights)) df_sub[[weights]] else NULL,
                  fixed_effects = ~states)
    }
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