# Measuring Racial Lived Experience
**Systematically Measuring the Multi-dimensions of Racial Lived Experience**

- The goal of this article is to document **how I cleaned, imputed, and ran a factor analysis on a large-scale survey data** (2016 National Asian American Survey) to measure the multi-dimensions of racial lived experience. This is part of the research which I have conducted with my advisor [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) (UC Berkeley) for the past two years (2018-2020). 


## Motivation 

- My focus is **Not Substantial**. I do not intend to discuss why we view racial lived experience as a multidimensional concept and why we measure it using particular questions used in this particular survey. That discussion goes far beyond the scope of this document and will be included in the paper based on this project. 
- The main focus is **Practical**. Survey data is the bread and butter of opinion/behavioral research. However, most of the survey data is not saved in a format that is suitable for statistical analysis. Therefore, researchers and practitioners spend numerous hours to clean the data. Over the past few years, I realized that cleaning survey data has patterns. I want to share some of my tricks to exploit these patterns as they can be helpful for researchers and practitioners. In the near future, I plan to develop an R package that helps preprocessing survey data. For that reason, please feel free to create issues on the Git repository when you have difficulties applying my code to your project.


## Workflow

In this section, I document how I preprocessed and analyzed the 2016 National Asian American Survey (NAAS) data step-by-step. I also provide the R code used in each step. `.Rmd` extension indicates an `R markdown file`. 


### 01_Cleaning Data [[01_data_cleaning.Rmd](https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/code/01_data_cleaning.Rmd)] 

- **Tidying:** I assume that your survey data is saved in [a tidy format](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). The NAAS is. Variables are columns and rows are observations. If not, turn your data into a tidy format. The `tidyr` package provides many useful tools to do this easily.  
- **Columns:** The first thing to do is to select columns or variables that you want to use for the analysis. In case of the NASS data, the original data has 406 variables. Yet, I did not need most of them. Most of the survey data, specially large ones, fall into this category. These survey are expensive to collect and, thus, try to be useful to a large number of and diverse users. The result of lots of questions that may have nothing to do with your particular research. Don't allow these variables take up your precious memory. 
- In addition, when selecting columns, it is useful to comment what they are about. As you can see below, most questions are named in a survey, following the order in which they appeared in the questionnaire. They are not informative. You are likely to go back and forth between the codebook and the survey data. Save these extra steps by commenting. 

```R
df <- data %>%
	dplyr::select(

    ## Covariates

    race, # race 
    pid4, # party ID
    q10_2b, # education level 
    q10_15, # income level
    q10_18, # age
    s9a, # states 
    forborn, # foreign born 
    citizen, # citizen 
```

- **Rows** The next step is taking a close look at rows or responses. Like column names, these rows are also often not ready for the analysis. For instance, if a question is measured on a 1-5 Likert scale, then the responses would look like "1. Strongly negative", "2. Somewhat negative", and so on. The problem with this is, in the end, we want only numbers from these responses to conduct a statistical analysis. Thankfully, there is an easy solution. Focus on the string pattern. From these responses, we only need numbers, especially the only first group (e.g., `1` or `99`). We can extract them using [regular expression](https://en.wikipedia.org/wiki/Regular_expression) or defining a search pattern. Then, you can create a function that uses this search pattern to extract the first capturing number group and apply the function to every column in the data. This trick makes a huge difference, if you deal with a big survey data. Find patterns and exploit them. This is the key point I will keep repeating throughout the document.
- While paying attention to general patterns is the first step toward efficiency, don't lose the sight on details. There are different categories of variables. While some are categorical, others are ordinal or numeric. For example, categorical variables are better treated as factor variables in R as statistical models are sensitive to these differences. Make sure categorical variables are treated as factors and remain that way.

```{R}
    # Create a function for capturing the first numeric elemetn of survey responses 
    
    return_numeric <- function(x) {as.numeric(gsub("([0-9]+).*$", "\\1", x))} # regular expression
    
    recoded <- df %>%
      select_if(negate(is.character)) %>% # exclude the only character variable 
      apply(2, return_numeric) %>% # apply the function to every column in the subsetted data 
      as.data.frame() # turn it into a dataframe (otherwise, it's a matrix)
```

- Once you have extracted the first capturing number group from responses, it is easy to replace nonresponses with NAs. Find what number stands for nonresponses, index them, and replace them with NAs.

```{R}
recoded[recoded == 9] <- NA
```

- Scaling is another important problem. A survey may have mixed scales. While some questions provide five multiple choices, others have only binary choices ("Yes" or "No"). These inconsistencies make measuring covariances among them less accurate and interpreting their regression coefficients difficult. You can rescale them on the identical normalized scale (0-1) using the `rescale() function` from the `scales` package.

```{R}
# Re-scale ordinal responses 

rescaled <- recoded %>%
  dplyr::select(starts_with("q")) %>% # starts with q
  apply(2, scales::rescale) %>%
  as.data.frame() 
```

- The other problem is replacing responses in binary choice questions. In the data I used for this project, I found that `1` corresponds to "Yes" and `2` corresponds to "No" in binary questions. In my view, these variables are better treated as dummy variables where `1` corresponds to "Yes" and `0` corresponds to "No". That way we can find some important patterns of the data easily. For instance, under this condition, the means of these questions reveal the percentages of the agreement among the survey respondents about the survey constructs. To do so, you can use the same trick I demonstrated above. First, create a function that recode binary responses. Second, apply the function to each variable in the data using the `apply function()`. This custom function + `the apply() function` combination is useful to reduce redundancy in data cleaning. 

```{R}
# Function to recode binary responses 

reverse_dummy <- function(data){
  data[data == 1] <- 2
  data[data == 0] <- 1
  data[data == 2] <- 0
  return(data)
} 
 
```

- When the cleaning process is done, save the file as "01_cleaned.csv" in the `processed_data` subdirectory within your project director. Separate your raw data from processed data and your code from your output. In general, input, process (computation), and output should be separated from each other. When you work with other people or need to share your project with someone else, this little structure helps them a lot to navigate many files you created. Don't underestimate the number of files you end up creating. The longer you work on the project, the more files you will produce. Sort them earlier rather than later. Also, don't forget naming files in an informative way (numbering plus for the research stage that is complete).


### 02_Imputing Data [[02_imputation.Rmd](https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/code/02_imputation.Rmd)]

- Even if a survey data is collected by probability sampling (a sampling method that involves random selection), it does not imply that the data is representative (=sampling estimates are unbiased estimators of population parameters). The survey might have failed to recruit survey respondents as they planned. Then, we we may need to weight data. Also, these respondents might have missed or not responded all stated questions. Then, we may need impute data. This missing data problem arises because data collection process is not entirely under the control of the people who designed and implemented a survey. Missing data is bound to occur and we should pay attention to them, especially whether the pattern of their occurrence is random or systematic. 
- Usually, big surveys provide their weights. From the user end, a more frequent problem is imputation: missing responses for questions of interest. 
- An easy solution is to use listwise deletion (`na.rm = TRUE`) or just ignoring these observations. This works only if these observations are missing completely at random (MCAR). This is a strong assumption as it happens very rarely. The `nanair package` provides many useful functions to inspect missing values in data. I used the `miss_var_summary() function` from this package to inspect the missing pattern and visualize it using ggplot2. The bar plot (Figure 1) shows that missing values are concentrated in particular variables and, thus, the missing pattern is unlikley to be MCAR. In the plot, AAPI stands for Asian American Pacific Islanders. These group categories are self-identified. I also conducted Little's test, a global test for MCAR and it confirms that the chance for MCAR is extreme (low p-value). Therefore, listwise deletion is discouraged as an option. 

**Figure 1. Missing Pattern in the NAAS Data**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/missing_rate.png>)

- I took multiple imputation approach. Basically, I recover (impute) missing values based on the observed data. I do so by making simulations of these imputed values to have some measures of their uncertainty. This method is pioneered by [Donald Rubin](https://statistics.fas.harvard.edu/people/donald-b-rubin) in the 1970s and many packages in R helps to implement this method pretty easily. 
- In this project, I used the `mice package` developed by [Stef van Buuren](https://stefvanbuuren.name/).  The following code shows how the imputation model is set up. I comment on each argument to make how the model is set up explicit. For instance, the `m argument` is for the number of imputations and I set it to `5`. That way I do not need to go back to the package documentation to recall what each argument is for. Also, readers can easily comprehend how I approach the imputation problem. More commenting and increasing transparency creates a win-win situation.

```{r}

imp <- mice(scaled,
     seed = 1234, # for reproducibility
     m = 5, # the number of imputations
     maxit = 10, # the max numbe of iterations 
     method = "pmm", # predictive mean method
     print = FALSE) 

```

- The goal of imputation is to create imputed values that are as close as possible to observed values. Figure 2 is the kernel density estimates (KDE) for the marginal distribution of the imputed (red) and the observed (blue) values. Kernel density estimation is calculated by weighting the distance of the data points. The plot shows that the distributions of the imputed and observed values are quite close especially among items on a Likert scale. I created this and many other related KDE plots (see the R markdown file) for the diagnostic test using the `densityplot() function` from the `mice package`. 

**Figure 2. The KDE for the Marginal Distribution of the Imputed (Red) and the Observed (Blue) Values**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/imputed_density_plot.png>)

- Another important task is to decide whether selecting one imputed data over the other is consequential. The `complete() function` pools the imputed data, but a question remains about which one we select. In this project, I created five imputed data. I built a simple regression model that examines associations between survey items related to the everday challenge component of racial experience. The `pool() function` from the `mice package` is Donald Rubin's rule test. The test "averages the estimates of the complete model" and "computes the total variance over the repeated analysis" (for more information, see [this function documentation](https://rdrr.io/cran/mice/man/pool.html)). The summary test result shows that the p-values for the regression coefficients are extremely small. Selecting one model over the other does make little difference for the model fit (=little within imputation variance). Rubin's rule test assumes that the distribution of the data follows a normal distribution. I checked this assumption using Shapiro-Wilk test.   

```
         	estimate   std.error  statistic        df      p.value
(Intercept) 0.06683443 0.01040490 6.4233596 23.693785 1.290565e-06
q5_7_b      0.01727157 0.04561810 0.3786122  4.819998 7.210765e-01
q5_7_c      0.06984457 0.03957057 1.7650637  5.090141 1.367832e-01
q5_7_d      0.03384945 0.05852687 0.5783575  4.638490 5.899585e-01
q5_7_e      0.08132900 0.02809624 2.8946578  6.933637 2.340577e-02
q5_7_f      0.02307511 0.02182523 1.0572676 17.021885 3.051641e-01
q5_7_g      0.01862781 0.05967398 0.3121596  4.506344 7.688390e-01
q5_7_h      0.04646602 0.05313962 0.8744137  4.707158 4.242517e-01
q5_7_i      0.08295875 0.04721147 1.7571734  4.926265 1.401059e-01
q5_7_j      0.11028588 0.06275444 1.7574197  4.409681 1.469854e-01
q5_7_k      0.20984365 0.05250121 3.9969296  4.667219 1.189750e-02
q5_7_l      0.02228526 0.04290861 0.5193656  5.154995 6.250292e-01
```

- I extracted the first imputed data and saved it as "imputed.csv" in the processed_data subdirectory.


### 03_Factor Analysis [[03_factor_analysis.Rmd](https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/code/03_factor_analysis.Rmd)] 

- I selected the variables related to the multi-dimensions of racial lived experience from the survey. This time, I renamed these variable names using the `rename function()` from the `dplyr` package as they will appear in the plots I will create soon.

```{R}
# Renaming these variables 

vars <- vars %>%
  rename(# micro-agression
         service_unfriendly = q5_1_b,
         english_proficiency = q5_1_c,
         afraid_of_you = q5_1_d,
         thought_dishonest = q5_1_e,
         insulted = q5_1_g,
         threatened = q5_1_h,
         name_mispronounced = q5_1_i,
```

- Taeku and I assumed that there are three dimensions of racial lived experience. We do not observe these constructs from the survey data. What we have is instead a battery of survey items that might hang together and map into the assumed conceptual framework. We can examine this pattern by calculating the covariance between survey items of interest. Factor analysis, by definition, is one way to perform this task because factors are latent/unobserved/low dimensions in data. 
- Let's first check whether the assumption about the number of factors is valid. The `fa.parallel` function from the `psyche package` compares the eigenvalues of the correlation matrix ([the metric of variance explained]((https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/))) from the observed data with the eigenvalues generated from random data. In Figure 3, the Y-axis indicates eignevalues and the X-indicates the number of possible factors from 1 to the maximum. Here, you can easily see that after three factors, the Y value drops immediately. 
- I set the `fm argument` in the `fa.parallel` function to "ml" (maximum likelihood estimation) to use common factor model, which assumes that covariance between items are explained by both "shared latent causes" as well as "unexplained variable-specific variance" (for more inforation, see this [link](https://psu-psychology.github.io/psy-597-SEM/06_factor_models/factor_models.html)). The result (again, an abrupt change in the slope) shows that assuming three factors is plausible.  

**Figure 3. Parallel Analysis Result**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/scree_plot.png>)

```{R}

# Factor analysis 
factor_analysis <- fa(vars, 
                  nfactors = 3, # three factors  
                  rotate = 'varimax', 
                  fm = 'ml') # ML estimation 

```

After validating the `nfactors = 3` assumption, I ran the factor analysis using the observed data. I assume that these factors are orthogonal by setting the `rotate  = 'oblimin'.`For interpretation, it means the factors show the correlations between question items and factors (for more information, see this [link](https://psu-psychology.github.io/psy-597-SEM/06_factor_models/factor_models.html#looking-under-the-hood-of-the-fa-model)). 

```{r}

# Factor analysis 
factor_analysis <- fa(vars, 
                  nfactors = 3, # three factors  
                  rotate = 'varimax', 
                  fm = 'ml') # ML estimation

```

**Figure 4. Factor Analysis Result**

In this section, the goal is to show how I visualized the relationship between each question item and three factors. I did in two steps. I first extracted factor loadings ([correlation coefficients between observed variables and latent common factors](https://methods.sagepub.com/reference/encyc-of-research-design/n149.xml)) and then put them into a dataframe. 

```{R}

# Extract factor loadings 
factor_frame <- factor_analysis$loadings %>%
                 unclass() %>%
                 as.data.frame()

# Putting them into a data frame
factor_df <- data.frame(Measures = rownames(factor_frame), 
              everyday_challenge = factor_frame$ML1,
              micro_aggression = factor_frame$ML2,
              discrimination = factor_frame$ML3)

```

Next, I visualized the relationship between factor loadings and three factors (see Figure 4). 

![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/factor_analysis.png>)

Finally, I created three index variables based on questions related to each factor and then displayed how three dimensions of racial lived experience vary across racial groups (see Figure 5).   

**Figure 5. Three Dimensions of Racial Lived Experience Vary across Racial Groups**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/factor_analysis_by_race.png>)


## Conclusions
1. Let's not try to be the smartest person in the room. I am a data science fellow and a consultant for [the Data-intensive Social Scineces Lab](https://dlab.berkeley.edu/) (D-Lab) at UC Berkeley. We have a nice motto, which I fully embrace in my life and work: IOKN2K (It's Okay Not to Know). Doing a data analysis is hard because there are so many ways it can go wrong. Factor analyis sounds simple. However, as you can see, getting there takes so many steps that involve many complex decisions. We can do this job well by documenting, reviewing, and improving workflow. We can do this even better by sharing this documentation with others and receiving and incorporating their feedback. Despite obvious benefits, this is hard to do in practice because we do not want to be seen as less competent than our peers. To change the mindset, we need to understand that learning happens through incremental steps. Failures are bound to happen in the process. Some are systematic and others random. We should not be distracted by noises, avoid socially inherited biases, and strive to learn from our own systematic mistakes. Documentation makes this process easier by making the assumptions we made in each step as clear as possible. Experience does not provide us expertise, but experience with reflection does. Please let me know if you spot mistakes in this Git repository or you have suggestions to make the code more elegant. As I alluded, I am working on a R package for preprocessing survey data, so any feedback would be greatly appreciated.
2. In addition, for R statistical packges, I think that it would be nice, if they have some printed messages for changing model parameters. In particular, it would be useful, if these pacakges could inform users regarding the limitations of default setups and the correct ways to interpret model outcomes. Most R statistical packages assume that users have strong knowledge of the functions they are using and the documentation they provided is sufficient. I believe that in most cases this assumption is unwarranted. As R gains more popularity, R users become more diverse in technical backgrounds. Therefore, nudging users to avoid overconfidence could make a meaningful difference in the ways these package are applied in data analysis.