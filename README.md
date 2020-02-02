# Measuring Racial Lived Experience
**Systematically Measuring the Multi-dimensions of Racial Lived Experience**



- The goal of this article is to document **how I cleaned, imputed, and ran a factor analysis on a large-scale survey data** (2016 National Asian American Survey) to measure the multi-dimensions of racial lived experience. This is part of the research which I have conducted with my advisor [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) (UC Berkeley) for the past two years (2018-2020). 



## Motivation 

- My focus is **Not Substantial**. I do not intend to discuss why we view racial lived experience as a multidimensional concept and why we measure it using particular questions used in this particular survey. That discussion goes far beyond the scope of this document and will be included in the paper based on this project. 
- The main focus is **Practical**. Survey data is the bread and butter of opinion/behavioral research. However, most of the survey data is not saved in a format that is suitable for statistical analysis. Therefore, researchers and practitioners spend numerous hours to clean the data. Over the past few years, I realized that cleaning survey data has patterns. I want to share some of my tricks to exploit these patterns as they can be helpful for researchers and practitioners. In the near future, I plan to develop an R package that helps preprocessing survey data. For that reason, please feel free to create issues on the Git repository when you have difficulties applying my code to your project.


## Workflow

In this section, I document how I preprocessed and analyzed the 2016 National Asian American Survey (NAAS) data step-by-step. I also provide the R code used in each step. Please note that `.Rmd` extension indicates an `R markdown file`. 


### 01_Cleaning Data [[01_data_cleaning.Rmd](https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/code/01_data_cleaning.Rmd)] 

- **Tidying:** Please note that I assume that your survey data is saved in [a tidy format](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). The NAAS is. Variables are columns and rows are observations. If not, turn your data into a tidy format. The `tidyr` package provides many useful tools to do this easily.  
- **Columns:** The first thing to do is to select columns or variables that you want to use for the analysis. In case of the NASS data, the original data has 406 variables. Yet, I did not need most of them. Most of the survey data, specially large ones, fall into this category. These survey are expensive to collect and, thus, try to be useful to a large number of and diverse users. The result of lots of questions that may have nothing to do with your particular research. Don't allow these variables take up your precious memory. 
- In addition, when selecting columns, it is useful to comment what they are about. As you can see below, most questions are named in a survey, following the order in which they appeard in the questionnaire. They are not informative. You are likely to go back and forth between the codebook and the survey data. Save these extra steps by commenting. 

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

- **Rows** The next step is taking a close look at rows or responses. Like column names, these rows are also often not ready for the analysis. For instance, if a question is measured on a 1-5 Likert scale, then the responses would look like "1. Strongly negative", "2. Somewhat negative", and so on. The problem with this is, in the end, we want only numbers from these responses to conduct a statistical analysis. Thankfully, there is an easy solution. Focus on the string pattern. From these responses, we only need numbers, especially the only first group (e.g., `1` or `99`). We can extract them using [regular expression](https://en.wikipedia.org/wiki/Regular_expression) or defining a search pattern. Then, you can create a function that uses this search pattern to extract the first capturing number group and apply the function to every column in the data. This trick makes a huge difference, if you deal with a big survey data. Find patterns and exploit them. This is the key point I will keep repeating throuhgout the document.
- While paying attention to general patterns is the first step toward efficiency, don't lose the sight on details. There are different categories of variabels. While some are categorical, others are ordinal or numeric. For example, categorical variables are better treated as factor variables in R as statistical models are sensitive to these differences. Make sure categorical variables are treated as factors and remain that way.

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

- The other problem is replacing responses in binary choice questions. In the data I used for this project, I found that `1` corresponds to "Yes" and `2` corresponds to "No" in binary questions. In my view, these variables are better treated as dummy variables where `1` corresponds to "Yes" and `0` corresponds to "No". That way we can find some important patterns of the data easily. For instance, under this condition, the means of these questions reveal the percentages of the agreement among the survey respondents about the survey constructs. To do so, you can use the same trick I demonstrated above. First, create a function that recoe binary responses. Second, apply the function to each variable in the data using the `apply function()`. This custom function + `the apply() function` combination is useful to reduce redundancy in data cleaning. 

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

- Even if a survey data is collected by probability sampling (a sampling method that involves random selection), it does not imply that the data is representative (=sampling estimates are unbiased estimators of population parameters). The survey might have failed to recruit survey respodnents as they planned. Then, we we may need to weight data. Also, these respondents might have missed or not responded all stated questions. Then, we may need imputate data. This missing data problem arises because data collection process is not entirely under the control of the people who designed and implemented a survey. Missing data is bound to occur and we should pay attention to them, especially whether the pattern of their occurance is random or systematic. 
- Usuaully, big surveys provide their weights. From the user end, a more frequent problem is imputation: missing responses for questions of interest. 
- An easy solution is to use listwise delection (`na.rm = TRUE`) or just ignoring these observations. This works only if these observations are missing completely at random (MCAR). This is a strong assuimption as it happens very rarely. The `nanair package` provides many useful functions to inspect missing values in data. I used the `miss_var_summary() function` from this pacakge to inspect the missing pattern and visualize it using ggplot2. The bar plot (Figure 1) shows that missing values are concentrated in particular variables and, thus, the missing pattern is unlikley to be MCAR. Please note that in the plot AAPI stands for Asian American Pacific Islanders. These group categories are self-identified. I also conducted Little's test, a global test for MCAR and it confirms that the chance for MCAR is extreme (low p-value). Therefore, listwise delection is discouraged as an option. 

**Figure 1. Missing Pattern in the NAAS Data**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/missing_rate.png>)

- I took multiple imputation approach. Basically, I recover (impute) missing values based on the observed data. I do so by making simulations of these imputed values to have some measures of their uncertainty. This method is pioneered by [Donald Rubin](https://statistics.fas.harvard.edu/people/donald-b-rubin) in the 1970s and many packages in R helps to implement this method pretty easily. 
- In this project, I used the `mice package` developed by [Stef van Buuren](https://stefvanbuuren.name/).  The following code shows how the imputation model is set up. Note that I comment on each argument to make how the model is set up explicit. For instance, the `m argument` is for the number of imputations and I set it to `5`. That way I do not need to go back to the package documentation to recall what each argument is for. Also, readers can easily comprehend how I approach the imputation problem. More commenting and incresaing transparency creates a win-win situation.

```{r}

imp <- mice(scaled,
     seed = 1234, # for reproducibility
     m = 5, # the number of imputations
     maxit = 10, # the max numbe of iterations 
     method = "pmm", # predictive mean method
     print = FALSE) 

```
 
- The goal of imputation is to create imputed values that are as close as possible to observed values. Figure 2 is the kernel density estimates (KDE) for the marginal distribution of the imputed (red) and the observed (blue) values. Please note that kernel density estimation is calculated by weighting the distance of the data points. The plot shows that the distributions of the imputed and observed values are quite close especially among itmes on a likert scale. I created this and many other related KDE plots (see the R markdown file) for the diagonostic test using the `densityplot() function` from the `mice package`. 

**Figure 2. The KDE for the Marginal Distribution of the Imputed (Red) and the Observed (Blue) Values**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/imputed_density_plot.png>)

- Another important task is to decide whether selecting one imputed data over the other is consequential. The `complete() function` pools the imputed data, but a question remains about which one we select. In this project, I created five imputed data. I built a simple regression model that examines associations between survey items related to the everday challenge component of racial experience. The `pool() function` from the `mice package` is Donald Rubin's rule test. The test "averages the estimates of the complete model" and "computes the total variance over the repeated analyse" (for more information, see [this function documentation](https://rdrr.io/cran/mice/man/pool.html)). The summary test result shows that the p-values for the regression coefficients are extremely small. Selecting one model over the other does make little difference for the model fit (=little within imputation variance). Rubin's rule test assumes that the distribution of the data follows a normal distribution. I checked this assumption using Shapiro-Wilk test.   

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

- Taeku and I assumed that there are three dimensions of racial lived experience. We do not observe these constructs from the observations. What we have is a battery of survey questions. What we argue is, essentially, these survey questions have three latent (or unobserved) dimensions, which can be examined by how these survey questions hang together. Factor analysis, by definition, is one way to accomplish this task. Factors are latent dimensions in the data. 
- Let's first check whether the assumption about the number of factors is valid. 

**Figure 3. Parallel Analysis Result**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/scree_plot.png>)

**Figure 4. Factor Analysis Result**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/factor_analysis.png>)

**Figure 5. Three Dimensions of Racial Lived Experience and How They Vary across Racial Groups**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/factor_analysis_by_race.png>)

## Conclusions