# Measuring Lived Racial Experience
**Systematically Measuring the Multi-dimensions of Lived Racial Experience**

- The goal of this article is to document **how I cleaned, imputed, and ran a factor analysis on a large-scale survey data** ([2016 National Asian American Post-Election Survey](http://naasurvey.com/data/)) that measured the multi-dimensions of lived racial experience. This is part of the research which I have conducted with my advisor [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) (UC Berkeley) over the past two years (2018-2020). 


## Motivation 

- This article does not intend to discuss why we view lived racial experience as a multidimensional concept and why we measure it using particular questions used in this particular survey. That discussion goes far beyond the scope of this article. 
- The main focus is **Practical**. Survey data is the bread and butter of opinion/behavioral research. However, most of the survey data is not saved in a format that is suitable for statistical analysis. Therefore, researchers and practitioners spend numerous hours cleaning up the data. Over the past few years, I have come to realize that patterns emerge when cleaning survey data, and I want to share some of my tricks for exploiting these patterns, as they can be helpful for researchers and practitioners. In the near future, I plan to develop an R package that helps with preprocessing survey data, so, please feel free to create issues on the Git repository if you have difficulties applying my code to your project.


## Workflow

In this section, I document how I preprocessed and analyzed the 2016 National Asian American Survey (NAAS) data step-by-step. I also provide the R code used in each step. The `.Rmd` extension indicates an `R markdown file`. 


### 01_Cleaning Data [[01_data_cleaning.Rmd](https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/code/01_data_cleaning.Rmd)] 

- **Tidying:** I assume that your survey data is saved in [a tidy format](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). Variables are columns and rows are observations. If this is not the case, turn your data into a tidy format. The `tidyr` package provides many useful tools to do this easily.  
- **Columns:** The first thing to do is to select the columns or variables that you want to use for the analysis. In case of the NAAS data, the original data has 406 variables; however, I did not need most of them. Most survey data, especially for large surveys, fall into this category. These surveys are expensive to collect and thus, they contain information that is intended to be useful to a large number of and diverse users. The result of this may be that there many questions that have nothing to do with your particular research. Don’t allow these variables to take up your precious memory. 
- In addition, when selecting columns, it is useful to make a comment on what they are about. As you can see below, most questions are named based on the order in which they appeared in the questionnaire. This is not especially informative, and it means that you will likely have to go back and forth between the codebook and the survey data. Save yourself these extra steps by commenting clearly on the content. 

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

- **Rows** The next step is to take a close look at rows or responses. Like column names, these rows are also often not ready for analysis. For instance, if a question is measured on a 1–5 Likert scale, then the responses would look like “1. Strongly negative,” “2. Somewhat negative,” and so on. The problem with this is that in the end, we only want the numbers from these responses to conduct a statistical analysis. Thankfully, there is an easy solution. Focus on the string pattern. From these responses, we only need numbers, especially the first group (e.g., `1` or `99`). - **Rows** The next step is to take a close look at rows or responses. Like column names, these rows are also often not ready for analysis. For instance, if a question is measured on a 1–5 Likert scale, then the responses would look like “1. Strongly negative,” “2. Somewhat negative,” and so on. The problem with this is that in the end, we only want the numbers from these responses to conduct a statistical analysis. Thankfully, there is an easy solution. Focus on the string pattern. From these responses, we only need numbers, especially the first group (e.g., `1` or `99`). You can extract them by using [regular expression](https://en.wikipedia.org/wiki/Regular_expression) or defining a search pattern. Then you can create a function that uses this search pattern to extract the first capturing number group and apply the function to every column in the data. This trick makes a huge difference if you deal with data from a big survey. Find patterns and exploit them: this is the key point I will keep repeating throughout the document.

```{R}
    # Create a function for capturing the first number group from survey responses 
    
    return_numeric <- function(x) {as.numeric(gsub("([0-9]+).*$", "\\1", x))} # regular expression
    
    recoded <- df %>%
      select_if(negate(is.character)) %>% # exclude the only character variable 
      apply(2, return_numeric) %>% # apply the function to every column in the subsetted data 
      as.data.frame() # turn it into a dataframe (otherwise, it's a matrix)
```

- While paying attention to general patterns is the first step toward efficiency, don’t lose sight of the details. There are different categories of variables—while some are categorical, others are ordinal or numeric. For example, categorical variables are better treated as factor variables in R, as statistical models are sensitive to these differences. Make sure categorical variables are treated as factors and that they remain that way.
- Once you have extracted the first capturing number group from the responses, it is easy to replace nonresponses with NAs. Find out which number stands for nonresponses, index it, and replace all cases of it with NAs.

```{R}
recoded[recoded == 9] <- NA
```

- Scaling is another important problem. A survey may have mixed scales. While some questions provide five choices, others may have only binary choices (“Yes” or “No”). These inconsistencies make measuring covariances among them less accurate and interpreting their regression coefficients difficult. You can rescale them on the identical normalized scale (0–1) using the `rescale() function` from the `scales` package.

```{R}
# Re-scale ordinal responses 

rescaled <- recoded %>%
  dplyr::select(starts_with("q")) %>% # starts with q
  apply(2, scales::rescale) %>%
  as.data.frame() 
```

- Another problem occurs when replacing responses in binary choice questions. In the data I used for this project, I found that `1` corresponds to “Yes” and `2` corresponds to “No” in binary questions. In my view, these variables are better treated as dummy variables where `1` corresponds to “Yes” and `0` corresponds to “No.” This way we can find important patterns in the data more easily. For instance, under this condition, the means of these questions reveal the percentages of agreement among the survey respondents about the survey constructs. To do this, you can use the same trick I demonstrated above. First, create a function that recodes binary responses. Second, apply the function to each variable in the data using the `apply function()`. This custom function + `the apply() function` combination is useful for reducing redundancy in data cleaning. 

```{R}
# Function to recode binary responses 

reverse_dummy <- function(data){
  data[data == 1] <- 2
  data[data == 0] <- 1
  data[data == 2] <- 0
  return(data)
} 
 
```

- When the cleaning process is done, save the file as “01_cleaned.csv” in the `processed_data` subdirectory within your project director. Separate your raw data from processed data and your code from your output. In general, input, process (computation), and output should be separated from each other. When you work with other people or need to share your project with someone else, this little structure is a great help for them in navigating the many files you have created. Don’t underestimate the number of files you will end up creating. The longer you work on the project, the more files you will produce—sort them earlier rather than later. Also, don’t forget to name files in an informative way (numbering plus for the research stage that is complete).


### 02_Imputing Data [[02_imputation.Rmd](https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/code/02_imputation.Rmd)]

- Even if a survey’s data is collected by probability sampling (a sampling method that involves random selection), it does not imply that the data is representative (=sampling estimates are unbiased estimators of population parameters). The survey might have failed to recruit as many survey respondents as was planned. If this is the case, we may need to weight the data. In addition, the respondents might have missed or not responded to all the stated questions. If that is the case, we may need to impute data. This missing data problem arises because the data collection process is not entirely under the control of the people who design and implement a survey. Missing data is bound to occur, and we should pay attention to it, especially whether the pattern of its occurrence is random or systematic. 
- Usually, big surveys provide their weights. From the user end, a more frequent problem is imputation: missing responses for questions of interest. 
- An easy solution is to use listwise deletion (`na.rm = TRUE`) or just ignore these observations. This works only if these observations are missing completely at random (MCAR). This is a strong assumption, as it happens very rarely. The `nanair package` provides many useful functions for inspecting missing data values. I used the `miss_var_summary() function` from this package to inspect the missing pattern and visualized it using ggplot2. The bar plot (Figure 1) shows that missing values are concentrated in relation to particular variables and, thus, the missing pattern is unlikely to be MCAR. In the plot, AAPI stands for Asian American Pacific Islanders. These group categories are self-identified. I also conducted a Little’s test, a global test for MCAR, which confirms that the chance for MCAR is extreme (low p-value). Therefore, listwise deletion is discouraged as an option. 

**Figure 1. Missing Pattern in the NAAS Data**
![](<https://github.com/jaeyk/measuring-racial-lived-experience/blob/master/outputs/missing_rate.png>)

- In this case, I took a multiple imputation approach. Basically, I recovered (imputed) missing values based on the observed data. I did so by making simulations of these imputed values in order to garner some measures of their uncertainty. This method was pioneered by [Donald Rubin](https://statistics.fas.harvard.edu/people/donald-b-rubin) in the 1970s, and many packages in R help to implement this method pretty easily. 
- In this project, I used the `mice package` developed by [Stef van Buuren](https://stefvanbuuren.name/).  The following code shows how the imputation model is set up. I comment on each argument to make it explicit how the model is set up. For instance, the `m argument` refers to the number of imputations, and I set it to `5`. This way I do not need to go back to the package documentation to recall what each argument is for. Also, readers can easily comprehend how I approach the imputation problem. More commenting and increased transparency create a win-win situation.

```{r}

imp <- mice(scaled,
     seed = 1234, # for reproducibility
     m = 5, # the number of imputations
     maxit = 10, # the max numbe of iterations 
     method = "pmm", # predictive mean method
     print = FALSE) 

```

- The goal of imputation is to create imputed values that are as close as possible to observed values. Figure 2 is the kernel density estimates (KDE) for the marginal distribution of the imputed (red) and the observed (blue) values. Kernel density estimation is calculated by weighting the distance of the data points. The plot shows that the distributions of the imputed and observed values are quite close, especially among items on a Likert scale. I created this and many other related KDE plots (see the R markdown file) for the diagnostic test using the `densityplot() function` from the `mice package`.

**Figure 2. The KDE for the Marginal Distribution of the Imputed (Red) and the Observed (Blue) Values**
![](<https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/outputs/imputed_density_plot.png>)

- Another important task is deciding whether selecting one imputed piece of data over another is consequential. The `complete() function` pools the imputed data, but a question remains about which one should be selected. In this project, I created five imputed datasets. I built a simple regression model that examines associations between survey items related to the everyday challenge component of racial experience. The `pool() function` from the `mice package` is Donald Rubin’s rule test. The test “averages the estimates of the complete model” and “computes the total variance over the repeated analysis” (for more information, see [this function documentation](https://rdrr.io/cran/mice/man/pool.html)). The summary test result shows that the p-values for the regression coefficients are extremely small. Selecting one model over the other makes little difference for the model fit (=little within imputation variance). Rubin’s rule test assumes that the distribution of the data follows a normal distribution. I checked this assumption using the Shapiro-Wilk test.   

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

- I extracted the first imputed dataset and saved it as "imputed.csv" in the processed_data subdirectory.


### 03_Factor Analysis [[03_factor_analysis.Rmd](https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/code/03_factor_analysis.Rmd)] 

- I selected the variables related to the multi-dimensions of lived racial experience from the survey. This time, I renamed these variable names using the `rename function()` from the `dplyr` package, as they will appear in the plots I will soon create.

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

- Taeku and I assumed that there are three dimensions of lived racial experience. We do not observe these constructs from the survey data. What we have instead is a battery of survey items that might hang together and map onto the assumed conceptual framework. We can examine this pattern by calculating the covariance between survey items of interest. Factor analysis, by definition, is one way to perform this task, because factors are latent/unobserved/low dimensions in data. 
- Let’s first check whether the assumption about the number of factors is valid. The `fa.parallel` function from the `psyche package` compares the eigenvalues of the correlation matrix ([the metric of variance explained]((https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/))) from the observed data with the eigenvalues generated from random data. In Figure 3, the Y-axis indicates eigenvalues and the X-indicates the number of possible factors from 1 to the maximum. Here, you can easily see that after three factors, the Y value drops immediately. 
- I set the `fm argument` in the `fa.parallel` function to “ml” (maximum likelihood estimation) to use the common factor model, which assumes that covariance between items is explained by both “shared latent causes” and “unexplained variable-specific variance” (for more information, see this [link](https://psu-psychology.github.io/psy-597-SEM/06_factor_models/factor_models.html)). The result (again, an abrupt change in the slope) shows that assuming three factors is plausible.  

**Figure 3. Parallel Analysis Result**
![](<https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/outputs/scree_plot.png>)

- After validating the `nfactors = 3` assumption, I ran the factor analysis using the observed data. I assume that these factors are orthogonal by setting the `rotate  = ‘oblimin’.` For interpretation, this means the factors show the correlations between question items and factors (for more information, see this [link](https://psu-psychology.github.io/psy-597-SEM/06_factor_models/factor_models.html#looking-under-the-hood-of-the-fa-model)). 

```{r}

# Factor analysis 
factor_analysis <- fa(vars, 
                  nfactors = 3, # three factors  
                  rotate = 'oblimin', 
                  fm = 'ml') # ML estimation

```

In this section, the goal is to show how I visualized the relationship between each question item and three factors. I did this in two steps. I first extracted factor loadings (correlation coefficients between observed data and factors) and then put them into a dataframe. 

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

Next, I visualized the relationship between factor loadings and the three factors (see Figure 4). 

**Figure 4. Factor Analysis Result**
![](<https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/outputs/factor_analysis.png>)

Finally, I created three index variables based on questions related to each factor and then displayed how the three dimensions of lived racial experience vary across racial groups (see Figure 5).   

**Figure 5. Three Dimensions of Lived Racial Experience Vary across Racial Groups**
![](<https://github.com/jaeyk/measuring-lived-racial-experience/blob/master/outputs/factor_analysis_by_race.png>)


## Conclusion
Let’s not try to be the smartest person in the room. I am a data science fellow and a consultant for [the Data-intensive Social Scineces Lab](https://dlab.berkeley.edu/) (D-Lab) at UC Berkeley. We have a nice motto, which I fully embrace in my life and work: IOKN2K (It's Okay Not to Know). Conducting data analysis is hard because there are so many ways it can go wrong. Factor analysis sounds simple; however, as you can see, getting there takes so many steps that involve many complex decisions. We can do this job well by documenting, reviewing, and improving workflow. We can do this even better by sharing the documentation with others and receiving and incorporating their feedback. Despite the obvious benefits, this is hard to do in practice because we do not want to be seen as being less competent than our peers. To change this mindset, we need to understand and accept that learning happens through incremental steps. Failures are bound to happen in the process. Some are systematic and others random. We should not be distracted by noises, we should avoid socially inherited biases, and we should strive to learn from our own systematic mistakes. Documentation makes this process easier by making the assumptions we made in each step as clear as possible. Experience on its own does not provide us with expertise, but experience paired with reflection does. Please let me know if you spot mistakes in this Git repository or if you have suggestions to make the code more elegant. As I noted earlier, I am working on an R package for preprocessing survey data, so any feedback would be greatly appreciated.
