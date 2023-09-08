advertising <- read.csv("advertising.csv")
advertising <- advertising[,-1]
model <- lm(sales ~ TV, data = advertising)
summary(model)

# using tidymodels
library(tidymodels)
tidymodels_prefer()
lm_model <- linear_reg() %>% set_engine("lm") # specify the model and the model engine
lm_form_fit <- lm_model %>% fit(sales~TV, data=advertising) # fit the specified model to the data
lm_form_fit %>% extract_fit_engine() %>% summary() # extract the desired information of the fitted model

############################################

x <- c(1,2,3,4)
y <- c(2,6,4,8)
cor(x,y)
mod.fit <- lm(y~x)

TSS <- sum((y-mean(y))^2) # Total Sum of Squared
RSS <- sum((y-mod.fit$fitted.values)^2) # Residual Sum of Squared
R.squared <- (TSS-RSS)/TSS

############################################

lm_model <- linear_reg() %>% set_engine("lm")
lm_tv_fit <- lm_model %>% fit(sales~TV, data = advertising)
lm_radio_fit <- lm_model %>% fit(sales~radio, data = advertising)
lm_news_fit <- lm_model %>% fit(sales~newspaper, data = advertising)

lm_tv_fit %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)
lm_radio_fit %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)
lm_news_fit %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)

lm_combined_fit <- lm_model %>% fit(sales~TV+radio+newspaper, data = advertising)
lm_combined_fit %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)
# Why some predictors become insignificant in multiple linear regression? Ans: correlation exist between x-variables

############################################

# use model workflow
lm_model <- linear_reg() %>% set_engine("lm") # create a model object
lm_wflow <- workflow() %>% add_model(lm_model) # create a workflow object and add the model to the workflow

# create a series of formula in a list and then have the workflow fit
formula_list <- list(
  TV= sales~ TV,
  radio=sales~radio,
  news= sales~newspaper
)

advertising_models <- workflow_set(preproc = formula_list,
                                   models = list(lm=lm_model))
print(advertising_models) # what's in the column 'info'?
advertising_models$info[[1]]  # what's in the column 'workflow'?
advertising_models$info[[1]]$workflow[[1]] # a workflow object

# the map function will fit the advertising data to each workflow object
ad_models_results <- advertising_models %>% mutate(fit= map(info, ~ fit(.x$workflow[[1]], advertising)))

ad_models_results$fit[[1]] %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)
ad_models_results$fit[[2]] %>% extract_fit_engine() %>% summary() %>% coef() %>% round(4)


############################################
