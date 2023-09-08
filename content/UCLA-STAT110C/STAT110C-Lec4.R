library(modeldata)
data(ames)
dim(ames)

library(tidymodels)
tidymodels_prefer()
set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

#################################################

lm_model <- linear_reg() %>% set_engine("lm")
lm_form_fit <- lm_model %>% fit(Sale_Price~Longitude+Latitude, data=ames_train)

x <- ames_train %>% select(Longitude, Latitude)
y <- ames_train %>% pull(Sale_Price) # pull is like select but for one column

lm_xy_fit <- lm_model %>% fit_xy(x=x, y=y)

# use extract_fit_engine() to use base R's functions
lm_form_fit %>% extract_fit_engine() %>% summary() %>% coef()
# better method: tidy()
lm_form_fit %>% tidy()

ames_test_small <- ames_test %>% slice(1:5) # just the first 5 values of the test data
predict(lm_form_fit, new_data = ames_test_small)

# combine predictions with other tibbles: use bind_cols()
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small, type="pred_int"))

#################################################

# install.packages(c("shiny", "miniUI", "rstudioapi"))

parsnip_addin()

#################################################

# a parsnip model object
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

# a workflow object
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow <- lm_wflow %>% add_formula(Sale_Price~Longitude+Latitude)

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test_small)

lm_fit %>% update_formula(Sale_Price~Longitude)

#################################################

# other ways to add variables

lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcomes = Sale_Price,
                predictors = c(Longitude, Latitude))
lm_wflow

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>%  # remove the current named predictor variables
  add_variables(outcomes = Sale_Price,
                predictors = c(ends_with("tude"))) # ends up the same: longitude, latitude
lm_wflow

fit(lm_wflow, ames_train)

#################################################

# use last_fit()

final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

#################################################

# base R

lm.fit.base <- lm(Sale_Price~Neighborhood+log10(Gr_Liv_Area)+Year_Built+Bldg_Type, data=ames_train)
summary(lm.fit.base)

# recipe steps

simple_ames <- 
  recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type, data=ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

lm_wflow <- 
  lm_wflow %>% remove_variables() %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test_small)


#################################################


