library(modeldata)
data(ames)
dim(ames)

library(tidymodels)
# the following command tells R to use the function in tidymodels if there is
tidymodels_prefer()
ggplot(ames, aes(x=Sale_Price))+
  geom_histogram(bins=50, col="white")+theme_bw()

# histogram of sale price after log transformation
ggplot(ames, aes(x=Sale_Price)) +
  geom_histogram(bins=50, col="white") +
  scale_x_log10() + theme_bw()

ames <- ames %>% mutate(Sale_Price= log10(Sale_Price))

################################################

# splitting data with tidymodels
# random sampling
# set seed for reproducibility
set.seed(501)
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)
dim(ames_train) # 2344 rows and 74 columns

# Stratified random sampling
set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
dim(ames_train) # 2342 rows and 74 columns

# it may be preferable to use the most recent observations as test data (when it comes to time series data)
ames_split <- initial_time_split(ames, prop = 0.80)
# the first 80% of rows to be the trainging set and the last 20% will be testing set

################################################

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v=10) # 10 folds CV
ames_folds 
# Recall the training data has 2342 rows, and the full ames data has 2930 rows
ames_folds$splits
ames_folds$splits[[1]] %>% analysis() # the training part
ames_folds$splits[[1]] %>% assessment() # the validation part


# Repeated CV
rep_ames_folds <- vfold_cv(ames_train, v=10, repeats = 5)
rep_ames_folds

################################################

