
# Neural Network for Cytotoxicity Regression ------------------------------
# Test Set Selection ------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)


# Dataset -----------------------------------------------------------------

file_name <- "cytotox-2896.csv"

nanoparticle_df <- read_csv(file_name, skip = 1,
                            col_names = c(
                              "nanoparticle",
                              "type_OI",
                              "coat",
                              "diameter",
                              "concentration",
                              "zeta_potential",
                              "cells",
                              "cell_line_or_primary_cells",
                              "human_or_animal_cells",
                              "animal",
                              "cell_morphology",
                              "cell_age",
                              "cell_organ_or_tissue_source",
                              "exposure_time",
                              "test",
                              "test_indicator",
                              "biochemical_metric",
                              "cell_viability",
                              "interference_checked",
                              "colloidal_stability_checked",
                              "positive_control",
                              "publication_year",
                              "particle_id",
                              "reference_doi"
                            ),
                            col_types = "fffnnnfffffffifffnfffiic",
                            show_col_types = TRUE
)

nanoparticle_df <- nanoparticle_df %>% select(!c(nanoparticle,
                                                 coat,
                                                 zeta_potential,
                                                 animal,
                                                 publication_year,
                                                 particle_id,
                                                 reference_doi))


# Dataset Transformation --------------------------------------------------

nanoparticle_dataset <- lapply(nanoparticle_df, function(x)
{
  if(is.factor(x))
  {
    to_categorical(as.numeric(x)-1)
  }
  else
  {
    scale(x)
  }
})

nanoparticle_dataset$cell_viability <- nanoparticle_df$cell_viability


# Response Variable -------------------------------------------------------

nn_response <- nanoparticle_dataset$cell_viability


# Predictor Variables -----------------------------------------------------

nn_predictors <- nanoparticle_dataset
nn_predictors$cell_viability <- NULL

predictor_length <- 0

for(i in 1:length(nn_predictors)) {
  
  if(is.null(dim(nn_predictors[[i]])))
  {
    predictor_length = predictor_length +1
  }
  else
  {
    predictor_length = predictor_length +dim(nn_predictors[[i]])[2]
  }
}

nn_predictors <- matrix(unlist(nn_predictors), ncol=predictor_length)


# Dataset Split -----------------------------------------------------------

observation_num <- nrow(nn_predictors)
ntest <- trunc(observation_num/4)
testid <- sample(1:observation_num, ntest)

nn_predictors_train_val <- nn_predictors[-testid,]
nn_predictors_test <- nn_predictors[testid,]

nn_response_train_val <- nn_response[-testid]
nn_response_test <- nn_response[testid]


# Four-fold Validation Split ----------------------------------------------

train_num <- nrow(nn_predictors_train_val)

k <- 4
indices <- sample(1:nrow(nn_predictors_train_val))
folds <- cut(indices, breaks = k, labels = FALSE)

save(nn_predictors_train_val,
     nn_response_train_val,
     nn_predictors_test,
     nn_response_test,
     predictor_length,
     k, indices, folds,
     file = "nn-dt1-split-data.RData")
