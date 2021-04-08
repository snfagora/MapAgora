## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

idx <- import_idx(2019)
usethis::use_data(idx, internal = TRUE) # Stores all data in a single object
