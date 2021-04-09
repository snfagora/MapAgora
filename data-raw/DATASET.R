## code to prepare `DATASET` dataset goes here

idx <- import_idx(2019)
idx <- subset(idx, EIN == "061553389")

usethis::use_data(idx, internal = TRUE) # Stores all data in a single object
