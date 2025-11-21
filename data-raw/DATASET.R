## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

library(tidyverse)

dataset_momo <- 
  readxl::read_excel("D:/database/data_wood_density_momo_et_al/Data_WD_centralAfrica_01-2020_idtax.xlsx")


tax_momo <- match_tax(idtax = unique(dataset_momo$ID.dico.name))

dataset_momo <- 
  dataset_momo %>% 
  left_join(tax_momo %>% select(idtax_n, tax_sp_level, tax_gen, tax_fam, starts_with("taxa_level"),
                                wood_density_mean),
            by = c("ID.dico.name" = "idtax_n")) %>% 
  rename(WDgwd = wood_density_mean)

model_wd_1 <- lm(VWWD ~ WDsb, data = dataset_momo)
summary(model_wd_1)
model_wd_1$coefficients
summary(model_wd_1)$r.squared

usethis::use_data(model_wd_1)

# new_data <- 
#   data.frame(WDsb = c(0.8, 0.65, 0.7, 0.3, 0.35, 0.1))

# predicted_new_mpg <- 
#   predict(model_1, newdata = new_data)
# 
# plot(dataset_momo$VWWD, dataset_momo$WDsb)
# abline(a = 0, b = 1)


# plot(dataset_momo$VWWD, dataset_momo$WDgwd)
# abline(a = 0, b = 1)

model_wd_2 <- lm(VWWD ~ WDgwd, data = dataset_momo)
summary(model_wd_2)
model_wd_2$coefficients
summary(model_wd_2)$r.squared

usethis::use_data(model_wd_2)


phylo_tree <- 
  ape::read.tree("D:/database/phylogeny_janssens/MyTree_uptaded.tre")
usethis::use_data(phylo_tree)

# saveRDS(phylo_tree, file="./data/phylo_tree.rds")
# 
# save(phylo_tree, file="data/phylo_tree.RData")
