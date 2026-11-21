# ---- 1. Load farm data ----
paddy_clean <- read.csv("cleaned_paddy_data.csv", stringsAsFactors = FALSE)
paddy_clean$yield_kg_ha <- paddy_clean$paddy_yield_in_kg / paddy_clean$hectares
paddy_clean$agriblock <- trimws(paddy_clean$agriblock)
paddy_clean$variety   <- trimws(paddy_clean$variety)

# ---- 2. Build the reference table (edit the numbers per TNAU) ----
by_variety <- data.frame(
  variety        = c("CO_43", "delux ponni", "ponmani"),
  rec_seed_kg_ha = c(30, 75, 40),
  rec_N_kg_ha    = c(150, 150, 150),
  rec_P2O5_kg_ha = c(50, 50, 50),
  rec_K2O_kg_ha  = c(50, 50, 50)
)
blocks <- data.frame(agriblock = c("Chinnasalem", "Cuddalore", "Kallakurichi",
                                   "Kurinjipadi", "Panruti", "Sankarapuram"))

paddy_reference <- merge(blocks, by_variety, by = NULL)   # 6 x 3 = 18 rows
paddy_reference$source <- "TNAU Agritech Portal, Rice nutrient management"
write.csv(paddy_reference, "paddy_reference.csv", row.names = FALSE)

print(table(paddy_reference$agriblock, paddy_reference$variety))  # every cell = 1
nrow(paddy_reference)                                             # 18

# ---- 3. Merge (inner join) ----
paddy_merged <- merge(paddy_clean, paddy_reference, by = c("agriblock", "variety"))
nrow(paddy_merged)                              # must be 2789
sum(is.na(paddy_merged$rec_N_kg_ha))            # must be 0

# ---- 4. Actual amounts per hectare ----
paddy_merged$seed_actual <- paddy_merged$seedrate_in_kg / paddy_merged$hectares
paddy_merged$N_actual    <- (paddy_merged$urea_40days * 0.46 + paddy_merged$dap_20days * 0.18) / paddy_merged$hectares
paddy_merged$P2O5_actual <- (paddy_merged$dap_20days * 0.46) / paddy_merged$hectares
paddy_merged$K2O_actual  <- (paddy_merged$potassh_50days * 0.60) / paddy_merged$hectares

# ---- 5. Deviations (actual minus recommended) ----
paddy_merged$seed_dev <- paddy_merged$seed_actual - paddy_merged$rec_seed_kg_ha
paddy_merged$N_dev    <- paddy_merged$N_actual    - paddy_merged$rec_N_kg_ha
paddy_merged$P2O5_dev <- paddy_merged$P2O5_actual - paddy_merged$rec_P2O5_kg_ha
paddy_merged$K2O_dev  <- paddy_merged$K2O_actual  - paddy_merged$rec_K2O_kg_ha

summary(paddy_merged[, c("seed_dev", "N_dev", "P2O5_dev", "K2O_dev")])

# ---- 6. Save ----
write.csv(paddy_merged, "paddy_merged.csv", row.names = FALSE)
# ---- 7. Deviation summary by variety (for the report) ----
dev_summary <- aggregate(cbind(seed_dev, N_dev, P2O5_dev, K2O_dev) ~ variety,
                         data = paddy_merged, FUN = mean)
dev_summary
write.csv(dev_summary, "deviation_summary_by_variety.csv", row.names = FALSE)