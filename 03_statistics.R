paddy_merged <- read.csv("paddy_merged.csv", stringsAsFactors = FALSE)
# ---------- 1. Group summaries (mean, median, SD, n) ----------
group_summary <- function(cols) {
  out <- do.call(data.frame,
                 aggregate(paddy_merged$yield_kg_ha, by = paddy_merged[cols],
                           FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x), n = length(x))))
  names(out) <- c(cols, "mean", "median", "sd", "n")
  out[, c("mean", "median", "sd")] <- round(out[, c("mean", "median", "sd")], 1)
  out
}
by_variety <- group_summary("variety")
by_soil    <- group_summary("soil_types")
by_block   <- group_summary("agriblock")
by_var_soil <- group_summary(c("variety", "soil_types"))
by_variety; by_soil; by_block; by_var_soil

# ---------- 2. ANOVA (does mean yield differ between groups?) ----------
a_var   <- aov(yield_kg_ha ~ variety,    data = paddy_merged)
a_soil  <- aov(yield_kg_ha ~ soil_types, data = paddy_merged)
a_block <- aov(yield_kg_ha ~ agriblock,  data = paddy_merged)
summary(a_var); summary(a_soil); summary(a_block)

# Post-hoc: which pairs differ? (only meaningful if the ANOVA p < 0.05)
TukeyHSD(a_var)

# Variety x soil interaction
a_two <- aov(yield_kg_ha ~ variety * soil_types, data = paddy_merged)
summary(a_two)

# Effect size (eta squared: share of yield variation explained)
ss <- summary(a_var)[[1]][["Sum Sq"]]
eta_sq_variety <- ss[1] / sum(ss)
eta_sq_variety

# ---------- 3. Check ANOVA assumptions ----------
set.seed(1)
shapiro.test(sample(residuals(a_var), 2000))      # normality of residuals
bartlett.test(yield_kg_ha ~ variety, data = paddy_merged)   # equal variances
kruskal.test(yield_kg_ha ~ variety, data = paddy_merged)    # backup test if assumptions fail

# ---------- 4. Pearson correlations: microclimate vs yield ----------
clim_cols <- grep("temp|rain|humidity|wind_speed", names(paddy_merged), value = TRUE)
clim_cols <- clim_cols[sapply(paddy_merged[clim_cols], is.numeric)]

r_val <- sapply(clim_cols, function(v) cor(paddy_merged[[v]], paddy_merged$yield_kg_ha))
p_val <- sapply(clim_cols, function(v) cor.test(paddy_merged[[v]], paddy_merged$yield_kg_ha)$p.value)
cor_table <- data.frame(variable = clim_cols, r = round(r_val, 3), p_value = signif(p_val, 3))
cor_table <- cor_table[order(-abs(cor_table$r)), ]
cor_table

# Input deviations vs yield
dev_cols <- c("seed_dev", "N_dev", "P2O5_dev", "K2O_dev")
dev_r <- sapply(dev_cols, function(v) cor(paddy_merged[[v]], paddy_merged$yield_kg_ha))
round(dev_r, 3)

# Full correlation matrix (for the heatmap later)
cor_mat <- cor(paddy_merged[, c("yield_kg_ha", clim_cols, dev_cols)])

# ---------- 5. Save everything ----------
write.csv(by_variety,   "summary_by_variety.csv",   row.names = FALSE)
write.csv(by_soil,      "summary_by_soil.csv",      row.names = FALSE)
write.csv(by_block,     "summary_by_agriblock.csv", row.names = FALSE)
write.csv(by_var_soil,  "summary_by_variety_soil.csv", row.names = FALSE)
write.csv(cor_table,    "correlation_microclimate_yield.csv", row.names = FALSE)
write.csv(round(cor_mat, 3), "correlation_matrix.csv")