graphics.off()

paddy_merged <- read.csv("paddy_merged.csv", stringsAsFactors = FALSE)
paddy_merged$trash_per_ha <- paddy_merged$trash_in_bundles / paddy_merged$hectares

clim_cols <- grep("temp|rain|humidity|wind_speed", names(paddy_merged), value = TRUE)
clim_cols <- clim_cols[sapply(paddy_merged[clim_cols], is.numeric)]
cm <- cor(paddy_merged[, c("yield_kg_ha", clim_cols, "trash_per_ha")])

n <- ncol(cm)
pal <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)

png("plot_correlation_matrix.png", width = 2000, height = 1800, res = 200)
layout(matrix(1:2, nrow = 1), widths = c(7, 1))      # wider key panel
par(mar = c(10, 10, 3, 1))
image(1:n, 1:n, t(cm[n:1, ]), col = pal, zlim = c(-1, 1),
      axes = FALSE, xlab = "", ylab = "", main = "Correlation matrix")
axis(1, 1:n, colnames(cm), las = 2, cex.axis = 0.55)
axis(2, 1:n, rev(rownames(cm)), las = 2, cex.axis = 0.55)

par(mar = c(10, 0.5, 3, 3.5))                        # smaller margins for the key
image(1, seq(-1, 1, length.out = 100), matrix(seq(-1, 1, length.out = 100), nrow = 1),
      col = pal, axes = FALSE, xlab = "", ylab = "")
axis(4, las = 1, cex.axis = 0.7)
mtext("Pearson r", side = 4, line = 2.3, cex = 0.7)
dev.off()