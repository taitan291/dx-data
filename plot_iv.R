# Run: Rscript plot_iv.R [input.csv] [output.png] [smoothing]
# Smoothing: 0 to 1; larger values give a smoother trend (default: 0.6).
# Input is read only. Power is taken directly from data.0.
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[1L] else "dx0.csv"
output <- if (length(args) >= 2L) args[2L] else "solar_panel_iv_pv.png"
smoothing <- if (length(args) >= 3L) as.numeric(args[3L]) else 0.6
if (!is.finite(smoothing) || smoothing < 0 || smoothing > 1) {
  stop("Smoothing must be a number between 0 and 1.")
}
if (normalizePath(input, mustWork = TRUE) ==
    normalizePath(output, mustWork = FALSE)) {
  stop("Input and output paths must differ.")
}

# The instrument export uses tabs and starts with a sep= directive.
data <- read.delim(input, skip = 1L, check.names = FALSE)
required <- c("data.0", "data.1", "data.2")
stopifnot(all(required %in% names(data)), nrow(data) > 0L)
stopifnot(all(vapply(data[required], is.numeric, logical(1))))
stopifnot(all(is.finite(as.matrix(data[required]))))
power_mw <- data[["data.0"]]
voltage_v <- data[["data.1"]]
current_ma <- data[["data.2"]]

# Fit each measured series independently; do not extrapolate beyond the data.
trend_voltage <- seq(min(voltage_v), max(voltage_v), length.out = 600L)
fit_trend <- function(values) {
  if (length(unique(signif(voltage_v, 6L))) < 4L) {
    stop("At least four distinct voltages are required for a smooth trend.")
  }
  fit <- smooth.spline(voltage_v, values, spar = smoothing)
  predict(fit, x = trend_voltage)$y
}
power_trend <- fit_trend(power_mw)
current_trend <- fit_trend(current_ma)

# Independent scales with aligned zeros; negative measurements are retained.
power_top <- ceiling(max(power_mw) / 10) * 10
current_top <- ceiling(max(current_ma) / 5) * 5
lower_fraction <- min(-0.06, min(power_mw) / power_top - 0.02,
                      min(current_ma) / current_top - 0.02)
power_limits <- power_top * c(lower_fraction, 1.08)
current_limits <- current_top * c(lower_fraction, 1.08)
voltage_limits <- c(0, ceiling(max(voltage_v) * 2) / 2)
power_color <- "#D55E00"
current_color <- "#0072B2"

png(output, width = 1800, height = 1200, res = 180, type = "cairo")
par(mar = c(5, 5.5, 4.5, 5.5), las = 1, family = "sans")
plot(voltage_v, power_mw, type = "n", xlim = voltage_limits,
     ylim = power_limits, xaxs = "i", yaxs = "i", axes = FALSE,
     xlab = "Voltage/V", ylab = "",
     main = "単結晶")
abline(v = seq(0, voltage_limits[2], by = 0.5),
       h = seq(0, power_top, by = 10), col = "#E5E7EB")
abline(h = 0, col = "#9CA3AF")
axis(1, at = seq(0, voltage_limits[2], by = 0.5))
axis(2, at = seq(0, power_top, by = 10), col.axis = power_color)
mtext("Power/mW", side = 2, line = 3.5, las = 0, col = power_color)
lines(trend_voltage, power_trend, col = power_color, lwd = 1.5)
points(voltage_v, power_mw, pch = 17, cex = 0.825,
       col = adjustcolor(power_color, alpha.f = 0.45))

par(new = TRUE)
plot(voltage_v, current_ma, type = "n", xlim = voltage_limits, ylim = current_limits,
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
lines(trend_voltage, current_trend, col = current_color, lwd = 1.5)
points(voltage_v, current_ma, pch = 16, cex = 0.825,
       col = adjustcolor(current_color, alpha.f = 0.45))
axis(4, at = seq(0, current_top, by = 5), col.axis = current_color)
mtext("Currnet/mA", side = 4, line = 3.5, las = 0, col = current_color)
box(col = "#6B7280")
legend("topleft", inset = 0.025,
       legend = c("I-V: current (right axis)", "P-V: power (left axis)"),
       col = c(current_color, power_color), pch = c(16, 17),
       lty = 1, lwd = 1.5,
       bg = "white", box.col = "#D1D5DB", cex = 0.9)
invisible(dev.off())
message("Saved: ", output)
