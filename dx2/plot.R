# Run: Rscript path/to/plot.R [input.csv] [output.png]
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else {
  getwd()
}
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[1L] else file.path(script_dir, "data.csv")
output <- if (length(args) >= 2L) args[2L] else
  file.path(script_dir, "solar_panel_iv_pv.png")
if (normalizePath(input, mustWork = TRUE) ==
    normalizePath(output, mustWork = FALSE)) {
  stop("Input and output paths must differ.")
}

data <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8")
if (ncol(data) < 5L || nrow(data) < 2L) {
  stop("Expected voltage, current, open-circuit voltage, and short-circuit current.")
}
voltage <- data[[1L]]
current <- data[[2L]]
voc <- data[[4L]][1L]
isc <- data[[5L]][1L]
if (!all(is.finite(voltage)) || !all(is.finite(current)) ||
    !is.finite(voc) || !is.finite(isc) || voc <= 0 || isc <= 0 ||
    any(voltage <= 0 | voltage >= voc)) {
  stop("Voltage/current measurements and endpoint values must be finite and valid.")
}

order_index <- order(voltage)
voltage <- voltage[order_index]
current <- current[order_index]
curve_voltage <- c(0, voltage, voc)
curve_current <- c(isc, current, 0)
# A shape-preserving cubic follows the sparse measurements without overshoot.
# Include Isc and Voc so the fitted curve reaches both measured endpoints.
if (any(diff(curve_voltage) <= 0)) {
  stop("Measured voltages must be distinct.")
}
trend_voltage <- seq(0, voc, length.out = 600L)
trend_current <- splinefun(curve_voltage, curve_current,
                           method = "monoH.FC")(trend_voltage)
trend_power <- trend_voltage * trend_current
power <- voltage * current
panel <- basename(script_dir)

png(output, width = 1800, height = 900, res = 150, type = "cairo")
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), family = "sans")

plot(trend_voltage, trend_current, type = "n",
     xlim = c(0, voc),
     ylim = c(0, max(curve_current, trend_current) * 1.06),
     xlab = "Voltage (V)", ylab = "Current (mA)",
     main = paste0(panel, "  I-V"))
grid(col = "#E5E7EB")
lines(trend_voltage, trend_current, col = "#0072B2", lwd = 2)
points(voltage, current, pch = 16, col = "#0072B2")
points(c(0, voc), c(isc, 0), pch = 21, bg = "white",
       col = "#0072B2", cex = 1.5, lwd = 2)
legend("topright", c("Fitted curve", "Measured", "Isc / Voc"),
       lty = c(1, NA, NA), pch = c(NA, 16, 21),
       pt.bg = c(NA, NA, "white"), col = "#0072B2", bty = "n")

plot(trend_voltage, trend_power, type = "n", xlim = c(0, voc),
     ylim = c(0, max(trend_power, power) * 1.1),
     xlab = "Voltage (V)", ylab = "Power (mW)",
     main = paste0(panel, "  P-V"))
grid(col = "#E5E7EB")
lines(trend_voltage, trend_power, col = "#D55E00", lwd = 2)
points(voltage, power, pch = 16, col = "#D55E00")
points(c(0, voc), c(0, 0), pch = 21, bg = "white",
       col = "#D55E00", cex = 1.5, lwd = 2)
max_index <- which.max(power)
points(voltage[max_index], power[max_index], pch = 17,
       col = "#D55E00", cex = 1.5)
legend("topright", sprintf("Measured max: %.1f mW", power[max_index]),
       pch = 17, col = "#D55E00", bty = "n")

invisible(dev.off())
message("Saved: ", output)
