rm(list = ls())

f_csfe <- function(x, y_bench, y_real) {
  error_bench <- (y_bench - y_real)^2
  error_x <- (x - y_real)^2
  result <- cumsum(error_bench - error_x)
  return(result)
}

csfe_list <- list()

for (i in 9:12) {
  
  load("Germany/forecasts/ar.rda")
  y_bench = forecasts[, i]
  
  load("Germany/forecasts/yout.rda")
  y_real = yout[, 1]
  
  load("Germany/forecasts/l2boost.rda")
  x = forecasts[, i]
  
  csfe_list[[i]] <- f_csfe(
    x,
    y_bench,
    y_real
  )
}

# h = 1:4
# par(mar = c(8, 4, 1, 1) + 0.1)
# 
# plot(csfe_list[[1]], type = "l", col = "red", xlab = "Janela móvel", ylab = "CSFE", lty = 1, ylim = c(-0.1, 3.5), cex.lab = 0.8, cex.axis = 0.8)
# lines(csfe_list[[2]], col = "green", lty = 1)
# lines(csfe_list[[3]], col = "blue", lty = 1)
# lines(csfe_list[[4]], col = "orange", lty = 1)
# 
# abline(h = 0)
# 
# legend("bottom", inset = c(0, -1), legend = paste("h =", 1:4), col = c("red", "green", "blue", "orange"), lty = 1, xpd = TRUE, horiz = TRUE, pt.cex = 1, cex = 0.8)



# h = 5:8
# par(mar = c(8, 4, 1, 1) + 0.1)
# 
# plot(csfe_list[[5]], type = "l", col = "red", xlab = "Janela móvel", ylab = "CSFE", lty = 1, ylim = c(-0.1, 1.9), cex.lab = 0.8, cex.axis = 0.8)
# lines(csfe_list[[6]], col = "green", lty = 1)
# lines(csfe_list[[7]], col = "blue", lty = 1)
# lines(csfe_list[[8]], col = "orange", lty = 1)
# 
# abline(h = 0)
# 
# legend("bottom", inset = c(0, -1), legend = paste("h =", 5:8), col = c("red", "green", "blue", "orange"), lty = 1, xpd = TRUE, horiz = TRUE, pt.cex = 1, cex = 0.8)



# h = 9:12
par(mar = c(8, 4, 1, 1) + 0.1)

plot(csfe_list[[9]], type = "l", col = "red", xlab = "Janela móvel", ylab = "CSFE", lty = 1, ylim = c(-0.2, 1.5), cex.lab = 0.8, cex.axis = 0.8)
lines(csfe_list[[10]], col = "green", lty = 1)
lines(csfe_list[[11]], col = "blue", lty = 1)
lines(csfe_list[[12]], col = "orange", lty = 1)

abline(h = 0)

legend("bottom", inset = c(0, -1), legend = paste("h =", 9:12), col = c("red", "green", "blue", "orange"), lty = 1, xpd = TRUE, horiz = TRUE, pt.cex = 1, cex = 0.8)
