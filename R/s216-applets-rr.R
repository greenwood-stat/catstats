
# Simulated null distribution of relative risk
# Data: phat1 = 8/40, phat2 = 12/40

dat <- data.frame(x = rep(c("Grp1","Grp2"), each = 40),
                  y = rep(c("Success","Failure","Success","Failure"), c(8, 40-8, 12, 40-12))
                  )
                  
sim_rr <- rep(NA, 1000)
for (i in 1:1000) {
  newResponse <- sample(dat$y)
  row.pcts <- prop.table(table(dat$x, newResponse), 1)
  sim_rr[i] <- row.pcts[1,2]/row.pcts[2,2]
}

hist(sim_rr, xlab = "Relative Risk", ylab = "", breaks = 15,
     main = "",
     yaxt = "n")
legend("topright",
       legend = c(
         paste("Mean =", round(mean(sim_rr, na.rm = T), 3)),
         paste("SD =", round(sd(sim_rr, na.rm = T), 3))
       ),
       col = "white", bty = "n"
)
