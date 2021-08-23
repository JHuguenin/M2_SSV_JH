n_acq = mt_h5$end

pk.MS <- cbind(sp$pk$MS[[n_acq[1]]],
               sp$pk$MS[[n_acq[2]]][,4],
               sp$pk$MS[[n_acq[3]]][,6])

n_ex <- which((mt$h5$start <= n_acq)&(mt$h5$end >= n_acq))
xmin <- 70
xmax <- 210
eph <- det_c(xmin,sp$xMS):det_c(xmax,sp$xMS)
ymax <- max(sp$MS[n_acq,eph])*1.1
titre <- paste0("S:/PTR-MS/Joris/Rapport_stage/M2_SSV_JH/images/3lavandes_8h.png")

png(filename = titre, width = 800, height = 580)
  par(mar = c(5,5,1,0.1), cex.main=2, cex.lab = 2, cex.axis = 2, mgp = c(3.5,1.5,0))
  matplot(sp$xMS, t(sp$MS[n_acq,]), xaxt="n",
          type = "l", col = viridis(3,.58), lwd = 2, lty = 1,
          xlim = c(xmin, xmax), ylim = c(0, ymax),
          xlab = "m/z (g/mol)", ylab = "Relative intensity (u.a.)",
          main = " ")
  legend("topright", legend = c("lavande 1", "lavande 2", "lavande 3"), col = viridis(3), lwd =2 )
  axis(1, at = seq(dizaine(xmin), dizaine(xmax), 10), lwd.ticks = 2, tck = -0.03)
  axis(1, at = seq(dizaine(xmin), dizaine(xmax)+10, 5), labels = FALSE, tck = -0.03)
  axis(1, at = seq(dizaine(xmin), dizaine(xmax)+10, 1), labels = FALSE, tck = -0.01)
  text(pk.MS[1,], pk.MS[2,], labels = pk.MS[1,], cex = 0.8, pos = 3, offset = 0.5)
dev.off()


dim(sp$MS)
##############

xmin <- 81
xmax <- 81.2
eph <- det_c(xmin,sp$xMS):det_c(xmax,sp$xMS)
ymax <- max(sp$MS[n_acq,eph])

par(mar = c(5,5,1,0.1), cex.main=2, cex.lab = 2, cex.axis = 2, mgp = c(3.5,1.5,0))
matplot(sp$xMS, sp$MS[5,],
        type = "l", col = viridis(3,.58)[1], lwd = 2, lty = 1,
        xlim = c(xmin, xmax), ylim = c(-20, ymax),
        xlab = "m/z (g/mol)", ylab = "Relative intensity (u.a.)",
        main = " ")
lines(sp$xMS_n_al, sp$MS_n_al[5,],type = "l", col = "black", lwd = 2, lty = 1)
lines(sp$xMS, sp$MS[5,],type = "l", col = viridis(3,.58)[1], lwd = 2, lty = 1)

legend("topright", legend = c("lavande 1", "lavande 3", "lavande 3"), col = viridis(3), lwd =2 )
dev.off()

### 
xmin <- 233
xmax <- 233.3
eph <- det_c(xmin,sp$xMS):det_c(xmax,sp$xMS)
ymax <- max(sp$MS[n_acq,eph])

par(mar = c(3,3,1,0.1), cex.lab = 1, cex.axis = 1, mgp = c(2,1,0))
matplot(sp$xMS, sp$MS[10,],
        type = "l", col = viridis(3,.58)[2], lwd = 2, lty = 1,
        xlim = c(xmin, xmax), ylim = c(-20, ymax),
        xlab = "m/z (g/mol)", ylab = "Relative intensity (u.a.)",
        main = " ")
lines(sp$xMS_n_al, sp$MS_n_al[10,],type = "l", col = "black", lwd = 2, lty = 1)
lines(sp$xMS, sp$MS[10,],type = "l", col = viridis(3,.58)[2], lwd = 2, lty = 1)

legend("topright", legend = c("lavande 2", "after alignement", "raw"),
       col = c(NA,viridis(3)[2],"black"), lwd =2,bty = "n", cex = 0.8)

dev.off()


