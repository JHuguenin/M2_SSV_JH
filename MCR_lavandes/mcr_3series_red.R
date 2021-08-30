load("MCR_lavandes/tea_als_lav.RData")

library(lubridate)
name_ech <- c("lv1", "lv2", "lv3", "bl1",
              "lv4", "lv5", "lv6", "bl2",
              "lv7", "lv8", "lv9", "bl3")

xt <- as.numeric(rownames(tea.als$CList[[1]]))
nech <- length(tea.als$CList)

###################
fmr <- dim(tea.als$S)
matS <- seq(1,0,-.25,) %>% matrix(fmr[1],fmr[2],byrow = TRUE)
matS <- matS + tea.als$S
massS <- rownames(tea.als$S) %>% as.numeric()

ncMCR <- 5


# plot Rapport 
layout(1,1,1)
png(filename = "images/lavandes_S_MCR.png", width = 600, height = 400)
  par(mar = c(3,3,0.1,0.1), mgp = c(2,1,0))
    matplot(massS,matS, type = "l", xlim = c(57,210), ylim = c(0,1.3), 
            lty = 1, lwd = c(1,2,1,2,1), col = alpha(viridis(5),0.8),
            xlab = "m/z (Da)", ylab = "Intensity (u.a.)" )
    legend("topright", legend = paste("Comp.", 1:ncMCR), bty = "n", lty = 1,
           col = alpha(viridis(5),0.8), lwd = c(1,2,1,2,1))
dev.off()

##
tC <- rownames(tea.als$CList[[1]]) %>% as.numeric()


aa <- 6
bb <- 9

png(filename = "images/lavandes_C_MCR.png", width = 600, height = 750)
layout(matrix(c(1:4,5:8,9:12),4,3),c(aa+1,aa,aa),c(bb,bb,bb,bb+1))
for(i in 1:12){
matC <- tea.als$CList[[i]]

if(i %in% 1:3)         par(mar = c(0.1,4  ,0.1,0.1), mgp = c(2.5,1,0), xaxt = "n", yaxt = "s", cex.lab = 1.5, cex.axis = 1.5)
if(i %in% 4)           par(mar = c(4  ,4  ,0.1,0.1), mgp = c(2.5,1,0), xaxt = "s", yaxt = "s", cex.lab = 1.5, cex.axis = 1.5)
if(i %in% c(8,12))     par(mar = c(4  ,0.1,0.1,0.1), mgp = c(2.5,1,0), xaxt = "s", yaxt = "n", cex.lab = 1.5, cex.axis = 1.5)
if(i %in% c(5:7,9:11)) par(mar = c(0.1,0.1,0.1,0.1), mgp = c(2.5,1,0), xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.axis = 1.5)


matplot(tC,matC, type = "l",
        lty = 1, lwd = c(1,2,1,2,1), col = alpha(viridis(5),0.8),
        xlab = "Time of day (hour)", ylab = "Intensity (u.a.)" )
abline(v = c(6,22), lty = 2, col = c("darkgoldenrod1","blue4"))
text(1,max(matC)*0.9, labels = name_ech[i], cex = 1.5)
if(i==12){
  legend("topright", inset = 0.01, legend = paste("Comp.", 1:ncMCR), lty = 1, cex = 1.5,
                 col = alpha(viridis(5),0.8), lwd = c(1,2,1,2,1), box.col = "white" )
  }
}
dev.off()
##

