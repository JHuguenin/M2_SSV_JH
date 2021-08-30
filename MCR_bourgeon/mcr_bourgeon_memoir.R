load("MCR_lavandes/teaals.RData")

# Image pour Master
fmr <- dim(tea.als$S)
matS <- seq(1,0,-.25,) %>% matrix(fmr[1],fmr[2],byrow = TRUE)
matS <- matS + tea.als$S
massS <- rownames(tea.als$S) %>% as.numeric()

ncMCR <- 5

png(filename = "images/bourgeon_S_MCR.png", width = 600, height = 400)
  par(mar = c(3,3,0.1,0.1), mgp = c(2,1,0))
  matplot(massS,matS, type = "l", xlim = c(57,210), ylim = c(0,1.3), 
          lty = 1, lwd = 2, col = alpha(viridis(5),0.8),
          xlab = "m/z (Da)", ylab = "Intensity (u.a.)" )
  legend("topright", legend = paste("Comp.", 1:ncMCR), bty = "n", lty = 1,
         col = alpha(viridis(5),0.8), lwd = 2)
dev.off()

##

matC <- tea.als$CList[[1]]
tC <- rownames(tea.als$CList[[1]]) %>% as.numeric()

D0 <- 1614988800

png(filename = "images/bourgeon_C_MCR.png", width = 600, height = 400)
  par(mar = c(0.5,3,0.1,0.1), mgp = c(2,1,0))
  matplot(tC,matC, type = "l", ylim = c(0,10^6), 
          lty = 1, lwd = 1.5, col = alpha(viridis(5),0.8),
          xlab = "m/z (Da)", ylab = "Intensity (u.a.)" )
  legend("topleft", legend = paste("Comp.", 1:ncMCR), bty = "n", lty = 1,
         col = alpha(viridis(5),0.8), lwd = 1.5)
  
  seq(0,100,2) %>% multiply_by(3600) %>% add(D0) %>%
    abline(v = ., lty = 2, col = alpha("grey",.5))
  c(24,48) %>% multiply_by(3600) %>% add(D0) %>% abline(v = ., lty = 2, col = "darkorange3")
  fmr <- seq(0,22, by = 2) %>% paste("h") %>% rep(3)
  Ym <- 7.5*10^5
  seq(0,70, by = 2) %>% multiply_by(3600) %>% add(D0) %>% text(Ym,labels = fmr, adj = c(.5,.5))
dev.off()

##

#length(tC): 6127  
brn <-  4000:5500

png(filename = "images/bourgeon_C_MCR_zoom.png", width = 600, height = 400)
  par(mar = c(0.5,3,0.1,0.1), mgp = c(2,1,0))
    matplot(tC[brn], matC[brn,], type = "l", ylim = c(0,5*10^5), 
            lty = 1, lwd = 2, col = alpha(viridis(5),0.8),
            xlab = "m/z (Da)", ylab = "Intensity (u.a.)" )
    legend("topleft", legend = c("",paste("Comp.", 1:ncMCR)), bty = "n", lty = 1,
           col = c(NA,alpha(viridis(5),0.8)), lwd = 2)
    
    seq(0,100,2) %>% multiply_by(3600) %>% add(D0) %>%
      abline(v = ., lty = 2, col = alpha("grey",.5))
    c(24,48) %>% multiply_by(3600) %>% add(D0) %>% abline(v = ., lty = 2, col = "darkorange3")
    fmr <- seq(0,22, by = 2) %>% paste("h") %>% rep(3)
    Ym <- 5*10^5
    seq(0,70, by = 2) %>% multiply_by(3600) %>% add(D0) %>% text(Ym,labels = fmr, adj = c(.5,.5))
dev.off()
