# Author: Rasmus S�fvenberg, Jonas Borgstr�m, William Larsson

# Unders�k om ggplot �r installerat p� datorn. 
if(!require("ggplot2")) install.packages("ggplot2")

# x motsvarar en skattad linj�r modell i form av lm. 
wally <- function(x,y=x) {

# L�s in x (predikterade v�rdena, d.v.s. yhatt) och y (residualerna)
y <- resid(x) 
x <- predict(x)

# G�r om x och y till numeriska variabler 
y <- as.numeric(y)
x <- as.numeric(x)

# Skapa 8st simulerade residualer fr�n modellen
antal <- 8
e <- list(mode="vector",length=antal)
for (i in 1:antal) {
  e[[i]] <- rnorm(y, mean = 0, sd = sd(y))
}

# G�r om listan e till en data frame
df <- data.frame(e)

# D�p om kolumnerna
colnames(df) <- c("e1","e2","e3","e4","e5","e6","e7","e8")

# L�gg till x och y som variabler i data frame
df$x <- x
df$y <- y

# Ladda in paketet ggplot2
library(ggplot2)

# Skapa en plot f�r varje residualsimulering och en som kommer fr�n den riktiga modellen
# na.rm = TRUE tar bort varningsmeddelandena som varnar om att visa punkter inte �r inom y-intervallet,
# vilket, i princip, �r ofarligt i detta fall enligt 68-95-99.7 regeln. Det betyder att 0.3% av punkterna hamnar utanf�r gr�nserna. 
plot1 <- ggplot(df,aes(x=x,y=e1)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot2 <- ggplot(df,aes(x=x,y=e2)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot3 <- ggplot(df,aes(x=x,y=e3)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot4 <- ggplot(df,aes(x=x,y=e4)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot5 <- ggplot(df,aes(x=x,y=e5)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot6 <- ggplot(df,aes(x=x,y=e6)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot7 <- ggplot(df,aes(x=x,y=e7)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot8 <- ggplot(df,aes(x=x,y=e8)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot9 <- ggplot(df,aes(x=x,y=y)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e ") + scale_y_continuous(limits = c(range(y)))

# Skapa en funktion som kan plotta alla 9 plottar p� samma sida. Ordningen g�r fr�n v�nster till h�ger. 
# F�rsta raden har s�ledes plot 1-3, andra raden 4-6 och tredje 7-9. 
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = TRUE)) {
  require(grid)
  
  # Skapa en lista med alla plottar. 
  plots <- c(list(...), plotlist)
  
  # Antal plottar har samma v�rde som l�ngden p� listan plots
  numPlots = length(plots)
  
  # Om antalet plots �r 1, printa d� en plot.
  if (numPlots == 1) {
    print(plots[[1]])
  # Annars anv�nds paketet grid f�r att skapa en s.k. grid-layout som best�r av en matris med sekvensen 1-9 med tre rader.
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # H�r matchas den i:te plotten i sekvensen 1 till och med det h�gsta v�rdet p� antalet plots som en data frame. 
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      # Printa sedan alla plots, d�r de matchade kolumnerna och raderna tills�tts i gridens layout. 
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# L�gg till alla plots i en lista
plotlista <- list(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9)

# Slumpa ordningen i listan
plotlist <- sample(plotlista)

# Plotta alla residualplots i den slumpade ordningen
multiplot(plotlist = plotlist, cols = 3)

# En loop d�r man f�r gissa vilken den verkliga residualplotten �r och R s�ger om det �r r�tt eller fel tills man gissat r�tt. 
repeat {
  svar <- readline(prompt="Vilken �r den verkliga resiudaplotten?: ")
  svar <- as.numeric(svar)
  if (plotlist[[(svar)]][["labels"]][["y"]] == "e " ) {
    print("Korrekt!")
    break
  } else print("Fel! G�r om, g�r r�tt!")
}
}

# Exempel p� hur filen kan se ut. Den skattade lm modellen kan heta vad som helst. 
lm.modell <- lm(cyl~wt, data = mtcars)

# L�gg in den skattade linj�ra modellen som ett argument f�r funktionen och v�ila!
wally(lm.modell)

