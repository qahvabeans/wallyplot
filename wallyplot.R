# Author: Rasmus Säfvenberg, Jonas Borgström, William Larsson

# Undersök om ggplot är installerat på datorn. 
if(!require("ggplot2")) install.packages("ggplot2")

# x motsvarar en skattad linjär modell i form av lm. 
wally <- function(x,y=x) {

# Läs in x (predikterade värdena, d.v.s. yhatt) och y (residualerna)
y <- resid(x) 
x <- predict(x)

# Gör om x och y till numeriska variabler 
y <- as.numeric(y)
x <- as.numeric(x)

# Skapa 8st simulerade residualer från modellen
antal <- 8
e <- list(mode="vector",length=antal)
for (i in 1:antal) {
  e[[i]] <- rnorm(y, mean = 0, sd = sd(y))
}

# Gör om listan e till en data frame
df <- data.frame(e)

# Döp om kolumnerna
colnames(df) <- c("e1","e2","e3","e4","e5","e6","e7","e8")

# Lägg till x och y som variabler i data frame
df$x <- x
df$y <- y

# Ladda in paketet ggplot2
library(ggplot2)

# Skapa en plot för varje residualsimulering och en som kommer från den riktiga modellen
# na.rm = TRUE tar bort varningsmeddelandena som varnar om att visa punkter inte är inom y-intervallet,
# vilket, i princip, är ofarligt i detta fall enligt 68-95-99.7 regeln. Det betyder att 0.3% av punkterna hamnar utanför gränserna. 
plot1 <- ggplot(df,aes(x=x,y=e1)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot2 <- ggplot(df,aes(x=x,y=e2)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot3 <- ggplot(df,aes(x=x,y=e3)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot4 <- ggplot(df,aes(x=x,y=e4)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot5 <- ggplot(df,aes(x=x,y=e5)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot6 <- ggplot(df,aes(x=x,y=e6)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot7 <- ggplot(df,aes(x=x,y=e7)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot8 <- ggplot(df,aes(x=x,y=e8)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e") + scale_y_continuous(limits = c(range(y)))
plot9 <- ggplot(df,aes(x=x,y=y)) + geom_point(na.rm = TRUE) + xlab(expression(hat(y))) + ylab("e ") + scale_y_continuous(limits = c(range(y)))

# Skapa en funktion som kan plotta alla 9 plottar på samma sida. Ordningen går från vänster till höger. 
# Första raden har således plot 1-3, andra raden 4-6 och tredje 7-9. 
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = TRUE)) {
  require(grid)
  
  # Skapa en lista med alla plottar. 
  plots <- c(list(...), plotlist)
  
  # Antal plottar har samma värde som längden på listan plots
  numPlots = length(plots)
  
  # Om antalet plots är 1, printa då en plot.
  if (numPlots == 1) {
    print(plots[[1]])
  # Annars används paketet grid för att skapa en s.k. grid-layout som består av en matris med sekvensen 1-9 med tre rader.
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Här matchas den i:te plotten i sekvensen 1 till och med det högsta värdet på antalet plots som en data frame. 
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      # Printa sedan alla plots, där de matchade kolumnerna och raderna tillsätts i gridens layout. 
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Lägg till alla plots i en lista
plotlista <- list(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9)

# Slumpa ordningen i listan
plotlist <- sample(plotlista)

# Plotta alla residualplots i den slumpade ordningen
multiplot(plotlist = plotlist, cols = 3)

# En loop där man får gissa vilken den verkliga residualplotten är och R säger om det är rätt eller fel tills man gissat rätt. 
repeat {
  svar <- readline(prompt="Vilken är den verkliga resiudaplotten?: ")
  svar <- as.numeric(svar)
  if (plotlist[[(svar)]][["labels"]][["y"]] == "e " ) {
    print("Korrekt!")
    break
  } else print("Fel! Gör om, gör rätt!")
}
}

# Exempel på hur filen kan se ut. Den skattade lm modellen kan heta vad som helst. 
lm.modell <- lm(cyl~wt, data = mtcars)

# Lägg in den skattade linjära modellen som ett argument för funktionen och vóila!
wally(lm.modell)

