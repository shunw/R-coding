dd <- data.frame(wafer = factor(rep(1:5, each = 6)),
                 operator = factor(rep(rep(1:3, each = 2), 5)),
                 thickness = c(0.62, 0.66, 0.53, 0.53, 0.51, 0.55,
                               0.99, 1.00, 1.05, 0.93, 1.05, 1.02,
                               0.82, 0.81, 0.80, 0.77, 0.90, 0.77,
                               0.85, 0.89, 0.83, 0.76, 0.79, 0.81,
                               0.59, 0.48, 0.39, 0.40, 0.46, 0.51))

# Summarize the data to output the mean, sd, min and max of thickness

library(ggplot2)
dsumm <- ddply(dd, .(wafer, operator), summarise, tmean = mean(thickness),
               tmin = min(thickness), tmax = max(thickness),
               tsd = sd(thickness))

# 'Multi-vari' plot:
p1 <- ggplot(dd) +
  geom_point(aes(x = wafer, y = thickness)) +
  geom_errorbar(data = dsumm, aes(x = wafer, y = tmean,
                                  ymin = tmin, ymax = tmax), colour = 'blue') +
  geom_segment(data = dsumm, aes(x = wafer, y = tmean, yend = tmean,
                                 xend = as.numeric(wafer) + 0.2),  colour = 'blue') +
  geom_segment(data = dsumm, aes(x = wafer, y = tmean, yend = tmean,
                                 xend = as.numeric(wafer) - 0.2),  colour = 'blue') +
  facet_wrap( ~ operator, nrow = 1) + xlab("")

# Standard deviation plot
p2 <- ggplot(dsumm, aes(x = wafer, y = tsd)) +
  geom_point(colour = 'blue') + geom_line(aes(group = 1), size =
                                            1, colour = 'blue') +
  facet_wrap( ~ operator, nrow = 1)

# Use the gridExtra package to combine the two graphs
library(gridExtra)
<<<<<<< HEAD
grid.arrange(p1, p2)
=======
grid.arrange(p1, p2)
>>>>>>> 709b90347a5c152bbac2b435ae4e415fcfa36b5a
