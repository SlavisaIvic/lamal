### LAMal ###
# Assura PharMed

library(ggplot2)
library(data.table)
library(paletteer)

lamal_calcul <- function(x, franchise, prime) {
  out <- prime*12 + pmin(x, franchise) + pmin(pmax(x - franchise, 0)*0.1, 700)
  return(out)
}

dt <- data.table(depenses = 1:1e+4,
                 p300 = 562.00,
                 p500 = 551.30,
                 p1000 = 524.10,
                 p1500 = 496.90,
                 p2000 = 469.90,
                 p2500 = 442.70)
dt[, `:=`(c300 = lamal_calcul(x = depenses, franchise = 300, prime = p300),
          c500 = lamal_calcul(x = depenses, franchise = 500, prime = p500),
          c1000 = lamal_calcul(x = depenses, franchise = 1000, prime = p1000),
          c1500 = lamal_calcul(x = depenses, franchise = 1500, prime = p1500),
          c2000 = lamal_calcul(x = depenses, franchise = 2000, prime = p2000),
          c2500 = lamal_calcul(x = depenses, franchise = 2500, prime = p2500)
          )]

dt <- melt(dt,
           id.vars = "depenses",
           measure.vars = paste0("c", c(300, 500, 1000, 1500, 2000, 2500)))

graph <- ggplot(data = dt,
                mapping = aes(x = depenses,
                              y = value,
                              color = variable)) +
  geom_line() +
  geom_point(x = 1890.67,
             y = 7203.07,
             color = "black") +
  annotate(geom = "text",
           label = "Changement de franchise\nde 2500 à 300 Fr.",
           x = 1910,
           y = 7170,
           color = "black",
           hjust = 0,
           vjust = 1,
           size = 9/14*4) +
  labs(x = "Frais médicaux prévus",
       y = "Primes et frais médicaux à charge de l'assuré") +
  scale_x_continuous(limits = c(0, 3e+3),
                     expand = c(0,0),
                     breaks = seq(0,3e+3,5e+2),
                     labels = paste0(seq(0,3e+3,5e+2), " Fr.")) +
  scale_y_continuous(limits = c(5e+3,8e+3),
                     expand = c(0,0),
                     breaks = seq(5e+3,8e+3,5e+2),
                     labels = paste0(seq(5e+3,8e+3,5e+2), " Fr.")) +
  scale_color_manual(values = paletteer_d("ggsci::category10_d3"),
                     breaks = paste0("c", c(300, 500, 1000, 1500, 2000, 2500)),
                     labels = paste0(c(300, 500, 1000, 1500, 2000, 2500), " Fr."),
                     name = "Franchise") +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black"),
        plot.margin = unit(c(5,15,5,5), units = "pt"))
#graph

ggsave(graph,
       filename = "lamal_2026.png",
       dpi = 600,
       height = 10,
       width = 20,
       units = "cm")

