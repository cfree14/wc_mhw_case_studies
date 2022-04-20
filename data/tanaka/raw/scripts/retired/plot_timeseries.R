library(colorRamps)
library(ggplot2)
library(ggpubr)

rm(list = ls())

ipcc_temp <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
               rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
               rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
               rgb(244, 165, 130, maxColorValue = 255, alpha = 255),
               rgb(253, 219, 199, maxColorValue = 255, alpha = 255),
               rgb(247, 247, 247, maxColorValue = 255, alpha = 255),
               rgb(209, 229, 240, maxColorValue = 255, alpha = 255),
               rgb(146, 197, 222, maxColorValue = 255, alpha = 255),
               rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
               rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
               rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

p = c(0.975, 0.95, 0.9)[2]

load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, ".RData")); hadi = yy_anom
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, ".RData")); cobe = yy_anom
load(paste0("/Users/ktanaka/extreme_normalizations/results/ER/SST_TippingPoints_", p, ".RData")); er = yy_anom

hadi$source = "HadISSTv1.1"
cobe$source = "COBEv2"
er$source = "ERSSTv5"

df = rbind(hadi, cobe, er)
rownames(df) <- c()

df = tidyr::separate(df, time, c("Year", "Month"), sep = "-")

df$Month <- sprintf("%02d", as.numeric(df$Month))
df$Day = 01
df$Day <- sprintf("%02d", as.numeric(df$Day))
df$Time = paste(df$Year, df$Month, df$Day, sep = "-")
df$Time = as.Date(df$Time)

cobe_date = paste(subset(df, source == "COBEv2" & year_sum > 0.5)[1,2:3], collapse = "")
hadi_date = paste(subset(df, source == "HadISSTv1.1" & year_sum > 0.5)[1,2:3], collapse = "")
er_date = paste(subset(df, source == "ERSSTv5" & year_sum > 0.5)[1,2:3], collapse = "")

cbPalette <- c("#000000", "#56B4E9", "#E69F00")

df$source = factor(df$source, levels = c("COBEv2","HadISSTv1.1","ERSSTv5"))

# cbPalette = wes_palette("Darjeeling1")[c(1,3,5)]

CI_95 = df %>%
  group_by(source) %>% 
  subset(Year %in% c(1900:2018)) %>% 
  dplyr::summarise(mean = mean(year_sum, na.rm = TRUE),
                   sd = sd(year_sum, na.rm = TRUE),
                   n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

df %>% 
  # subset(Year %in% c(1900:1919)) %>% 
  ggplot(aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1.25, alpha = 0.75) +
  # geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_hline(data = CI_95, aes(yintercept = upper.ci, colour = source), size = 1) + 
  geom_hline(data = CI_95, aes(yintercept = mean, colour = source), size = 2) + 
  labs(x = "", y = "Area Fraction") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y")) 
# theme_pubr(I(15)) +
# facet_wrap(.~Month) +
# theme(legend.position = c(0.12, 0.95), 
#       axis.text.x = element_text(angle = 90, hjust = 1))
 
ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
ipcc_temp_expand = ipcc_temp_expand(90)
ipcc_temp_expand = matlab.like(90)

get_slope = function(s){
  
  slope = NULL
  
  png(paste0("/Users/ktanaka/Desktop/", s, "_30yr_slope.png"), height = 5, width = 5, res = 300, units = "in")
  
  for (y in 1:90) {
    
    # y = 1
    # s = "COBEv2"
    
    yy = y+1899
    
    dfl = df %>% 
      mutate(
        # year_sum = scale(year_sum, center = T),
             Year = as.numeric(Year)) %>% 
      subset(Year %in% c(yy:(yy+29))) 
    # %>% 
    #   subset(source == s) 
    
    b = lm(year_sum ~ Year, data = dfl)$coefficients
    
    if (yy == 1900) plot(1, type = "n", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-0.01, 0.01), axes = F, col.axis = "white")
    
    abline(0, b[2], col = alpha(ipcc_temp_expand[y], 0.5), lwd = 5)
    
    if (yy == 1989){
      
      axis(1); axis(2, las = 1)
      
      t_col = ipcc_temp_expand
      col.labels = c("1900-1929","1989-2018")
      plotrix::color.legend(0.4, #xl
                            -0.01, #yb
                            0.6, #xr
                            -0.005, #yt
                            col.labels,
                            t_col,
                            align="rb",
                            gradient="y")
      
      # legend("topleft", legend = s, bty = "n")
      
      dev.off()
      
    }
    
    b$p = paste0(yy, "-" (yy+29))
    slope = rbind(slope, b)
    
  }
  
  slope = as.data.frame(slope)
  
  return(slope)
}

get_slope("COBEv2")
get_slope("HadISSTv1.1")
get_slope("ERSSTv5")


pdf("~/Desktop/Time_Series__Month.pdf", height = 10, width = 10)
p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "prop of global area") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "20 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(20)) +
  facet_wrap(.~Month, ncol = 4) +
  # facet_grid(source ~ Month) +
  theme(legend.position = "bottom",
        legend.justification = c(1,0),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

print(p1)
dev.off()

tipped_hadi = subset(df, source == "HadISSTv1.1" & year_sum > 0.5)
tipped_cobe = subset(df, source == "COBEv2" & year_sum > 0.5)
tipped_er = subset(df, source == "ERSSTv5" & year_sum > 0.5)

ElNino = subset(df, Year %in% c(1905, 1906, 
                                1911, 1912, 1914, 1915, 
                                1940, 1941, 1942, 
                                1965, 1966, 
                                1972, 1973,
                                1982, 1983, 1987, 1988, 
                                1991, 1992, 1997, 1998, 
                                2015, 2016))

p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  
  # geom_vline(data = tipped_cobe, aes(xintercept = Time), color = cbPalette[1], alpha = 0.1) +
  # geom_vline(data = tipped_hadi, aes(xintercept = Time), color = cbPalette[2], alpha = 0.1) +
  
  # geom_vline(data = ElNino, aes(xintercept = Time), color = "red", alpha = 0.1) + 
  
  annotate("segment", x = tipped_cobe[,6], xend = tipped_cobe[,6], y = -0.15, yend = -0.2, color = cbPalette[1]) +
  annotate("segment", x = tipped_hadi[,6], xend = tipped_hadi[,6], y = -0.1, yend = -0.15, color = cbPalette[2]) +
  annotate("segment", x = tipped_er[,6], xend = tipped_er[,6], y = -0.05, yend = -0.1, color = cbPalette[3]) + 
  
  annotate("segment", x = tipped_cobe[1,6], xend = tipped_cobe[1,6], y = 0.55, yend = 0.6, color = cbPalette[1], linetype = 2) +
  annotate("segment", x = tipped_cobe[2,6], xend = tipped_cobe[2,6], y = 0.55, yend = 0.64, color = cbPalette[1], linetype = 2) +
  annotate("segment", x = tipped_cobe[3,6], xend = tipped_cobe[3,6], y = 0.55, yend = 0.68, color = cbPalette[1], linetype = 2) +
  
  annotate("segment", x = tipped_hadi[1,6], xend = tipped_hadi[1,6], y = 0.55, yend = 0.8, color = cbPalette[2], linetype = 2) +
  annotate("segment", x = tipped_hadi[2,6], xend = tipped_hadi[2,6], y = 0.55, yend = 0.84, color = cbPalette[2], linetype = 2) +
  annotate("segment", x = tipped_hadi[3,6], xend = tipped_hadi[3,6], y = 0.55, yend = 0.88, color = cbPalette[2], linetype = 2) +
  
  annotate("segment", x = tipped_er[1,6], xend = tipped_er[1,6], y = 0.55, yend = 0.9, color = cbPalette[3], linetype = 2) +
  annotate("segment", x = tipped_er[2,6], xend = tipped_er[2,6], y = 0.55, yend = 0.94, color = cbPalette[3], linetype = 2) +
  annotate("segment", x = tipped_er[3,6], xend = tipped_er[3,6], y = 0.55, yend = 0.98, color = cbPalette[3], linetype = 2) +
  
  # annotate("segment", x = as.Date(paste(2014, 04, 01, sep = "-")), xend = as.Date(paste(2014, 04, 01, sep = "-")), y = 0.75, yend = 0.84, color = cbPalette[1]) +
  # annotate("text", x = as.Date(paste(2014, 04, 01, sep = "-")), y = 0.86, hjust = 1, label = "2014-01, point of no return", color = cbPalette[1]) + 
  
  geom_text(data = tipped_cobe[1,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.6), colour = cbPalette[1], angle = 0, hjust = 1.1, text = element_text(size = 11)) +
  geom_text(data = tipped_cobe[2,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.64), colour = cbPalette[1], angle = 0, hjust = 1.1, text = element_text(size = 11)) +
  geom_text(data = tipped_cobe[3,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.68), colour = cbPalette[1], angle = 0, hjust = 1.1, text = element_text(size = 11)) +
  
  geom_text(data = tipped_hadi[1,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.8), colour = cbPalette[2], angle = 0, hjust = 1.1, text = element_text(size = 11)) +
  geom_text(data = tipped_hadi[2,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.84), colour = cbPalette[2], angle = 0, hjust = 1.1,text = element_text(size = 11)) +
  geom_text(data = tipped_hadi[3,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.88), colour = cbPalette[2], angle = 0, hjust = 1.1,text = element_text(size = 11)) +
  
  geom_text(data = tipped_er[1,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.9), colour = cbPalette[3], angle = 0, hjust = 1.1,text = element_text(size = 11)) +
  geom_text(data = tipped_er[2,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.94), colour = cbPalette[3], angle = 0, hjust = 1.1,text = element_text(size = 11)) +
  geom_text(data = tipped_er[3,], aes(x = Time, label = paste0(Year, "-", Month), y = 0.98), colour = cbPalette[3], angle = 0, hjust = 1.1,text = element_text(size = 11)) +
  
  geom_line(size = 1, alpha = 0.75) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  annotate("text", x = as.Date(paste(1915, 01, 01, sep = "-")), y = 0.5, vjust = -1, label = "50% threshold", color = "black") + 
  labs(x = "", y = "Proportion of Global Area") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  theme_pubr(I(20)) +
  theme(legend.position = c(0.15, 0.95),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

pdf(paste0("~/Desktop/Time_Series_", p, ".pdf"), height = 5, width = 8)
p1
dev.off()

