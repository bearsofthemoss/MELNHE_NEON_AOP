library(ggplot2)
library(tidyr)
library(dplyr)
## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada <- read.csv(here::here( "data_folder","processed_spectra2.csv"))



age_class <- c("Young forest","Mid-aged forest","Mature forest")



# stand ages
dada$Age[dada$Stand=="C1"]<-"Young forest"
dada$Age[dada$Stand=="C2"]<-"Young forest"
dada$Age[dada$Stand=="C3"]<-"Young forest"
dada$Age[dada$Stand=="C4"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C5"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C6"]<-"Mid-aged forest" 
dada$Age[dada$Stand=="C7"]<-"Mature forest"
dada$Age[dada$Stand=="C8"]<-"Mature forest"
dada$Age[dada$Stand=="C9"]<-"Mature forest"

names(dada)
# make a 'long' version of dada
ldada<-gather(dada, "wvl","refl",8:352)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)



# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345


## Univariate analysis
# for N*P Anova
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada$Ntrmt <- factor(  ifelse(ldada$Treatment == "N" | ldada$Treatment == "NP", "N", "NoN"))
ldada$Ptrmt <- factor(  ifelse(ldada$Treatment %in% c("P", "NP"), "P", "NoP"))


ldada$region <- cut(ldada$wvl,
                    breaks = c(0, 700, 1100, 2400),
                    labels = c("VIS (400–700 nm)", "NIR (700–1100 nm)", "SWIR (1100–2400 nm)"),
                    right  = TRUE)
names(ldada)


# ── 4. Plot-level summaries: mean ± SD per Treatment × wavelength ─────────────
plot_sum <- aggregate(refl ~ Stand + Treatment + Age + wvl + region,
                      data = ldada, FUN = mean)

names(plot_sum)[names(plot_sum) == "refl"] <- "plot_mean"

plot_sd  <- aggregate(refl ~ Stand + Treatment + Age + wvl + region,
                      data = ldada, FUN = sd)
names(plot_sd)[names(plot_sd) == "refl"] <- "plot_sd"

plot_level <- merge(plot_sum, plot_sd,
                    by = c("Stand","Treatment","Age","wvl","region"))


# ── 5. Factor ordering ────────────────────────────────────────────────────────
plot_level$Treatment <- factor(plot_level$Treatment,
                            levels = c("Control","N","P","NP"))

trt_colors <- c("Control" = "black",
                "N"       = "#2166ac",
                "P"       = "#d73027",
                "NP"      = "#7B2D8B")


plot_level$staplo <- paste(plot_level$Stand, plot_level$Treatment)
# ── 6. Plot ───────────────────────────────────────────────────────────────────
# spec_region_fig <- ggplot(plot_level, aes(x = wvl, y = plot_mean,
#                                           colour = Treatment, fill = Treatment, group = staplo)) +
#   
#   geom_line(linewidth = 0.6, aes(linetype = Age)) +
#   
#   facet_wrap(~ region, scales = "free", nrow = 1) +
#   
#   scale_colour_manual(values = trt_colors, name = "Treatment") +
#   scale_fill_manual  (values = trt_colors, name = "Treatment") +
#   
#   scale_linetype_manual(
#     name   = "Age",
#     values = c("Mature forest"   = "solid",
#                "Mid-aged forest" = "longdash",
#                "Young forest"    = "dotted"),
#     guide  = guide_legend(
#       nrow           = 1,
#       override.aes   = list(colour = "grey30", linewidth = 0.7)
#     )
#   ) +
#   
#   scale_x_continuous(
#     breaks = scales::pretty_breaks(n = 5),
#     labels = function(x) paste0(x, " nm")
#   ) +
#   
#   scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
#   
#   labs(x = "Wavelength (nm)", y = "Reflectance") +
#   
#   guides(
#     colour   = guide_legend(nrow = 1, order = 1,
#                             override.aes = list(linewidth = 1.2)),
#     fill     = guide_legend(nrow = 1, order = 1),
#     linetype = guide_legend(nrow = 1, order = 2,
#                             override.aes = list(colour = "grey30", linewidth = 0.7))
#   ) +
#   
#   theme_bw(base_size = 12) +
#   theme(
#     strip.background   = element_rect(fill = "grey94", colour = "grey60", linewidth = 0.4),
#     strip.text         = element_text(face = "bold", size = 11),
#     axis.title         = element_text(size = 11),
#     axis.text          = element_text(size = 9, colour = "grey20"),
#     axis.ticks         = element_line(linewidth = 0.3, colour = "grey50"),
#     panel.border       = element_rect(linewidth = 0.5, colour = "grey40"),
#     panel.grid.major   = element_line(linewidth = 0.3, colour = "grey88"),
#     panel.grid.minor   = element_blank(),
#     legend.position    = "bottom",
#     legend.direction   = "horizontal",
#     legend.box         = "vertical",       # two rows: Treatment row, Age row
#     legend.box.spacing = unit(0.15, "cm"),
#     legend.key.width   = unit(1.4, "cm"),
#     legend.key.height  = unit(0.45, "cm"),
#     legend.text        = element_text(size = 9),
#     legend.title       = element_text(size = 10, face = "bold"),
#     legend.spacing.y   = unit(0.1, "cm"),
#     plot.margin        = margin(6, 8, 4, 6, "pt")
#   )
# 


plot_level$line_group <- NA
plot_level$line_group[plot_level$wvl < 1340] <- "1"
plot_level$line_group[plot_level$wvl > 1450 & m_vip$wvl < 1780] <- "2"
plot_level$line_group[plot_level$wvl > 1960] <- "3"

plot_level <- plot_level[!is.na(plot_level$line_group),]

library(ggh4x)
plot_level$staplo_line <- paste(plot_level$staplo, plot_level$line_group)

spec_region_fig <- ggplot(plot_level, aes(x = wvl, y = plot_mean,
                                          colour = Treatment, fill = Treatment, group = staplo_line)) +
  
  geom_line(linewidth = 0.6, aes(linetype = Age)) +
  
  facet_wrap(~ region, scales = "free", nrow = 1) +
  
  # Per-facet x-axis breaks — order matches factor levels of region
  facetted_pos_scales(
    x = list(
      region == "VIS (400–700 nm)"    ~ scale_x_continuous(
        breaks = c(400, 500, 600, 700),
        labels = function(x) paste0(x, " nm")),
      region == "NIR (700–1100 nm)"   ~ scale_x_continuous(
        breaks = c(700, 800, 900, 1000, 1100),
        labels = function(x) paste0(x, " nm")),
      region == "SWIR (1100–2400 nm)" ~ scale_x_continuous(
        breaks = c(1200, 1500, 1800, 2100, 2400),
        labels = function(x) paste0(x, " nm"))
    )
  ) +
  
  scale_colour_manual(values = trt_colors, name = "Treatment") +
  scale_fill_manual  (values = trt_colors, name = "Treatment") +
  
  scale_linetype_manual(
    name   = "Age",
    values = c("Mature forest"   = "solid",
               "Mid-aged forest" = "longdash",
               "Young forest"    = "dotted"),
    guide  = guide_legend(
      nrow         = 1,
      override.aes = list(colour = "grey30", linewidth = 0.7)
    )
  ) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  
  labs(x = "Wavelength (nm)", y = "Reflectance") +
  
  guides(
    colour   = guide_legend(nrow = 1, order = 1,
                            override.aes = list(linewidth = 1.2)),
    fill     = guide_legend(nrow = 1, order = 1),
    linetype = guide_legend(nrow = 1, order = 2,
                            override.aes = list(colour = "grey30", linewidth = 0.7))
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    strip.background   = element_rect(fill = "grey94", colour = "grey60", linewidth = 0.4),
    strip.text         = element_text( size = 15),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 14, colour = "grey20"),
    axis.text.y = element_text(size = 14, colour = "grey20"),
    axis.title=element_text(size=14),
    axis.ticks         = element_line(linewidth = 0.3, colour = "grey50"),
    panel.border       = element_rect(linewidth = 0.5, colour = "grey40"),
    panel.grid.major   = element_line(linewidth = 0.3, colour = "grey88"),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.box         = "vertical",
    legend.box.spacing = unit(0.15, "cm"),
    legend.key.width   = unit(1.4, "cm"),
    legend.key.height  = unit(0.45, "cm"),
    legend.text        = element_text(size = 9),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.spacing.y   = unit(0.1, "cm"),
    plot.margin        = margin(6, 8, 4, 6, "pt")
  )

spec_region_fig


 spec_region_fig




ggsave("figure_2a.png", spec_region_fig , 
       width = 10, height = 3.5, dpi = 300, bg = "white")

