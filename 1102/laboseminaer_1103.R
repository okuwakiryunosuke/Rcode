
library(ggplot2)
library(gdxrrw)
library(grid)

# ============================================================
# 1. READ GDX
# ============================================================

igdx("C:/GAMS/54")

merged_gdx <- rgdx.param(
  "global/1102/merged_output.gdx",
  "iamc_gdx"
)

df <- as.data.frame(merged_gdx)

# Sc       : Scenario
# Sr       : Region
# Sv       : Variable
# Y5       : Year
# iamc_gdx : Value


# ============================================================
# 2. PRIMARY ENERGY MAPPING
# ============================================================

primary_mapping <- c(
  "Prm_Ene_Oil_wo_CCS" = "Oil w/o CCS",
  "Prm_Ene_Oil_w_CCS"  = "Oil w/ CCS",
  
  "Prm_Ene_Coa_wo_CCS" = "Coal w/o CCS",
  "Prm_Ene_Coa_w_CCS"  = "Coal w/ CCS",
  
  "Prm_Ene_Gas_wo_CCS" = "Gas w/o CCS",
  "Prm_Ene_Gas_w_CCS"  = "Gas w/ CCS",
  
  "Prm_Ene_Bio_wo_CCS" = "Biomass w/o CCS",
  "Prm_Ene_Bio_w_CCS"  = "Biomass w/ CCS",
  
  "Prm_Ene_Nuc"   = "Nuclear",
  "Prm_Ene_Hyp"   = "Hydro",
  "Prm_Ene_Solar" = "Solar",
  "Prm_Ene_Win"   = "Wind",
  "Prm_Ene_Oce"   = "Ocean",
  "Prm_Ene_Geo"   = "Other"
)


# ============================================================
# 3. PRIMARY ENERGY STACK ORDER
# 下から Oil -> Coal -> Gas -> Other
# ============================================================

primary_order <- c(
  "Oil w/o CCS",
  "Oil w/ CCS",
  
  "Coal w/o CCS",
  "Coal w/ CCS",
  
  "Gas w/o CCS",
  "Gas w/ CCS",
  
  "Biomass w/o CCS",
  "Biomass w/ CCS",
  
  "Nuclear",
  "Hydro",
  "Solar",
  "Wind",
  "Ocean",
  "Other"
)


# ============================================================
# 4. PRIMARY ENERGY COLORS
# ============================================================

primary_colors <- c(
  "Coal w/o CCS" = "#7F7F7F",
  "Coal w/ CCS"  = "#4D4D4D",
  
  "Oil w/o CCS" = "#F4A261",
  "Oil w/ CCS"  = "#E76F51",
  
  "Gas w/o CCS" = "#E9D66B",
  "Gas w/ CCS"  = "#C9A227",
  
  "Biomass w/o CCS" = "#A6D96A",
  "Biomass w/ CCS"  = "#66A61E",
  
  "Nuclear" = "#F6D7A7",
  "Hydro"   = "#A9C4E4",
  "Solar"   = "lightsalmon",
  "Wind"    = "#80B1D3",
  "Ocean"   = "#66C2A5",
  "Other"   = "#B87333"
)


# ============================================================
# 5. CHECK DATA
# ============================================================

print(names(df))
print(unique(df$Sr))
print(unique(df$Sc))

print(
  grep(
    "^Prm_Ene",
    unique(df$Sv),
    value = TRUE
  )
)


# ============================================================
# 6. FILTER DATA
# World / Import / Nolimit
# ============================================================

plot_data <- df[
  as.character(df$Sr) == "World" &
    as.character(df$Sc) %in% c("Import", "Nolimit") &
    as.character(df$Sv) %in% names(primary_mapping) &
    as.numeric(as.character(df$Y5)) >= 2005 &
    as.numeric(as.character(df$Y5)) <= 2050,
  ,
  drop = FALSE
]

if (nrow(plot_data) == 0) {
  stop("No matching data. Check World, Sc and Sv.")
}


# ============================================================
# 7. CONVERT DATA TYPES
# ============================================================

plot_data$Sc <- as.character(plot_data$Sc)
plot_data$Sr <- as.character(plot_data$Sr)
plot_data$Sv <- as.character(plot_data$Sv)

plot_data$Y5 <- as.numeric(
  as.character(plot_data$Y5)
)

plot_data$iamc_gdx <- as.numeric(
  as.character(plot_data$iamc_gdx)
)


# ============================================================
# 8. RENAME SCENARIOS
# ============================================================

plot_data$Sc[
  plot_data$Sc == "Import"
] <- "Calibration"


# ============================================================
# 9. RENAME ENERGY VARIABLES
# ============================================================

plot_data$Energy <- unname(
  primary_mapping[plot_data$Sv]
)


# ============================================================
# 10. FACTOR ORDER
# ============================================================

plot_data$Sc <- factor(
  plot_data$Sc,
  levels = c("Calibration", "Nolimit")
)

plot_data$Energy <- factor(
  plot_data$Energy,
  levels = primary_order
)


# ============================================================
# 11. AGGREGATE
# ============================================================

plot_data <- aggregate(
  iamc_gdx ~ Sc + Y5 + Energy,
  data = plot_data,
  FUN = sum
)

# シナリオ別・年別の総量確認
total_check <- aggregate(
  iamc_gdx ~ Sc + Y5,
  data = plot_data,
  FUN = sum
)

print(total_check)


# ============================================================
# 12. STACKED AREA PLOT
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = Y5,
    y = iamc_gdx,
    fill = Energy,
    group = Energy
  )
) +
  
  geom_area(
    position = position_stack(reverse = TRUE),
    color = NA,
    alpha = 1,
    show.legend = TRUE
  ) +
  
  facet_wrap(
    ~ Sc,
    nrow = 1,
    scales = "fixed"
  ) +
  
  scale_fill_manual(
    values = primary_colors,
    limits = primary_order,
    breaks = primary_order,
    drop = FALSE
  ) +
  
  scale_x_continuous(
    breaks = seq(2005, 2050, 5),
    limits = c(2005, 2050),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Year",
    y = "Primary Energy Supply (EJ)",
    fill = "Energy Source"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    
    strip.background = element_rect(
      fill = "white",
      color = "black"
    ),
    
    strip.text = element_text(
      size = 14,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    axis.text = element_text(
      size = 11,
      color = "black"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    legend.key.size = unit(
      0.45, "cm"
    ),
    
    panel.spacing = unit(
      1.2, "lines"
    )
  ) +
  
  guides(
    fill = guide_legend(
      ncol = 4,
      byrow = TRUE,
      reverse = FALSE,
      override.aes = list(
        alpha = 1
      )
    )
  )


# ============================================================
# 13. DISPLAY
# ============================================================

print(p)


# ============================================================
# 14. SAVE FIGURE
# ============================================================

dir.create(
  "global/1102/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = paste0(
    "global/1102/figures/",
    "Primary_Energy_World_Calibration_Nolimit.png"
  ),
  plot = p,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)
