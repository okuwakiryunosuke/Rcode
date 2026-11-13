library(dplyr)
library(tidyr)
library(ggplot2)

# ==========================================
# 1. ファイルの場所を取得
# ==========================================

# 現在の作業ディレクトリ：prog
# prog以下からファイルを検索
find_csv <- function(filename) {
  
  files <- list.files(
    getwd(),
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  files <- files[
    basename(files) == filename
  ]
  
  if (length(files) != 1) {
    print(files)
    stop(paste(filename, "が1つに特定できません"))
  }
  
  return(files[1])
}

iea_file <- find_csv("Braz.csv")
iea_file <- "global/1112/supply_constraint/Braz.csv"
aim_file <- find_csv("BRA_Ref.csv")

# ==========================================
# 2. IEAデータ読み込み
# ==========================================

# Braz.csv は先頭2行がヘッダー情報
header_line <- readLines(
  iea_file,
  n = 1,
  warn = FALSE
)

iea_header <- as.character(
  unlist(
    read.csv(
      text = header_line,
      header = FALSE,
      check.names = FALSE
    )
  )
)

iea <- read.csv(
  iea_file,
  skip = 2,
  header = FALSE,
  col.names = c(
    "TIME", "FLOW", iea_header[-c(1, 2)]
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("..", "x", "")
)

# ==========================================
# 3. IEA 2005年 Total Energy Supply
# ==========================================

iea_2005 <- iea %>%
  filter(
    TIME == 2005,
    trimws(FLOW) == "Total energy supply"
  )

if (nrow(iea_2005) != 1) {
  stop("IEAの2005年Total energy supplyが見つかりません")
}

# TJ → EJ
# 石油は原油・NGL・石油製品を含む合計
iea_values <- c(
  Coal = as.numeric(
    iea_2005[["Coal and coal products"]]
  ),
  Biomass = as.numeric(
    iea_2005[["Biofuels and waste"]]
  ),
  Oil = as.numeric(
    iea_2005[["Memo: Primary and secondary oil"]]
  ),
  Gas = as.numeric(
    iea_2005[["Natural gas"]]
  )
) / 1e6

# ==========================================
# 4. AIM-Refデータ読み込み
# ==========================================

aim <- read.csv(
  aim_file,
  stringsAsFactors = FALSE
)

aim_2005 <- aim %>%
  filter(
    Sc == "Ref",
    Sr == "BRA",
    Sy == 2005
  )

# AIMの対象変数
aim_vars <- c(
  Coal = "Prm_Ene_Coa",
  Biomass = "Prm_Ene_Bio",
  Oil = "Prm_Ene_Oil",
  Gas = "Prm_Ene_Gas"
)

idx <- match(
  unname(aim_vars),
  aim_2005$Sv
)

if (anyNA(idx)) {
  stop("AIM-Refの対象変数が見つかりません")
}

aim_values <- aim_2005$Value[idx]
names(aim_values) <- names(aim_vars)

# ==========================================
# 5. 比較データ作成
# ==========================================

fuel_order <- c(
  "Coal", "Biomass", "Oil", "Gas"
)

comparison <- data.frame(
  Fuel = rep(fuel_order, 2),
  
  Source = rep(
    c("IEA Historical", "AIM-Ref"),
    each = 4
  ),
  
  Value = c(
    iea_values[fuel_order],
    aim_values[fuel_order]
  )
)

comparison <- comparison %>%
  mutate(
    Fuel = factor(
      Fuel,
      levels = fuel_order
    ),
    
    Source = factor(
      Source,
      levels = c("IEA Historical", "AIM-Ref")
    )
  )

# ==========================================
# 6. 差分データ作成
# ==========================================

comparison_wide <- data.frame(
  Fuel = fuel_order,
  IEA_EJ = unname(iea_values[fuel_order]),
  AIM_Ref_EJ = unname(aim_values[fuel_order])
) %>%
  mutate(
    Difference_EJ = AIM_Ref_EJ - IEA_EJ,
    Difference_PJ = Difference_EJ * 1000,
    Difference_pct = Difference_EJ / IEA_EJ * 100
  )

print(comparison_wide)

# ==========================================
# 7. 棒グラフ作成
# ==========================================

p <- ggplot(
  comparison,
  aes(
    x = Fuel,
    y = Value,
    fill = Source
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  
  geom_text(
    aes(label = sprintf("%.2f", Value)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 4
  ) +
  
  scale_fill_manual(
    values = c(
      "IEA Historical" = "#555555",
      "AIM-Ref" = "#2166AC"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  labs(
    title = "Brazil Primary Energy Supply (2005)",
    x = NULL,
    y = "Primary Energy Supply (EJ)",
    fill = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 8. PNG保存
# ==========================================

ggsave(
  "BRA_Primary_Energy_Comparison_2005.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# 9. CSV保存
# ==========================================

write.csv(
  comparison_wide,
  "BRA_Primary_Energy_Comparison_2005.csv",
  row.names = FALSE
)
































library(gdxrrw)
library(dplyr)
library(tidyr)

igdx()

# GDX読み込み
df_gdx <- rgdx.param(
  "global/1112/supply_constraint/merged_output.gdx",
  "iamc_gdx"
)

colnames(df_gdx)[1:5] <- c("Sc", "Sr", "Sv", "Sy", "Value")

# BRAの産業部門バイオマス
bio_data <- df_gdx %>%
  filter(
    Sc %in% c("Ref", "Import"),
    Sr == "BRA",
    Sv %in% c(
      "Fin_Ene_Ind_SolidsBio",
      "Fin_Ene_Ind_Liq_Bio"
    )
  ) %>%
  mutate(
    Sy = as.numeric(as.character(Sy)),
    Value = as.numeric(as.character(Value))
  ) %>%
  group_by(Sc, Sy) %>%
  summarise(
    Biomass_EJ = sum(Value),
    .groups = "drop"
  )

# RefとImportの差分
comparison <- bio_data %>%
  pivot_wider(
    names_from = Sc,
    values_from = Biomass_EJ
  ) %>%
  mutate(
    Difference_EJ = Import - Ref,
    Difference_PJ = Difference_EJ * 1000,
    Difference_pct = Difference_EJ / Ref * 100
  ) %>%
  arrange(Sy)

# 全年度の比較
print(as.data.frame(comparison), row.names = FALSE)

# 2005年だけ表示
cat("\n=== BRA 2005 Industrial Biomass ===\n")
print(
  as.data.frame(
    comparison %>% filter(Sy == 2005)
  ),
  row.names = FALSE
)

# CSV保存
write.csv(
  comparison,
  "BRA_Industrial_Biomass_Ref_Import.csv",
  row.names = FALSE
)



library(ggplot2)
library(dplyr)

# ==========================================
# 1. グラフ用データ
# ==========================================

plot_data <- bio_data %>%
  filter(Sy %in% c(2005, 2010, 2015, 2020)) %>%
  mutate(
    Sy = factor(Sy, levels = c(2005, 2010, 2015, 2020)),
    Sc = factor(Sc, levels = c("Ref", "Import"))
  )

if (nrow(plot_data) == 0) {
  stop("対象データがありません")
}

# ==========================================
# 2. 棒グラフ
# ==========================================

p <- ggplot(
  plot_data,
  aes(
    x = Sy,
    y = Biomass_EJ,
    fill = Sc
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Biomass_EJ)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "Ref" = "#2166AC",
      "Import" = "#D6604D"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "Brazil Industrial Biomass Consumption",
    subtitle = "Ref vs Import (2005–2020)",
    x = "Year",
    y = "Final Energy Consumption (EJ)",
    fill = "Scenario"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 3. PNG保存
# ==========================================

ggsave(
  "BRA_Industrial_Biomass_Ref_Import.png",
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 300
)






library(dplyr)
library(ggplot2)

# ==========================================
# 1. 製鉄部門のデータ抽出
# ==========================================

steel_data <- df_gdx %>%
  filter(
    Sr == "BRA",
    Sc %in% c("Ref", "Import"),
    Sv %in% c(
      "Fin_Ene_Ind_Ste_Solids",
      "Fin_Ene_Ind_Ste_SolidsBioEne"
    ),
    as.numeric(as.character(Sy)) %in%
      c(2005, 2010, 2015, 2020)
  ) %>%
  mutate(
    Sy = factor(
      as.character(Sy),
      levels = c("2005", "2010", "2015", "2020")
    ),
    Sc = factor(
      Sc,
      levels = c("Ref", "Import")
    ),
    Sv = factor(
      Sv,
      levels = c(
        "Fin_Ene_Ind_Ste_Solids",
        "Fin_Ene_Ind_Ste_SolidsBioEne"
      ),
      labels = c(
        "Solid Fuels",
        "Solid Biomass"
      )
    ),
    Value = as.numeric(as.character(Value))
  )

if (nrow(steel_data) == 0) {
  stop("BRAの製鉄部門データが見つかりません")
}

# ==========================================
# 2. 棒グラフ
# ==========================================

p <- ggplot(
  steel_data,
  aes(
    x = Sy,
    y = Value,
    fill = Sc
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Value)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.5
  ) +
  facet_wrap(
    ~ Sv,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Ref" = "#2166AC",
      "Import" = "#D6604D"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Brazil Steel Sector Energy Consumption",
    subtitle = "Ref vs Import (2005–2020)",
    x = "Year",
    y = "Final Energy Consumption (EJ)",
    fill = "Scenario"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 3. PNG・CSV保存
# ==========================================

ggsave(
  "BRA_Steel_Solids_Biomass_Ref_Import.png",
  plot = p,
  width = 11,
  height = 5.5,
  dpi = 300
)

write.csv(
  steel_data,
  "BRA_Steel_Solids_Biomass_Ref_Import.csv",
  row.names = FALSE
)























library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)

igdx()

# ==========================================
# 1. ファイルパス
# ==========================================

base_dir <- "global/1112/supply_constraint"

iea_file <- file.path(base_dir, "Braz.csv")
gdx_file <- file.path(base_dir, "merged_output.gdx")

stopifnot(file.exists(iea_file))
stopifnot(file.exists(gdx_file))

years <- c(2005, 2010, 2015, 2020)

# ==========================================
# 2. IEAデータ読み込み
# ==========================================

# Braz.csvの1行目にあるPRODUCT名を取得
header <- read.csv(
  iea_file,
  header = FALSE,
  nrows = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

product_names <- as.character(
  unlist(header[1, 3:ncol(header)])
)

# 3行目以降が実際のデータ
iea <- read.csv(
  iea_file,
  skip = 2,
  header = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("..", "x", "")
)

colnames(iea) <- c(
  "TIME", "FLOW", product_names
)

# ==========================================
# 3. IEA Iron and steel抽出
# ==========================================

iea_data <- iea %>%
  mutate(
    TIME = as.numeric(TIME),
    FLOW = trimws(FLOW)
  ) %>%
  filter(
    TIME %in% years,
    FLOW == "Iron and steel"
  ) %>%
  select(
    TIME,
    `Coal and coal products`,
    `Biofuels and waste`
  ) %>%
  pivot_longer(
    cols = -TIME,
    names_to = "Fuel",
    values_to = "Value"
  ) %>%
  mutate(
    Sc = "IEA Historical",
    Sy = TIME,
    Fuel = recode(
      Fuel,
      "Coal and coal products" = "Coal",
      "Biofuels and waste" = "Biomass"
    ),
    # IEAはTJ → PJ
    Value_PJ = as.numeric(Value) / 1000
  ) %>%
  select(Sc, Sy, Fuel, Value_PJ)

# ==========================================
# 4. AIM GDX読み込み
# ==========================================

df_gdx <- rgdx.param(
  gdx_file,
  "iamc_gdx"
)

colnames(df_gdx)[1:5] <- c(
  "Sc", "Sr", "Sv", "Sy", "Value"
)

# ==========================================
# 5. AIM製鉄部門抽出
# ==========================================

aim_data <- df_gdx %>%
  filter(
    Sc %in% c("Ref", "Import"),
    Sr == "BRA",
    Sv %in% c(
      "Fin_Ene_Ind_Ste_SolidsFos",
      "Fin_Ene_Ind_Ste_SolidsBioEne"
    )
  ) %>%
  mutate(
    Sy = as.numeric(as.character(Sy)),
    Value = as.numeric(as.character(Value))
  ) %>%
  filter(Sy %in% years) %>%
  transmute(
    Sc = Sc,
    Sy = Sy,
    Fuel = case_when(
      Sv == "Fin_Ene_Ind_Ste_SolidsFos" ~ "Coal",
      Sv == "Fin_Ene_Ind_Ste_SolidsBioEne" ~ "Biomass"
    ),
    # AIMはEJ → PJ
    Value_PJ = Value * 1000
  )

# ==========================================
# 6. データ結合・確認
# ==========================================

plot_data <- bind_rows(
  iea_data,
  aim_data
) %>%
  mutate(
    Sc = factor(
      Sc,
      levels = c(
        "IEA Historical",
        "Ref",
        "Import"
      )
    ),
    Fuel = factor(
      Fuel,
      levels = c("Coal", "Biomass")
    ),
    Sy = factor(
      Sy,
      levels = years
    )
  )

# 各年・各燃料で3系列そろっているか確認
check_data <- plot_data %>%
  count(Sy, Fuel, Sc)

if (
  nrow(check_data) != length(years) * 2 * 3 ||
  any(check_data$n != 1) ||
  any(is.na(plot_data$Value_PJ))
) {
  print(as.data.frame(check_data))
  stop("比較データに欠損または重複があります")
}

print(
  as.data.frame(plot_data),
  row.names = FALSE
)

# ==========================================
# 7. 棒グラフ作成
# ==========================================

p <- ggplot(
  plot_data,
  aes(
    x = Sy,
    y = Value_PJ,
    fill = Sc
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72
  ) +
  
  geom_text(
    aes(label = sprintf("%.1f", Value_PJ)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.3
  ) +
  
  facet_wrap(
    ~ Fuel,
    ncol = 2,
    scales = "free_y"
  ) +
  
  scale_fill_manual(
    values = c(
      "IEA Historical" = "#555555",
      "Ref" = "#2166AC",
      "Import" = "#D6604D"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  labs(
    title = "Brazil Steel Sector Energy Consumption",
    subtitle = "IEA Historical vs AIM Ref and Import",
    x = "Year",
    y = "Final Energy Consumption (PJ)",
    fill = "Scenario"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 8. 差分表作成
# ==========================================

comparison <- plot_data %>%
  mutate(
    Sc = as.character(Sc),
    Fuel = as.character(Fuel),
    Sy = as.numeric(as.character(Sy))
  ) %>%
  pivot_wider(
    names_from = Sc,
    values_from = Value_PJ
  ) %>%
  mutate(
    Ref_minus_IEA = Ref - `IEA Historical`,
    Import_minus_IEA = Import - `IEA Historical`,
    Import_minus_Ref = Import - Ref
  ) %>%
  arrange(Fuel, Sy)

print(
  as.data.frame(comparison),
  row.names = FALSE
)

# ==========================================
# 9. PNG・CSV保存
# ==========================================

ggsave(
  "BRA_Steel_Coal_Biomass_IEA_AIM.png",
  plot = p,
  width = 11,
  height = 5.5,
  dpi = 300
)

write.csv(
  comparison,
  "BRA_Steel_Coal_Biomass_IEA_AIM.csv",
  row.names = FALSE
)






















library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)

igdx()

# ==========================================
# 1. ファイル読み込み
# ==========================================

base_dir <- "global/1112/supply_constraint"

iea_file <- file.path(base_dir, "Braz.csv")
gdx_file <- file.path(base_dir, "merged_output.gdx")

# ==========================================
# 2. IEAデータ読み込み
# ==========================================

header <- read.csv(
  iea_file,
  header = FALSE,
  nrows = 1,
  check.names = FALSE
)

product_names <- as.character(
  unlist(header[1, 3:ncol(header)])
)

iea <- read.csv(
  iea_file,
  skip = 2,
  header = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("..", "x", "")
)

colnames(iea) <- c("TIME", "FLOW", product_names)

# IEA：2005年・製鉄部門
iea_data <- iea %>%
  mutate(
    TIME = as.numeric(TIME),
    FLOW = trimws(FLOW)
  ) %>%
  filter(
    TIME == 2005,
    FLOW == "Iron and steel"
  ) %>%
  select(
    `Coal and coal products`,
    `Biofuels and waste`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Fuel",
    values_to = "Value"
  ) %>%
  transmute(
    Source = "IEA Historical",
    Fuel = recode(
      Fuel,
      "Coal and coal products" = "Coal",
      "Biofuels and waste" = "Biomass"
    ),
    Value_PJ = as.numeric(Value) / 1000
  )

# ==========================================
# 3. AIM-Import読み込み
# ==========================================

df_gdx <- rgdx.param(
  gdx_file,
  "iamc_gdx"
)

colnames(df_gdx)[1:5] <- c(
  "Sc", "Sr", "Sv", "Sy", "Value"
)

aim_data <- df_gdx %>%
  filter(
    Sc == "Import",
    Sr == "BRA",
    as.numeric(as.character(Sy)) == 2005,
    Sv %in% c(
      "Fin_Ene_Ind_Ste_SolidsFos",
      "Fin_Ene_Ind_Ste_SolidsBioEne"
    )
  ) %>%
  transmute(
    Source = "AIM-Import",
    Fuel = case_when(
      Sv == "Fin_Ene_Ind_Ste_SolidsFos" ~ "Coal",
      Sv == "Fin_Ene_Ind_Ste_SolidsBioEne" ~ "Biomass"
    ),
    Value_PJ = as.numeric(Value) * 1000
  )

# ==========================================
# 4. データ結合
# ==========================================

plot_data <- bind_rows(
  iea_data,
  aim_data
) %>%
  mutate(
    Fuel = factor(
      Fuel,
      levels = c("Coal", "Biomass")
    ),
    Source = factor(
      Source,
      levels = c("IEA Historical", "AIM-Import")
    )
  )

if (nrow(plot_data) != 4 ||
    anyNA(plot_data$Value_PJ) ||
    any(table(plot_data$Fuel, plot_data$Source) != 1)) {
  print(plot_data)
  stop("IEAまたはAIMのデータに欠損・重複があります")
}

print(as.data.frame(plot_data), row.names = FALSE)

# ==========================================
# 5. 棒グラフ
# ==========================================

p <- ggplot(
  plot_data,
  aes(
    x = Fuel,
    y = Value_PJ,
    fill = Source
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = sprintf("%.1f", Value_PJ)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "IEA Historical" = "#555555",
      "AIM-Import" = "#D6604D"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Brazil Steel Sector Energy Consumption (2005)",
    x = NULL,
    y = "Final Energy Consumption (PJ)",
    fill = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 6. 差分と保存
# ==========================================

comparison <- plot_data %>%
  mutate(
    Fuel = as.character(Fuel),
    Source = as.character(Source)
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = Value_PJ
  ) %>%
  mutate(
    Difference_PJ = `AIM-Import` - `IEA Historical`,
    Difference_pct =
      Difference_PJ / `IEA Historical` * 100
  )

print(as.data.frame(comparison), row.names = FALSE)

ggsave(
  "BRA_Steel_Coal_Biomass_2005.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

write.csv(
  comparison,
  "BRA_Steel_Coal_Biomass_2005.csv",
  row.names = FALSE
)













# ==========================================
# 6. 単位変更
# ==========================================

plot_data <- plot_data %>%
  mutate(
    Value_plot = ifelse(
      Sector == "Industry Total",
      Value_PJ / 1000,
      Value_PJ
    ),
    Sector = factor(
      as.character(Sector),
      levels = c("Steel", "Industry Total"),
      labels = c("Steel (PJ)", "Industry Total (EJ)")
    )
  )

# ==========================================
# 7. 棒グラフ作成
# ==========================================

p <- ggplot(
  plot_data,
  aes(
    x = Fuel,
    y = Value_plot,
    fill = Source
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = ifelse(
        Sector == "Industry Total (EJ)",
        sprintf("%.3f", Value_plot),
        sprintf("%.1f", Value_plot)
      )
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.5
  ) +
  facet_wrap(
    ~ Sector,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "IEA Historical" = "#555555",
      "AIM-Import" = "#D6604D"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Brazil Industrial Energy Consumption (2005)",
    subtitle = "IEA Historical vs AIM-Import",
    x = NULL,
    y = "Final Energy Consumption",
    fill = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==========================================
# 8. PNG保存
# ==========================================

ggsave(
  "BRA_Steel_PJ_Industry_EJ_2005.png",
  plot = p,
  width = 11,
  height = 5.5,
  dpi = 300
)



























library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)

igdx()

# ==================================================
# 1. ファイル設定
# ==================================================

base_dir <- "global/1112/supply_constraint"

iea_file <- file.path(base_dir, "Braz.csv")
gdx_file <- file.path(base_dir, "merged_output.gdx")

stopifnot(file.exists(iea_file))
stopifnot(file.exists(gdx_file))

# ==================================================
# 2. IEAデータ読み込み
# ==================================================

header <- read.csv(
  iea_file,
  header = FALSE,
  nrows = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

product_names <- as.character(
  unlist(header[1, 3:ncol(header)])
)

iea <- read.csv(
  iea_file,
  skip = 2,
  header = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("..", "x", "")
)

if (ncol(iea) != length(product_names) + 2) {
  stop("Braz.csvのヘッダー構造を確認してください")
}

colnames(iea) <- c("TIME", "FLOW", product_names)

# 数値変換用
to_num <- function(x) {
  as.numeric(gsub(",", "", as.character(x)))
}

# IEAの燃料列を確認
print(colnames(iea))

coal_col <- "Coal and coal products"
bio_col  <- "Biofuels and waste"

if (!all(c(coal_col, bio_col) %in% names(iea))) {
  stop("IEAの石炭・バイオマス列が見つかりません")
}

# 2005年のみ
iea_2005 <- iea %>%
  mutate(
    TIME = to_num(TIME),
    FLOW = trimws(as.character(FLOW))
  ) %>%
  filter(TIME == 2005)

# 産業全体のFLOW名を検出
industry_candidates <- c(
  "Industry",
  "Total industry",
  "Industry (including non-energy use)"
)

industry_flow <- industry_candidates[
  industry_candidates %in% iea_2005$FLOW
][1]

if (is.na(industry_flow)) {
  print(unique(iea_2005$FLOW))
  stop("産業全体のIEA FLOW名が見つかりません")
}

if (!"Iron and steel" %in% iea_2005$FLOW) {
  print(unique(iea_2005$FLOW))
  stop("Iron and steelが見つかりません")
}

# IEAデータ整形
iea_data <- iea_2005 %>%
  filter(
    FLOW %in% c("Iron and steel", industry_flow)
  ) %>%
  select(
    FLOW,
    all_of(c(coal_col, bio_col))
  ) %>%
  pivot_longer(
    cols = -FLOW,
    names_to = "Fuel",
    values_to = "Value"
  ) %>%
  transmute(
    Sector = ifelse(
      FLOW == "Iron and steel",
      "Steel",
      "Industry Total"
    ),
    Source = "IEA",
    Fuel = ifelse(
      Fuel == coal_col,
      "Coal",
      "Biomass"
    ),
    Value_PJ = to_num(Value) / 1000
  ) %>%
  group_by(Sector, Source, Fuel) %>%
  summarise(
    Value_PJ = sum(Value_PJ),
    .groups = "drop"
  )

# ==================================================
# 3. AIMデータ読み込み
# ==================================================

df_gdx <- rgdx.param(
  gdx_file,
  "iamc_gdx"
)

colnames(df_gdx)[1:5] <- c(
  "Sc", "Sr", "Sv", "Sy", "Value"
)

aim_raw <- df_gdx %>%
  mutate(
    Sy = to_num(Sy),
    Value = to_num(Value)
  ) %>%
  filter(
    Sc == "Import",
    Sr == "BRA",
    Sy == 2005
  )

# ==================================================
# 4. AIM変数の確認
# ==================================================

# 以前のIAMC出力で使用した変数
required_vars <- c(
  "Fin_Ene_Ind_Ste_Solids",
  "Fin_Ene_Ind_Ste_SolidsBioEne",
  "Fin_Ene_Ind_Solids",
  "Fin_Ene_Ind_SolidsBio",
  "Fin_Ene_Ind_Liq_Bio"
)

missing_vars <- setdiff(
  required_vars,
  unique(aim_raw$Sv)
)

if (length(missing_vars) > 0) {
  
  cat("\n見つからないAIM変数:\n")
  print(missing_vars)
  
  cat("\n利用可能な関連変数:\n")
  print(
    sort(unique(
      aim_raw$Sv[
        grepl("Fin_Ene_Ind.*(Solids|Bio)", aim_raw$Sv)
      ]
    ))
  )
  
  stop("AIM変数名を確認してください")
}

# 同一変数の重複を検出
aim_check <- aim_raw %>%
  filter(Sv %in% required_vars) %>%
  count(Sv)

if (any(aim_check$n != 1)) {
  print(aim_check)
  stop("AIM変数に重複があります")
}

# 変数の値を取得する関数
get_aim <- function(variable) {
  aim_raw$Value[match(variable, aim_raw$Sv)]
}

# ==================================================
# 5. AIMの石炭・バイオマス消費量
# ==================================================

# Steel
steel_solids <- get_aim(
  "Fin_Ene_Ind_Ste_Solids"
)

steel_bio <- get_aim(
  "Fin_Ene_Ind_Ste_SolidsBioEne"
)

steel_coal <- steel_solids - steel_bio

# Industry Total
industry_solids <- get_aim(
  "Fin_Ene_Ind_Solids"
)

industry_bio_solid <- get_aim(
  "Fin_Ene_Ind_SolidsBio"
)

industry_bio_liquid <- get_aim(
  "Fin_Ene_Ind_Liq_Bio"
)

industry_coal <- industry_solids - industry_bio_solid

industry_bio <- industry_bio_solid + industry_bio_liquid

# AIMはEJのため、PJに変換
aim_data <- data.frame(
  Sector = c(
    "Steel", "Steel",
    "Industry Total", "Industry Total"
  ),
  Source = "AIM-Import",
  Fuel = c(
    "Coal", "Biomass",
    "Coal", "Biomass"
  ),
  Value_PJ = c(
    steel_coal,
    steel_bio,
    industry_coal,
    industry_bio
  ) * 1000
)

# ==================================================
# 6. データ結合・単位変更
# ==================================================

plot_data <- bind_rows(
  iea_data,
  aim_data
) %>%
  mutate(
    Value_plot = ifelse(
      Sector == "Industry Total",
      Value_PJ / 1000,
      Value_PJ
    ),
    Sector = factor(
      Sector,
      levels = c("Steel", "Industry Total"),
      labels = c(
        "Steel (PJ)",
        "Industry Total (EJ)"
      )
    ),
    Fuel = factor(
      Fuel,
      levels = c("Coal", "Biomass")
    ),
    Source = factor(
      Source,
      levels = c("IEA", "AIM-Import")
    )
  )

# データが8行あることを確認
print(as.data.frame(plot_data), row.names = FALSE)

check_data <- plot_data %>%
  count(Sector, Fuel, Source)

if (
  nrow(check_data) != 8 ||
  any(check_data$n != 1) ||
  anyNA(plot_data$Value_plot)
) {
  stop("IEAまたはAIMのデータが不足・重複しています")
}

# ==================================================
# 7. グラフ
# ==================================================

p <- ggplot(
  plot_data,
  aes(
    x = Fuel,
    y = Value_plot,
    fill = Source
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        Sector == "Industry Total (EJ)",
        sprintf("%.3f", Value_plot),
        sprintf("%.1f", Value_plot)
      )
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.8
  ) +
  
  facet_wrap(
    ~ Sector,
    ncol = 2,
    scales = "free_y"
  ) +
  
  scale_fill_manual(
    values = c(
      "IEA" = "#555555",
      "AIM-Import" = "#D6604D"
    ),
    drop = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  
  labs(
    title = "Brazil Industrial Energy Consumption (2005)",
    subtitle = "IEA vs AIM-Import",
    x = NULL,
    y = "Energy Consumption (PJ / EJ)",
    fill = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==================================================
# 8. 差分表作成
# ==================================================

comparison <- plot_data %>%
  mutate(
    Sector = as.character(Sector),
    Fuel = as.character(Fuel),
    Source = as.character(Source)
  ) %>%
  select(
    Sector,
    Fuel,
    Source,
    Value_plot
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = Value_plot
  ) %>%
  mutate(
    Difference = `AIM-Import` - IEA,
    Difference_pct = Difference / IEA * 100
  )

print(as.data.frame(comparison), row.names = FALSE)

# ==================================================
# 9. 保存
# ==================================================

ggsave(
  filename = "BRA_2005_Steel_Industry_IEA_Import.png",
  plot = p,
  width = 11,
  height = 5.5,
  dpi = 300
)

write.csv(
  comparison,
  "BRA_2005_Steel_Industry_IEA_Import.csv",
  row.names = FALSE
)














# ==================================================
# AIMの石炭・バイオマス消費量（EJ）
# ==================================================

get_aim <- function(variable) {
  aim_raw$Value[match(variable, aim_raw$Sv)]
}

# 製鉄部門
steel_coal <- get_aim("Fin_Ene_Ind_Ste_SolidsFos")
steel_bio  <- get_aim("Fin_Ene_Ind_Ste_SolidsBioEne")

# 産業全体
industry_coal <- get_aim("Fin_Ene_Ind_SolidsCoa")
industry_bio  <- get_aim("Fin_Ene_Ind_SolidsBio")

# PJに統一（後で産業全体のみEJ表示）
aim_data <- data.frame(
  Sector = c(
    "Steel", "Steel",
    "Industry Total", "Industry Total"
  ),
  Source = "AIM-Import",
  Fuel = c(
    "Coal", "Biomass",
    "Coal", "Biomass"
  ),
  Value_PJ = c(
    steel_coal,
    steel_bio,
    industry_coal,
    industry_bio
  ) * 1000
)













# ==================================================
# 6. IEAとAIMを結合・単位変換
# ==================================================

plot_data <- bind_rows(
  iea_data,
  aim_data
) %>%
  mutate(
    Value_plot = ifelse(
      Sector == "Industry Total",
      Value_PJ / 1000,
      Value_PJ
    ),
    Sector = factor(
      Sector,
      levels = c("Steel", "Industry Total"),
      labels = c("Steel (PJ)", "Industry Total (EJ)")
    ),
    Fuel = factor(
      Fuel,
      levels = c("Coal", "Biomass")
    ),
    Source = factor(
      Source,
      levels = c("IEA", "AIM-Import")
    )
  )

print(as.data.frame(plot_data))

# ==================================================
# 7. グラフ作成
# ==================================================

p <- ggplot(
  plot_data,
  aes(
    x = Fuel,
    y = Value_plot,
    fill = Source
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = ifelse(
        Sector == "Industry Total (EJ)",
        sprintf("%.3f", Value_plot),
        sprintf("%.1f", Value_plot)
      )
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.8
  ) +
  facet_wrap(
    ~ Sector,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "IEA" = "#555555",
      "AIM-Import" = "#D6604D"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Brazil Industrial Energy Consumption (2005)",
    subtitle = "IEA Historical vs AIM-Import",
    x = NULL,
    y = "Energy Consumption (PJ / EJ)",
    fill = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    ),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p)

# ==================================================
# 8. 差分表
# ==================================================

comparison <- plot_data %>%
  mutate(
    Sector = as.character(Sector),
    Fuel = as.character(Fuel),
    Source = as.character(Source)
  ) %>%
  select(
    Sector,
    Fuel,
    Source,
    Value_plot
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = Value_plot
  ) %>%
  mutate(
    Difference = `AIM-Import` - IEA,
    Difference_pct = Difference / IEA * 100
  )

print(as.data.frame(comparison))

# ==================================================
# 9. PNG・CSV保存
# ==================================================

ggsave(
  "BRA_2005_Steel_Industry_IEA_Import.png",
  plot = p,
  width = 11,
  height = 5.5,
  dpi = 300
)

write.csv(
  comparison,
  "BRA_2005_Steel_Industry_IEA_Import.csv",
  row.names = FALSE
)
