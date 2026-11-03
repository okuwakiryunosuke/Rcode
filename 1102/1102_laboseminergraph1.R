
# ============================================================
# Japan 2050 Primary Energy Shannon Diversity
# 81 scenarios + Ref
# Secondary Energy Trade included as independent category
# R 4.3.3
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gdxrrw)

# ============================================================
# 1. 基本設定
# ============================================================

setwd("C:/Users/okuwa/OneDrive - Kyoto University/prog")

igdx("C:/GAMS/54")

gdx_file <- "global/0901/merged_output.gdx"

target_year <- 2050
target_region <- "JPN"

output_dir <- "global/0901/Shannon_diversity_2050"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 2. シナリオ一覧
# ============================================================

scenario_grid <- tidyr::expand_grid(
  N = c("Nh", "Nm", "NL"),
  V = c("Vh", "Vm", "VL"),
  C = c("Ch", "Cm", "CL"),
  D = c("Dh", "Dm", "DL")
) |>
  mutate(
    Sc = paste(N, V, C, D, sep = "_")
  )

scenario_order <- c("Ref", scenario_grid$Sc)

default_scenario <- "Nm_Vm_Cm_Dm"

# ============================================================
# 3. 燃料分類
# ============================================================

fuel_mapping <- c(
  "Prm_Ene_Oil_wo_CCS" = "Oil",
  "Prm_Ene_Oil_w_CCS"  = "Oil",
  
  "Prm_Ene_Coa_wo_CCS" = "Coal",
  "Prm_Ene_Coa_w_CCS"  = "Coal",
  
  "Prm_Ene_Gas_wo_CCS" = "Gas",
  "Prm_Ene_Gas_w_CCS"  = "Gas",
  
  "Prm_Ene_Nuc"        = "Nuclear",
  "Prm_Ene_Hyp"        = "Hydro",
  
  "Prm_Ene_Bio_wo_CCS" = "Biomass",
  "Prm_Ene_Bio_w_CCS"  = "Biomass",
  
  "Prm_Ene_Geo"        = "Other",
  "Prm_Ene_Solar"      = "Solar",
  "Prm_Ene_Win"        = "Wind",
  "Prm_Ene_Oce"        = "Ocean",
  
  # 今回追加する分類
  "Prm_Ene_Sec_Ene_Trd" = "Secondary Energy Trade"
)

fuel_order <- unique(unname(fuel_mapping))

# ============================================================
# 4. GDXデータ読み込み
# ============================================================

merged_gdx <- rgdx.param(
  gdx_file,
  "iamc_gdx"
)

df <- merged_gdx |>
  mutate(
    Sc = as.character(Sc),
    Sr = as.character(Sr),
    Sv = as.character(Sv),
    Y5 = as.integer(
      str_extract(as.character(Y5), "[0-9]{4}")
    ),
    EJ = as.numeric(iamc_gdx)
  ) |>
  filter(
    Sr == target_region,
    Y5 == target_year,
    Sc %in% scenario_order
  )

# ============================================================
# 5. 変数・シナリオ存在確認
# ============================================================

missing_variables <- setdiff(
  names(fuel_mapping),
  unique(df$Sv)
)

if (length(missing_variables) > 0) {
  message(
    "データに存在しない燃料変数: ",
    paste(missing_variables, collapse = ", ")
  )
}

# 一次エネルギー総量
primary_total <- df |>
  filter(Sv == "Prm_Ene") |>
  group_by(Sc) |>
  summarise(
    Total_EJ = sum(EJ, na.rm = TRUE),
    .groups = "drop"
  )

missing_scenarios <- setdiff(
  scenario_order,
  primary_total$Sc
)

if (length(missing_scenarios) > 0) {
  stop(
    "一次エネルギー総量が存在しないシナリオ: ",
    paste(missing_scenarios, collapse = ", ")
  )
}

if (any(!is.finite(primary_total$Total_EJ)) ||
    any(primary_total$Total_EJ <= 0)) {
  stop("Prm_Eneに無効な総量があります。")
}

# ============================================================
# 6. 燃料別一次エネルギー供給量
# ============================================================

fuel_supply_raw <- df |>
  filter(Sv %in% names(fuel_mapping)) |>
  mutate(
    Fuel = unname(fuel_mapping[Sv])
  ) |>
  group_by(Sc, Fuel) |>
  summarise(
    Supply_EJ = sum(EJ, na.rm = TRUE),
    .groups = "drop"
  )

# データに存在しない燃料を0として補完
fuel_supply <- tidyr::expand_grid(
  Sc = scenario_order,
  Fuel = fuel_order
) |>
  left_join(
    fuel_supply_raw,
    by = c("Sc", "Fuel")
  ) |>
  mutate(
    Supply_EJ = replace_na(Supply_EJ, 0)
  ) |>
  left_join(
    primary_total,
    by = "Sc"
  )

# ============================================================
# 7. 負の供給量チェック
# ============================================================

negative_supply <- fuel_supply |>
  filter(Supply_EJ < -1e-8)

if (nrow(negative_supply) > 0) {
  
  print(negative_supply, n = Inf)
  
  stop(
    "負の供給量が存在します。",
    "特に二次エネルギー貿易の純輸出を確認してください。"
  )
}

# 微小な数値誤差のみゼロに補正
fuel_supply <- fuel_supply |>
  mutate(
    Supply_EJ = pmax(Supply_EJ, 0)
  )

# ============================================================
# 8. エネルギーバランス確認
# ============================================================

balance_check <- fuel_supply |>
  group_by(Sc) |>
  summarise(
    Components_EJ = sum(Supply_EJ),
    Total_EJ = first(Total_EJ),
    Difference_EJ = Components_EJ - Total_EJ,
    Difference_pct = 100 * Difference_EJ / Total_EJ,
    .groups = "drop"
  )

print(
  balance_check |>
    arrange(desc(abs(Difference_pct))) |>
    head(15)
)

write.csv(
  balance_check,
  file.path(output_dir, "Energy_balance_check.csv"),
  row.names = FALSE
)

# 誤差2%以上なら計算中止
if (any(abs(balance_check$Difference_pct) > 2)) {
  stop(
    "燃料別合計とPrm_Eneの差が2%を超えています。",
    "分類漏れ、貿易計上、重複集計を確認してください。"
  )
}

# ============================================================
# 9. 燃料構成比
# ============================================================

fuel_share <- fuel_supply |>
  mutate(
    Share = Supply_EJ / Total_EJ
  )

# ============================================================
# 10. Shannon指数
# ============================================================

# Shannon計算関数
calculate_shannon <- function(p) {
  p <- p[p > 0]
  -sum(p * log(p))
}

# Prm_Ene基準のShannon指数
shannon_result <- fuel_share |>
  group_by(Sc) |>
  summarise(
    Shannon = calculate_shannon(Share),
    Total_EJ = first(Total_EJ),
    Share_Sum = sum(Share),
    .groups = "drop"
  )

# 構成比合計が1からずれる場合も
# Prm_Eneを分母として固定して計算
# ただし2%超の誤差は上で停止済み

# ============================================================
# 11. シナリオ属性付与
# ============================================================

shannon_result <- shannon_result |>
  left_join(
    scenario_grid,
    by = "Sc"
  ) |>
  mutate(
    Type = if_else(
      Sc == "Ref",
      "Reference",
      "Scenario"
    ),
    Is_Default = Sc == default_scenario
  )

# 順序設定
shannon_result <- shannon_result |>
  mutate(
    Sc = factor(Sc, levels = scenario_order),
    
    N = factor(N, levels = c("Nh", "Nm", "NL")),
    V = factor(V, levels = c("Vh", "Vm", "VL")),
    C = factor(C, levels = c("Ch", "Cm", "CL")),
    D = factor(D, levels = c("Dh", "Dm", "DL"))
  )

# ============================================================
# 12. 結果確認
# ============================================================

print(
  shannon_result |>
    arrange(desc(Shannon)) |>
    select(Sc, Shannon, Total_EJ, Share_Sum),
  n = Inf
)

# 最大・最小・平均
shannon_summary <- shannon_result |>
  filter(Type == "Scenario") |>
  summarise(
    Mean = mean(Shannon),
    Median = median(Shannon),
    SD = sd(Shannon),
    Minimum = min(Shannon),
    Maximum = max(Shannon)
  )

print(shannon_summary)

# ============================================================
# 13. CSV保存
# ============================================================

write.csv(
  shannon_result,
  file.path(output_dir, "Shannon_2050_81scenarios.csv"),
  row.names = FALSE
)

write.csv(
  fuel_share,
  file.path(output_dir, "Fuel_share_2050.csv"),
  row.names = FALSE
)

write.csv(
  fuel_supply,
  file.path(output_dir, "Fuel_supply_2050.csv"),
  row.names = FALSE
)

write.csv(
  shannon_summary,
  file.path(output_dir, "Shannon_summary.csv"),
  row.names = FALSE
)

# ============================================================
# 14. グラフ共通設定
# ============================================================

theme_shannon <- theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )

plot_data <- shannon_result |>
  filter(Type == "Scenario")

# デフォルトシナリオのShannon指数
default_H <- shannon_result |>
  filter(Is_Default) |>
  pull(Shannon)

# RefのShannon指数
ref_H <- shannon_result |>
  filter(Sc == "Ref") |>
  pull(Shannon)

# ============================================================
# 15. 4要素別Shannon分布図
# ============================================================

make_factor_plot <- function(data, factor_name) {
  
  ggplot(
    data,
    aes(
      x = .data[[factor_name]],
      y = Shannon
    )
  ) +
    geom_boxplot(
      width = 0.48,
      outlier.shape = NA,
      fill = "grey85",
      color = "grey30"
    ) +
    geom_jitter(
      width = 0.14,
      height = 0,
      alpha = 0.65,
      size = 2
    ) +
    geom_hline(
      yintercept = default_H,
      linetype = "dashed",
      color = "red",
      linewidth = 0.8
    ) +
    labs(
      title = paste0(
        "Shannon Diversity by ", factor_name,
        " (Japan, 2050)"
      ),
      x = factor_name,
      y = "Shannon Diversity Index"
    ) +
    theme_shannon
}

factor_names <- c("N", "V", "C", "D")

for (factor_name in factor_names) {
  
  p <- make_factor_plot(
    plot_data,
    factor_name
  )
  
  print(p)
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0("Shannon_by_", factor_name, ".png")
    ),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

# ============================================================
# 16. 81シナリオのShannon指数ランキング
# ============================================================

ranking_data <- plot_data |>
  arrange(Shannon) |>
  mutate(
    Sc_rank = factor(
      as.character(Sc),
      levels = as.character(Sc)
    )
  )

p_ranking <- ggplot(
  ranking_data,
  aes(
    x = Sc_rank,
    y = Shannon
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = default_H,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Shannon Diversity across 81 Scenarios",
    x = "Scenarios (ascending Shannon index)",
    y = "Shannon Diversity Index"
  ) +
  theme_shannon +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p_ranking)

ggsave(
  file.path(output_dir, "Shannon_81scenarios.png"),
  p_ranking,
  width = 10,
  height = 5,
  dpi = 300
)

# ============================================================
# 17. デフォルトシナリオのエネルギー構成比
# ============================================================

default_fuel <- fuel_share |>
  filter(Sc == default_scenario) |>
  mutate(
    Fuel = factor(
      Fuel,
      levels = fuel_order
    )
  )

p_fuel <- ggplot(
  default_fuel,
  aes(
    x = reorder(Fuel, Share),
    y = Share * 100
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    title = paste0(
      "Primary Energy Composition: ",
      default_scenario
    ),
    x = NULL,
    y = "Share (%)"
  ) +
  theme_shannon

print(p_fuel)

ggsave(
  file.path(output_dir, "Default_fuel_composition.png"),
  p_fuel,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 18. 計算結果表示
# ============================================================

cat("\n====================================\n")
cat("Japan 2050 Shannon Diversity\n")
cat("====================================\n")

cat(
  "Number of scenarios:",
  nrow(shannon_result),
  "\n"
)

cat(
  "Default scenario:",
  default_scenario,
  "\n"
)

cat(
  "Default Shannon:",
  default_H,
  "\n"
)

cat(
  "Reference Shannon:",
  ref_H,
  "\n"
)

cat(
  "Maximum Shannon:",
  max(plot_data$Shannon),
  "\n"
)

cat(
  "Minimum Shannon:",
  min(plot_data$Shannon),
  "\n"
)

cat(
  "Mean Shannon:",
  mean(plot_data$Shannon),
  "\n"
)

cat(
  "Output directory:",
  output_dir,
  "\n"
)

cat("====================================\n")
