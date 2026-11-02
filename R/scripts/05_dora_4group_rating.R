# =============================================================================
# 05_dora_4group_rating.R
# DORA群における悪夢/SP有無別4群のRating比較
# =============================================================================

library(tidyverse)
library(dunn.test)  # Dunn検定用

# 01_load_data.R を先に実行しておくこと

cat("=============================================================================\n")
cat("DORA群 4群比較: Rating by NAD/SP status\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# 1. DORA群のみ抽出
# -----------------------------------------------------------------------------

dora_drugs <- c("suvorexant", "lemborexant", "daridorexant")

df_dora <- df_analysis %>%
  filter(drug %in% dora_drugs)

cat("【DORA群データ】\n")
cat("総数:", nrow(df_dora), "\n\n")
cat("薬剤別:\n")
print(df_dora %>% count(drug, sort = TRUE))

# -----------------------------------------------------------------------------
# 2. 4群の作成（離脱除外後のアウトカム使用）
# -----------------------------------------------------------------------------

df_dora <- df_dora %>%
  mutate(
    group = case_when(
      nightmare_excl_withdrawal == 0 & sp_excl_withdrawal == 0 ~ "Neither",
      nightmare_excl_withdrawal == 1 & sp_excl_withdrawal == 0 ~ "NAD only",
      nightmare_excl_withdrawal == 0 & sp_excl_withdrawal == 1 ~ "SP only",
      nightmare_excl_withdrawal == 1 & sp_excl_withdrawal == 1 ~ "Both",
      TRUE ~ NA_character_
    ),
    # 順序を指定
    group = factor(group, levels = c("Neither", "NAD only", "SP only", "Both"))
  )

cat("\n【4群の分布】\n")
print(df_dora %>% count(group))

# -----------------------------------------------------------------------------
# 3. 記述統計
# -----------------------------------------------------------------------------

cat("\n【各群のRating統計】\n")

group_stats <- df_dora %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(rating, na.rm = TRUE), 2),
    sd = round(sd(rating, na.rm = TRUE), 2),
    median = median(rating, na.rm = TRUE),
    q1 = quantile(rating, 0.25, na.rm = TRUE),
    q3 = quantile(rating, 0.75, na.rm = TRUE),
    min = min(rating, na.rm = TRUE),
    max = max(rating, na.rm = TRUE),
    .groups = "drop"
  )

print(group_stats)

# -----------------------------------------------------------------------------
# 4. Kruskal-Wallis検定
# -----------------------------------------------------------------------------

cat("\n【Kruskal-Wallis検定】\n")

kw_test <- kruskal.test(rating ~ group, data = df_dora)

cat("  Chi-squared =", round(kw_test$statistic, 2), "\n")
cat("  df =", kw_test$parameter, "\n")
cat("  p-value =", format.pval(kw_test$p.value, digits = 4), "\n")

# -----------------------------------------------------------------------------
# 5. Dunn検定（事後検定）
# -----------------------------------------------------------------------------

cat("\n【Dunn検定（Bonferroni補正）】\n")

dunn_result <- dunn.test(
  df_dora$rating,
  df_dora$group,
  method = "bonferroni",
  kw = FALSE,
  label = TRUE,
  table = FALSE
)

# Dunn検定結果をデータフレームに整形
dunn_df <- tibble(
  comparison = dunn_result$comparisons,
  Z = round(dunn_result$Z, 3),
  p_unadjusted = dunn_result$P,
  p_adjusted = dunn_result$P.adjusted
) %>%
  mutate(
    significance = case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

print(dunn_df)

# -----------------------------------------------------------------------------
# 6. 作図
# -----------------------------------------------------------------------------

cat("\n【作図中...】\n")

# カラーパレット（4群用、グレースケール）
group_colors <- c(
  "Neither" = "#E0E0E0",
  "NAD only" = "#909090",
  "SP only" = "#606060",
  "Both" = "#303030"
)

# 群ラベル（改行入り）
group_labels <- c(

"Neither" = "Neither\n(n=GROUP_N1)",
  "NAD only" = "NAD\nonly\n(n=GROUP_N2)",
  "SP only" = "SP only\n(n=GROUP_N3)",
  "Both" = "Both\n(n=GROUP_N4)"
)

# Nを埋め込む
n_by_group <- df_dora %>% count(group) %>% deframe()
group_labels_with_n <- c(
  "Neither" = paste0("Neither\n(n=", n_by_group["Neither"], ")"),
  "NAD only" = paste0("NAD\nonly\n(n=", n_by_group["NAD only"], ")"),
  "SP only" = paste0("SP only\n(n=", n_by_group["SP only"], ")"),
  "Both" = paste0("Both\n(n=", n_by_group["Both"], ")")
)

# p値の表示用
p_display <- ifelse(kw_test$p.value < 0.001, "p < 0.001",
                    paste0("p = ", round(kw_test$p.value, 3)))

# メインプロット
p_main <- df_dora %>%
  ggplot(aes(x = group, y = rating, fill = group)) +
  # ボックスプロット（外れ値非表示、ジッターで表示するため）
  geom_boxplot(
    alpha = 0.8,
    outlier.shape = NA,
    color = "black",
    width = 0.6
  ) +
  # ジッタープロット（実測値）
  geom_jitter(
    width = 0.15,
    alpha = 0.4,
    size = 1.5,
    color = "black"
  ) +
  # 中央値を強調（赤い横線）
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.5,
    color = "red",
    linewidth = 0.8
  ) +
  # カラー設定
  scale_fill_manual(values = group_colors) +
  # X軸ラベル
  scale_x_discrete(labels = group_labels_with_n) +
  # Y軸
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5, 1)) +
  # ラベル
  labs(
    title = "Overall rating by NAD/SP status in DORA users",
    subtitle = paste0("Kruskal-Wallis test: ", p_display),
    x = NULL,
    y = "Rating (5-point scale)"
  ) +
  # テーマ
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )

# 保存
ggsave(
  "output/figures/dora_rating_4groups.png",
  p_main,
  width = 8,
  height = 6,
  dpi = 300
)

cat("  保存: output/figures/dora_rating_4groups.png\n")

# -----------------------------------------------------------------------------
# 7. 有意な事後比較を示す簡潔版（論文用）
# -----------------------------------------------------------------------------

# Dunn検定で有意だった比較のみを、図中の括弧の位置と対応させる
significant_brackets <- tribble(
  ~comparison,          ~x_start, ~x_end, ~y,
  "NAD only - Neither",        1,      2, 5.25,
  "Neither - SP only",         1,      3, 5.60,
  "Both - Neither",            1,      4, 5.95
) %>%
  left_join(
    dunn_df %>% filter(p_adjusted < 0.05) %>% select(comparison, p_adjusted),
    by = "comparison"
  ) %>%
  mutate(
    label = if_else(
      p_adjusted < 0.001,
      "p < 0.001",
      paste0("p = ", formatC(p_adjusted, format = "f", digits = 3))
    )
  )

if (any(is.na(significant_brackets$p_adjusted))) {
  stop("Figure 3用の有意なDunn検定結果を特定できませんでした。")
}

p_clean <- df_dora %>%
  ggplot(aes(x = group, y = rating, fill = group)) +
  geom_boxplot(
    alpha = 0.8,
    outlier.shape = NA,
    color = "black",
    width = 0.6
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.35,
    size = 1.2,
    color = "black"
  ) +
  # Dunn検定（Bonferroni補正）で有意だった比較
  geom_segment(
    data = significant_brackets,
    aes(x = x_start, xend = x_end, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = significant_brackets,
    aes(x = x_start, xend = x_start, y = y, yend = y - 0.08),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = significant_brackets,
    aes(x = x_end, xend = x_end, y = y, yend = y - 0.08),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_text(
    data = significant_brackets,
    aes(x = (x_start + x_end) / 2, y = y + 0.08, label = label),
    inherit.aes = FALSE,
    size = 3.2
  ) +
  scale_fill_manual(values = group_colors) +
  scale_x_discrete(labels = group_labels_with_n) +
  scale_y_continuous(limits = c(0, 6.2), breaks = seq(0, 5, 1)) +
  labs(
    title = "Overall rating by NAD/SP status (DORA)",
    subtitle = paste0("Kruskal-Wallis: ", p_display),
    x = NULL,
    y = "Rating (5-point scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_blank()
  )

ggsave(
  "output/figures/dora_rating_4groups_clean.png",
  p_clean,
  width = 8,
  height = 6,
  dpi = 300
)

cat("  保存: output/figures/dora_rating_4groups_clean.png\n")

# -----------------------------------------------------------------------------
# 8. CSV保存
# -----------------------------------------------------------------------------

# 記述統計
write_csv(group_stats, "output/tables/dora_4group_rating_stats.csv")

# Dunn検定結果
write_csv(dunn_df, "output/tables/dora_4group_dunn_test.csv")

# サマリー（Kruskal-Wallis含む）
summary_df <- tibble(
  analysis = "DORA 4-group comparison",
  total_n = nrow(df_dora),
  kw_chi_sq = round(kw_test$statistic, 3),
  kw_df = kw_test$parameter,
  kw_p_value = kw_test$p.value
)
write_csv(summary_df, "output/tables/dora_4group_kw_summary.csv")

cat("\n【CSV保存完了】\n")
cat("  - output/tables/dora_4group_rating_stats.csv\n")
cat("  - output/tables/dora_4group_dunn_test.csv\n")
cat("  - output/tables/dora_4group_kw_summary.csv\n")

# -----------------------------------------------------------------------------
# 9. 薬剤別の内訳（参考）
# -----------------------------------------------------------------------------

cat("\n【参考: 薬剤別 × 4群】\n")
print(df_dora %>% count(drug, group) %>% pivot_wider(names_from = group, values_from = n, values_fill = 0))

cat("\n=== DORA 4群比較 完了 ===\n")
