# =============================================================================
# 03_main_prr_forest.R
# メイン結果: PRR + Bootstrap CI + フォレストプロット
# =============================================================================

library(tidyverse)
library(boot)
library(openxlsx)

# 01_load_data.R を先に実行しておくこと

cat("=== Main Analysis: PRR with Bootstrap CI ===\n\n")

# -----------------------------------------------------------------------------
# 1. PRR計算関数の定義
# -----------------------------------------------------------------------------

calculate_prr <- function(data, drug_name, outcome_col) {
  target <- data %>% filter(drug == drug_name)
  others <- data %>% filter(drug != drug_name)

  a <- sum(target[[outcome_col]], na.rm = TRUE)
  b <- nrow(target) - a
  c <- sum(others[[outcome_col]], na.rm = TRUE)
  d <- nrow(others) - c

  prop_target <- a / (a + b)
  prop_others <- c / (c + d)

  if (prop_others == 0) return(NA)
  return(prop_target / prop_others)
}

prr_boot <- function(data, indices, drug_name, outcome_col) {
  d <- data[indices, ]
  return(calculate_prr(d, drug_name, outcome_col))
}

calculate_prr_with_ci <- function(data, drug_name, outcome_col, n_boot = 100000) {
  prr <- calculate_prr(data, drug_name, outcome_col)

  if (is.na(prr)) {
    return(c(PRR = NA, CI_lower = NA, CI_upper = NA))
  }

  set.seed(42)
  boot_result <- boot(
    data = data,
    statistic = prr_boot,
    R = n_boot,
    drug_name = drug_name,
    outcome_col = outcome_col
  )

  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

  if (is.null(ci)) {
    return(c(PRR = prr, CI_lower = NA, CI_upper = NA))
  }

  return(c(
    PRR = prr,
    CI_lower = ci$percent[4],
    CI_upper = ci$percent[5]
  ))
}

# -----------------------------------------------------------------------------
# 2. 全薬剤のPRR計算
# -----------------------------------------------------------------------------

cat("PRR計算開始（Bootstrap 100,000回）...\n\n")

drugs <- unique(df_analysis$drug)

# 悪夢のPRR
cat("【NAD PRR】\n")
nightmare_prr <- map_dfr(drugs, function(d) {
  cat("  Processing:", d, "\n")
  result <- calculate_prr_with_ci(df_analysis, d, "drug_related_nightmare_final")
  tibble(drug = d, PRR = result["PRR"], CI_lower = result["CI_lower"], CI_upper = result["CI_upper"])
})

# 睡眠麻痺のPRR
cat("\n【SP PRR】\n")
sp_prr <- map_dfr(drugs, function(d) {
  cat("  Processing:", d, "\n")
  result <- calculate_prr_with_ci(df_analysis, d, "drug_related_sp_final")
  tibble(drug = d, PRR = result["PRR"], CI_lower = result["CI_lower"], CI_upper = result["CI_upper"])
})

# -----------------------------------------------------------------------------
# 3. 結果の整理
# -----------------------------------------------------------------------------

nightmare_results <- nightmare_prr %>%
  left_join(drug_class, by = "drug") %>%
  left_join(
    df_analysis %>%
      group_by(drug) %>%
      summarise(
        n = n(),
        cases = sum(drug_related_nightmare_final, na.rm = TRUE),
        rate = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
        .groups = "drop"
      ),
    by = "drug"
  ) %>%
  arrange(desc(PRR))

sp_results <- sp_prr %>%
  left_join(drug_class, by = "drug") %>%
  left_join(
    df_analysis %>%
      group_by(drug) %>%
      summarise(
        n = n(),
        cases = sum(drug_related_sp_final, na.rm = TRUE),
        rate = mean(drug_related_sp_final, na.rm = TRUE) * 100,
        .groups = "drop"
      ),
    by = "drug"
  ) %>%
  arrange(desc(PRR))

cat("\n=== NAD PRR Results ===\n")
print(nightmare_results %>%
        select(drug, class, n, cases, rate, PRR, CI_lower, CI_upper) %>%
        mutate(across(c(rate, PRR, CI_lower, CI_upper), ~round(., 2))))

cat("\n=== SP PRR Results ===\n")
print(sp_results %>%
        select(drug, class, n, cases, rate, PRR, CI_lower, CI_upper) %>%
        mutate(across(c(rate, PRR, CI_lower, CI_upper), ~round(., 2))))

# -----------------------------------------------------------------------------
# 4. フォレストプロット作成
# -----------------------------------------------------------------------------

cat("\n=== Creating Forest Plots ===\n")

# カラーテーマ（錠剤の色を参考）
class_colors <- c(
  "BZD" = "#007BFF",        # コバルトブルー（Valiumの青錠剤）
  "Z-drug" = "#4169E1",     # ロイヤルブルー（Lunesta/Ambien）
  "MRA" = "#FFA500",        # オレンジ（Rozeremのイエロー/ベージュ）
  "Sedating AD" = "#DAA520", # ゴールデンロッド（Mirtazapine/Trazodone）
  "DORA" = "#FF7F50"        # コーラルオレンジ（Quviviq）
)

create_forest_plot <- function(data, title, filename, x_limits = c(0.1, 20), x_breaks = NULL) {
  data <- data %>% mutate(drug = fct_reorder(drug, PRR))

  # デフォルトのbreaks設定
  if (is.null(x_breaks)) {
    x_breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16)
  }

  p <- ggplot(data, aes(x = PRR, y = drug, color = class)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, linewidth = 0.8) +
    geom_point(size = 3) +
    geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, rate)),
              hjust = -0.1, vjust = -0.5, size = 3, color = "gray30") +
    scale_x_log10(
      breaks = x_breaks,
      labels = as.character(x_breaks)
    ) +
    scale_color_manual(values = class_colors, name = "Drug Class") +
    labs(
      title = title,
      x = "Proportional Reporting Ratio (PRR) [log scale]",
      y = NULL,
      caption = "Error bars: 95% CI (Bootstrap, 100,000 replications). Dashed line: PRR = 1"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(xlim = x_limits)

  ggsave(filename, p, width = 10, height = 7, dpi = 300)
  cat("Saved:", filename, "\n")
  return(p)
}

# NAD Forest Plot
p_nightmare <- create_forest_plot(
  nightmare_results,
  "Nightmares and abnormal dreams: Proportional Reporting Ratio",
  "output/figures/figure_forest_nightmare.png",
  x_limits = c(0.1, 20),
  x_breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16)
)

# SP Forest Plot（スケール拡張：lemborexant対応）
p_sp <- create_forest_plot(
  sp_results,
  "Sleep paralysis: Proportional Reporting Ratio",
  "output/figures/figure_forest_sp.png",
  x_limits = c(0.01, 200),
  x_breaks = c(0.01, 0.1, 1, 10, 100)
)

# -----------------------------------------------------------------------------
# 5. 薬剤クラス5群比較: Omnibus + Post-hoc
# -----------------------------------------------------------------------------

cat("\n=== Drug Class Comparison (5-group Omnibus + Post-hoc) ===\n")

class_order <- c("DORA", "MRA", "Sedating AD", "Z-drug", "BZD")
df_analysis <- df_analysis %>%
  mutate(class_f = factor(class, levels = class_order))

df_dora <- df_analysis %>% filter(class == "DORA")
other_classes <- c("MRA", "Sedating AD", "Z-drug", "BZD")

# --- NAD: Omnibus ---
cat("\n【NAD - Omnibus Test (5 classes)】\n")
nm_class_table <- table(df_analysis$class_f, df_analysis$drug_related_nightmare_final)
print(nm_class_table)

nm_expected <- suppressWarnings(chisq.test(nm_class_table))$expected
cat("\nExpected counts (min =", round(min(nm_expected), 1), "):\n")

if (any(nm_expected < 5)) {
  cat("Expected counts < 5 detected -> Fisher-Freeman-Halton test\n")
  set.seed(42)
  nm_omnibus <- fisher.test(nm_class_table, simulate.p.value = TRUE, B = 100000)
  nm_omnibus_method <- "Fisher-Freeman-Halton (Monte Carlo, B=100,000)"
} else {
  nm_omnibus <- chisq.test(nm_class_table)
  nm_omnibus_method <- paste0("Chi-square (df=", nm_omnibus$parameter, ")")
}
cat("Method:", nm_omnibus_method, "\n")
cat("p-value:", format.pval(nm_omnibus$p.value, digits = 3), "\n")

# --- SP: Omnibus ---
cat("\n【SP - Omnibus Test (5 classes)】\n")
sp_class_table <- table(df_analysis$class_f, df_analysis$drug_related_sp_final)
print(sp_class_table)

sp_expected <- suppressWarnings(chisq.test(sp_class_table))$expected
cat("\nExpected counts (min =", round(min(sp_expected), 1), "):\n")

if (any(sp_expected < 5)) {
  cat("Expected counts < 5 detected -> Fisher-Freeman-Halton test\n")
  set.seed(42)
  sp_omnibus <- fisher.test(sp_class_table, simulate.p.value = TRUE, B = 100000)
  sp_omnibus_method <- "Fisher-Freeman-Halton (Monte Carlo, B=100,000)"
} else {
  sp_omnibus <- chisq.test(sp_class_table)
  sp_omnibus_method <- paste0("Chi-square (df=", sp_omnibus$parameter, ")")
}
cat("Method:", sp_omnibus_method, "\n")
cat("p-value:", format.pval(sp_omnibus$p.value, digits = 3), "\n")

# --- Post-hoc: DORA vs Each Class (Holm correction) ---
cat("\n【Post-hoc: DORA vs Each Class (Holm correction)】\n")

posthoc_results <- map_dfr(other_classes, function(cl) {
  df_other <- df_analysis %>% filter(class == cl)

  dora_nm <- sum(df_dora$drug_related_nightmare_final, na.rm = TRUE)
  dora_n  <- nrow(df_dora)
  other_nm <- sum(df_other$drug_related_nightmare_final, na.rm = TRUE)
  other_n  <- nrow(df_other)

  nm_mat <- matrix(c(other_nm, other_n - other_nm, dora_nm, dora_n - dora_nm), nrow = 2)
  nm_fisher <- fisher.test(nm_mat)

  dora_sp  <- sum(df_dora$drug_related_sp_final, na.rm = TRUE)
  other_sp <- sum(df_other$drug_related_sp_final, na.rm = TRUE)

  sp_mat <- matrix(c(other_sp, other_n - other_sp, dora_sp, dora_n - dora_sp), nrow = 2)
  sp_fisher <- fisher.test(sp_mat)

  tibble(
    comparison     = paste("DORA vs", cl),
    dora_n         = dora_n,
    other_n        = other_n,
    dora_nad_n     = dora_nm,
    other_nad_n    = other_nm,
    dora_nad_rate  = dora_nm / dora_n * 100,
    other_nad_rate = other_nm / other_n * 100,
    nad_OR         = nm_fisher$estimate,
    nad_ci_lower   = nm_fisher$conf.int[1],
    nad_ci_upper   = nm_fisher$conf.int[2],
    nad_p_raw      = nm_fisher$p.value,
    dora_sp_n      = dora_sp,
    other_sp_n     = other_sp,
    dora_sp_rate   = dora_sp / dora_n * 100,
    other_sp_rate  = other_sp / other_n * 100,
    sp_OR          = sp_fisher$estimate,
    sp_ci_lower    = sp_fisher$conf.int[1],
    sp_ci_upper    = sp_fisher$conf.int[2],
    sp_p_raw       = sp_fisher$p.value
  )
})

# Holm correction (4 comparisons)
posthoc_results <- posthoc_results %>%
  mutate(
    nad_p_holm = p.adjust(nad_p_raw, method = "holm"),
    sp_p_holm  = p.adjust(sp_p_raw,  method = "holm")
  )

cat("\n")
print(posthoc_results %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# -----------------------------------------------------------------------------
# 6. Publication Table: DORA vs Each Class (Excel)
# -----------------------------------------------------------------------------

cat("\n=== Creating Publication Table ===\n")

# Helper: 3桁フォーマット（10以上は小数1桁、未満は小数2桁）
fmt_rate <- function(x) {
  ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
}

# Helper: OR [95% CI] フォーマット
fmt_or_ci <- function(or_val, ci_lo, ci_hi) {
  if (is.na(or_val) || is.infinite(or_val)) return("\u2014")
  ci_hi_str <- if (is.infinite(ci_hi)) "\u221e" else sprintf("%.2f", ci_hi)
  paste0(sprintf("%.2f", or_val), " [", sprintf("%.2f", ci_lo), "\u2013", ci_hi_str, "]")
}

# Helper: p-value フォーマット
fmt_p <- function(p) {
  if (is.na(p) || p == "") return("")
  p <- as.numeric(p)
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# Helper: 文中表記用 p値（"p < 0.001" or "p = 0.123"）
fmt_p_stmt <- function(p) {
  ps <- fmt_p(p)
  if (substr(ps, 1, 1) == "<") paste0("p < ", sub("^<", "", ps)) else paste0("p = ", ps)
}

# DORA reference row
dora_n_total   <- posthoc_results$dora_n[1]
dora_nad_n_val <- posthoc_results$dora_nad_n[1]
dora_nad_rate  <- posthoc_results$dora_nad_rate[1]
dora_sp_n_val  <- posthoc_results$dora_sp_n[1]
dora_sp_rate   <- posthoc_results$dora_sp_rate[1]

ref_row <- tibble(
  drug_class = "DORA",
  n          = dora_n_total,
  nad_events = paste0(dora_nad_n_val, " (", fmt_rate(dora_nad_rate), ")"),
  nad_or     = "ref",
  nad_p      = "",
  sp_events  = paste0(dora_sp_n_val, " (", fmt_rate(dora_sp_rate), ")"),
  sp_or      = "ref",
  sp_p       = ""
)

# Comparator rows
comp_rows <- posthoc_results %>%
  rowwise() %>%
  mutate(
    drug_class = str_remove(comparison, "DORA vs "),
    nad_events = paste0(other_nad_n, " (", fmt_rate(other_nad_rate), ")"),
    nad_or     = fmt_or_ci(nad_OR, nad_ci_lower, nad_ci_upper),
    nad_p      = fmt_p(nad_p_holm),
    sp_events  = paste0(other_sp_n, " (", fmt_rate(other_sp_rate), ")"),
    sp_or      = fmt_or_ci(sp_OR, sp_ci_lower, sp_ci_upper),
    sp_p       = fmt_p(sp_p_holm)
  ) %>%
  ungroup() %>%
  transmute(drug_class, n = other_n, nad_events, nad_or, nad_p,
            sp_events, sp_or, sp_p)

pub_table <- bind_rows(ref_row, comp_rows)

cat("\n")
print(pub_table)

# --- Excel出力 ---
wb <- createWorkbook()
addWorksheet(wb, "Post-hoc")

# Row 1: merged outcome headers
writeData(wb, "Post-hoc", "", startCol = 1, startRow = 1)
writeData(wb, "Post-hoc", "Nightmares and abnormal dreams", startCol = 3, startRow = 1)
writeData(wb, "Post-hoc", "Sleep paralysis", startCol = 6, startRow = 1)
mergeCells(wb, "Post-hoc", cols = 3:5, rows = 1)
mergeCells(wb, "Post-hoc", cols = 6:8, rows = 1)

# Row 2: column headers
col_headers <- c("Drug class", "n", "n (%)", "OR [95% CI]", "Adjusted p",
                  "n (%)", "OR [95% CI]", "Adjusted p")
writeData(wb, "Post-hoc", as.data.frame(t(col_headers)),
          startRow = 2, colNames = FALSE)

# Row 3+: data
writeData(wb, "Post-hoc", pub_table, startRow = 3, colNames = FALSE)

# Footnotes
fn_row <- nrow(pub_table) + 4
writeData(wb, "Post-hoc",
  paste0("Omnibus test (5 drug classes): NAD, ", nm_omnibus_method, ", ",
         fmt_p_stmt(nm_omnibus$p.value), "; SP, ", sp_omnibus_method, ", ",
         fmt_p_stmt(sp_omnibus$p.value), "."),
  startRow = fn_row, startCol = 1)
mergeCells(wb, "Post-hoc", cols = 1:8, rows = fn_row)

writeData(wb, "Post-hoc",
  paste0("Pairwise comparisons of each comparator class with the DORA class were ",
         "performed using Fisher's exact test; adjusted p, Holm-corrected p-values for ",
         "the 4 post-hoc comparisons. Odds ratios express the odds of the outcome in the ",
         "comparator class relative to the DORA class (reference); an OR < 1 indicates a ",
         "lower reporting frequency than in the DORA class."),
  startRow = fn_row + 1, startCol = 1)
mergeCells(wb, "Post-hoc", cols = 1:8, rows = fn_row + 1)

writeData(wb, "Post-hoc",
  "OR, odds ratio; CI, confidence interval; NAD, nightmares and abnormal dreams; SP, sleep paralysis.",
  startRow = fn_row + 2, startCol = 1)
mergeCells(wb, "Post-hoc", cols = 1:8, rows = fn_row + 2)

# Styles
outcome_style <- createStyle(halign = "center", textDecoration = "bold",
                              border = "bottom", fontSize = 11)
addStyle(wb, "Post-hoc", outcome_style, rows = 1, cols = 1:8)

col_header_style <- createStyle(halign = "center", textDecoration = "bold",
                                 border = "bottom", wrapText = TRUE, fontSize = 10)
addStyle(wb, "Post-hoc", col_header_style, rows = 2, cols = 1:8)

center_style <- createStyle(halign = "center")
addStyle(wb, "Post-hoc", center_style,
         rows = 3:(nrow(pub_table) + 2), cols = 2:8, gridExpand = TRUE)

ref_style <- createStyle(textDecoration = "bold")
addStyle(wb, "Post-hoc", ref_style, rows = 3, cols = 1)

footnote_style <- createStyle(fontSize = 9, wrapText = TRUE)
addStyle(wb, "Post-hoc", footnote_style,
         rows = fn_row:(fn_row + 2), cols = 1, stack = TRUE)

# Column widths
setColWidths(wb, "Post-hoc", cols = 1, widths = 16)
setColWidths(wb, "Post-hoc", cols = 2, widths = 7)
setColWidths(wb, "Post-hoc", cols = c(3, 6), widths = 12)
setColWidths(wb, "Post-hoc", cols = c(4, 7), widths = 20)
setColWidths(wb, "Post-hoc", cols = c(5, 8), widths = 12)

saveWorkbook(wb, "output/tables/main_dora_vs_class_posthoc.xlsx", overwrite = TRUE)
cat("Saved: output/tables/main_dora_vs_class_posthoc.xlsx\n")

# -----------------------------------------------------------------------------
# 7. CSVに保存
# -----------------------------------------------------------------------------

write_csv(nightmare_results, "output/tables/main_nightmare_prr.csv")
write_csv(sp_results, "output/tables/main_sp_prr.csv")

# Omnibus (5群)
omnibus_results <- tibble(
  outcome = c("NAD", "SP"),
  method  = c(nm_omnibus_method, sp_omnibus_method),
  p_value = c(nm_omnibus$p.value, sp_omnibus$p.value)
)
write_csv(omnibus_results, "output/tables/main_class_omnibus.csv")

# Post-hoc (raw data)
write_csv(posthoc_results, "output/tables/main_dora_vs_class_posthoc.csv")

# =============================================================================

cat("\n=== Main Analysis Complete ===\n")
cat("Tables:\n")
cat("  - output/tables/main_nightmare_prr.csv\n")
cat("  - output/tables/main_sp_prr.csv\n")
cat("  - output/tables/main_class_omnibus.csv\n")
cat("  - output/tables/main_dora_vs_class_posthoc.csv\n")
cat("  - output/tables/main_dora_vs_class_posthoc.xlsx\n")
cat("Figures:\n")
cat("  - output/figures/figure_forest_nightmare.png\n")
cat("  - output/figures/figure_forest_sp.png\n")
