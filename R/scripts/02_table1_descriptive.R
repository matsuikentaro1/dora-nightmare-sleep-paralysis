# =============================================================================
# 02_table1_descriptive.R
# Table 1: Demographic Data (Natter et al. 2021 format)
# =============================================================================

library(tidyverse)
library(openxlsx)

# UTF-8エンコーディング設定（Windows対応）
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# 01_load_data.R を先に実行しておくこと

cat("=== Table 1: Demographic Data ===\n\n")

# -----------------------------------------------------------------------------
# Helper function: 3桁フォーマット（10以上は小数点1位、10未満は小数点2位）
# -----------------------------------------------------------------------------

format_3digit <- function(x) {
  ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
}

# Unicode文字を変数で定義（エスケープシーケンス問題回避）
pm_sign <- "\u00B1"      # ±
en_dash <- "\u2013"      # –

# -----------------------------------------------------------------------------
# 1. Table 1 作成（Natter 2021形式）
# -----------------------------------------------------------------------------

total_n <- nrow(df_analysis)

# 薬剤クラス順（DORA → MRA → Sedating AD → Z-drug → BZD）、各クラス内はN降順
drug_order <- c(
  "suvorexant",   # DORA
  "lemborexant",  # DORA
  "daridorexant", # DORA
  "ramelteon",    # MRA
  "trazodone",    # Sedating AD
  "doxepin",      # Sedating AD
  "zolpidem",     # Z-drug
  "eszopiclone",  # Z-drug
  "zaleplon",     # Z-drug
  "temazepam",    # BZD
  "triazolam"     # BZD
)

# 薬剤別の統計
table1_drugs <- df_analysis %>%
  group_by(drug, class) %>%
  summarise(
    n = n(),
    rating_mean = mean(rating, na.rm = TRUE),
    rating_sd = sd(rating, na.rm = TRUE),
    nightmare_n = sum(drug_related_nightmare_final, na.rm = TRUE),
    nightmare_pct = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
    sp_n = sum(drug_related_sp_final, na.rm = TRUE),
    sp_pct = mean(drug_related_sp_final, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = n / total_n * 100,
    drug = factor(drug, levels = drug_order)
  ) %>%
  arrange(drug)

# Total行
table1_total <- df_analysis %>%
  summarise(
    drug = "Total",
    class = en_dash,
    n = n(),
    rating_mean = mean(rating, na.rm = TRUE),
    rating_sd = sd(rating, na.rm = TRUE),
    nightmare_n = sum(drug_related_nightmare_final, na.rm = TRUE),
    nightmare_pct = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
    sp_n = sum(drug_related_sp_final, na.rm = TRUE),
    sp_pct = mean(drug_related_sp_final, na.rm = TRUE) * 100,
    pct_of_total = 100
  )

# 結合
table1_full <- bind_rows(table1_drugs, table1_total)

# -----------------------------------------------------------------------------
# 2. Natter形式の表示用フォーマット
# -----------------------------------------------------------------------------

# 表示用にフォーマット（Natter 2021 Table 1と同様）
# カラム名にはASCII、値にUnicode使用
table1_display <- table1_full %>%
  mutate(
    Molecule = drug,
    Class = class,
    Reviews = paste0(format(n, big.mark = ","), " (", format_3digit(pct_of_total), ")"),
    User_Rating = paste0(format_3digit(rating_mean), " ", pm_sign, " ", format_3digit(rating_sd)),
    NAD = paste0(nightmare_n, " (", format_3digit(nightmare_pct), ")"),
    SP = paste0(sp_n, " (", format_3digit(sp_pct), ")")
  ) %>%
  select(Molecule, Class, Reviews, User_Rating, NAD, SP)

# カラム名をリネーム（Publication用）
colnames(table1_display) <- c(
  "Molecule",
  "Class",
  "Reviews, n (%)*",
  paste0("User rating", "\u2020"),
  paste0("Nightmares and abnormal dreams, n (%)", "\u2021"),
  paste0("Sleep paralysis, n (%)", "\u2021")
)

# -----------------------------------------------------------------------------
# 3. 出力
# -----------------------------------------------------------------------------

cat("Table 1. Characteristics of the 11 hypnotic drugs studied\n")
cat("=========================================================================\n")
cat("* Percentage of total reviews (N = ", format(total_n, big.mark = ","), ")\n", sep = "")
cat("\u2020 Mean \u00B1 SD; 0\u20135 scale\n")
cat("\u2021 Percentage of reviews per molecule\n")
cat("=========================================================================\n\n")

# テーブル表示
print(table1_display, n = 15)

# -----------------------------------------------------------------------------
# 4. クラス別サマリー
# -----------------------------------------------------------------------------

cat("\n\n【Class Summary】\n")

table1_class <- df_analysis %>%
  group_by(class) %>%
  summarise(
    n = n(),
    pct_of_total = n() / total_n * 100,
    rating_mean = mean(rating, na.rm = TRUE),
    rating_sd = sd(rating, na.rm = TRUE),
    nightmare_n = sum(drug_related_nightmare_final, na.rm = TRUE),
    nightmare_pct = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
    sp_n = sum(drug_related_sp_final, na.rm = TRUE),
    sp_pct = mean(drug_related_sp_final, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Class = class,
    Reviews = paste0(format(n, big.mark = ","), " (", format_3digit(pct_of_total), ")"),
    User_Rating = paste0(format_3digit(rating_mean), " ", pm_sign, " ", format_3digit(rating_sd)),
    NAD = paste0(nightmare_n, " (", format_3digit(nightmare_pct), ")"),
    SP = paste0(sp_n, " (", format_3digit(sp_pct), ")")
  ) %>%
  select(Class, Reviews, User_Rating, NAD, SP) %>%
  arrange(desc(as.numeric(gsub(",", "", gsub(" .*", "", Reviews)))))

print(table1_class)

# -----------------------------------------------------------------------------
# 5. DORA vs Comparators
# -----------------------------------------------------------------------------

cat("\n\n【DORA vs Comparators】\n")

table1_dora <- df_analysis %>%
  mutate(group = ifelse(class == "DORA", "DORA", "Comparators")) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    pct_of_total = n() / total_n * 100,
    rating_mean = mean(rating, na.rm = TRUE),
    rating_sd = sd(rating, na.rm = TRUE),
    nightmare_n = sum(drug_related_nightmare_final, na.rm = TRUE),
    nightmare_pct = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
    sp_n = sum(drug_related_sp_final, na.rm = TRUE),
    sp_pct = mean(drug_related_sp_final, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Group = group,
    Reviews = paste0(format(n, big.mark = ","), " (", format_3digit(pct_of_total), ")"),
    User_Rating = paste0(format_3digit(rating_mean), " ", pm_sign, " ", format_3digit(rating_sd)),
    NAD = paste0(nightmare_n, " (", format_3digit(nightmare_pct), ")"),
    SP = paste0(sp_n, " (", format_3digit(sp_pct), ")")
  ) %>%
  select(Group, Reviews, User_Rating, NAD, SP)

print(table1_dora)

# -----------------------------------------------------------------------------
# 6. Excelファイルに保存（Publication format）
# -----------------------------------------------------------------------------

dagger  <- "\u2020"   # †
ddagger <- "\u2021"   # ‡

wb <- createWorkbook()
addWorksheet(wb, "Table 1")

# --- Column headers ---
col_headers <- c(
  "Molecule",
  "Class",
  paste0("Reviews, n (%)*"),
  paste0("User rating", dagger),
  paste0("Nightmares and abnormal dreams, n (%)", ddagger),
  paste0("Sleep paralysis, n (%)", ddagger)
)
writeData(wb, "Table 1", as.data.frame(t(col_headers)),
          startRow = 1, colNames = FALSE)

# --- Data ---
table1_out <- table1_display
colnames(table1_out) <- paste0("V", seq_len(ncol(table1_out)))
writeData(wb, "Table 1", table1_out, startRow = 2, colNames = FALSE)

data_end_row <- nrow(table1_out) + 1   # header=1, data starts at 2

# --- Footnotes ---
fn_row <- data_end_row + 2
total_n_fmt <- format(total_n, big.mark = ",")

fn_lines <- c(
  paste0("* Percentage of total reviews (N = ", total_n_fmt, ")."),
  paste0(dagger, " Mean ", pm_sign, " SD; 0", en_dash, "5 scale."),
  paste0(ddagger, " Percentage of reviews per molecule.")
)
for (i in seq_along(fn_lines)) {
  writeData(wb, "Table 1", fn_lines[i],
            startRow = fn_row + i - 1, startCol = 1)
  mergeCells(wb, "Table 1", cols = 1:6, rows = fn_row + i - 1)
}

# --- Styles ---

# Header row
header_style <- createStyle(
  halign = "center", textDecoration = "bold",
  border = "TopBottom", wrapText = TRUE, fontSize = 10
)
addStyle(wb, "Table 1", header_style, rows = 1, cols = 1:6)

# Molecule column: left-align
left_style <- createStyle(halign = "left")
addStyle(wb, "Table 1", left_style,
         rows = 2:data_end_row, cols = 1, gridExpand = TRUE)

# Class column: left-align
addStyle(wb, "Table 1", left_style,
         rows = 2:data_end_row, cols = 2, gridExpand = TRUE)

# Numeric columns: center
center_style <- createStyle(halign = "center")
addStyle(wb, "Table 1", center_style,
         rows = 2:data_end_row, cols = 3:6, gridExpand = TRUE)

# Total row: bold + top border
total_style <- createStyle(textDecoration = "bold", border = "top")
addStyle(wb, "Table 1", total_style,
         rows = data_end_row, cols = 1:6, stack = TRUE)

# Footnote style
fn_style <- createStyle(fontSize = 9, wrapText = TRUE)
addStyle(wb, "Table 1", fn_style,
         rows = fn_row:(fn_row + length(fn_lines) - 1), cols = 1,
         stack = TRUE)

# --- Column widths ---
setColWidths(wb, "Table 1", cols = 1, widths = 14)
setColWidths(wb, "Table 1", cols = 2, widths = 14)
setColWidths(wb, "Table 1", cols = 3, widths = 16)
setColWidths(wb, "Table 1", cols = 4, widths = 14)
setColWidths(wb, "Table 1", cols = 5, widths = 28)
setColWidths(wb, "Table 1", cols = 6, widths = 20)

saveWorkbook(wb, "output/tables/table1.xlsx", overwrite = TRUE)

cat("\n=== Table 1 Complete ===\n")
cat("Output file: output/tables/table1.xlsx\n")
