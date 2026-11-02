# =============================================================================
# 01_load_data.R
# データ読み込みと基本統計の確認
# =============================================================================

# パッケージ読み込み
library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# 1. データ読み込み
# -----------------------------------------------------------------------------

# データパス（R_analysisフォルダから実行する場合）
data_path <- here("data", "merged_reviews.csv")

# もしhereが使えない場合の代替パス
if (!file.exists(data_path)) {
  data_path <- "data/merged_reviews.csv"
}

cat("Loading data from:", data_path, "\n")

# CSV読み込み
df <- read_csv(data_path, show_col_types = FALSE)

cat("\n=== データ読み込み完了 ===\n")
cat("総レビュー数:", nrow(df), "\n")

# -----------------------------------------------------------------------------
# 2. 基本統計
# -----------------------------------------------------------------------------

cat("\n=== 基本統計 ===\n")

# 解析対象の確認
cat("\n【除外フラグ別】\n")
print(table(df$excluded, useNA = "ifany"))

# 解析対象のみ抽出
df_analysis <- df %>% filter(excluded == 0)
cat("\n解析対象レビュー数:", nrow(df_analysis), "\n")

# サイト別
cat("\n【サイト別】\n")
print(df_analysis %>% count(site, sort = TRUE))

# 薬剤別
cat("\n【薬剤別】\n")
print(df_analysis %>% count(drug, sort = TRUE))

# 薬剤クラスの定義
drug_class <- tribble(
  ~drug,          ~class,
  "suvorexant",   "DORA",
  "lemborexant",  "DORA",
  "daridorexant", "DORA",
  "eszopiclone",  "Z-drug",
  "zaleplon",     "Z-drug",
  "zolpidem",     "Z-drug",
  "triazolam",    "BZD",
  "temazepam",    "BZD",
  "ramelteon",    "MRA",
  "doxepin",      "Sedating AD",
  "trazodone",    "Sedating AD"
)

# クラス情報を結合
df_analysis <- df_analysis %>%
  left_join(drug_class, by = "drug")

# クラス別
cat("\n【薬剤クラス別】\n")
print(df_analysis %>% count(class, sort = TRUE))

# -----------------------------------------------------------------------------
# 3. 離脱関連の処理とアウトカム変数の確定
# -----------------------------------------------------------------------------

cat("\n=== 離脱関連 (withdrawal_related_final) ===\n")

if ("withdrawal_related_final" %in% colnames(df_analysis)) {
  cat("離脱疑い (=1):", sum(df_analysis$withdrawal_related_final == 1, na.rm = TRUE), "件\n")
  cat("非該当 (=0):", sum(df_analysis$withdrawal_related_final == 0, na.rm = TRUE), "件\n")

  # 離脱除外後のアウトカム変数を作成し、メイン変数として上書き
  df_analysis <- df_analysis %>%
    mutate(
      nightmare_excl_withdrawal = ifelse(
        drug_related_nightmare_final == 1 & withdrawal_related_final != 1, 1, 0
      ),
      sp_excl_withdrawal = ifelse(
        drug_related_sp_final == 1 & withdrawal_related_final != 1, 1, 0
      ),
      # メイン変数を離脱除外後の値で上書き
      drug_related_nightmare_final = nightmare_excl_withdrawal,
      drug_related_sp_final = sp_excl_withdrawal
    )

} else {
  cat("WARNING: withdrawal_related_final カラムが見つかりません\n")
  df_analysis <- df_analysis %>%
    mutate(
      nightmare_excl_withdrawal = drug_related_nightmare_final,
      sp_excl_withdrawal = drug_related_sp_final
    )
}

# -----------------------------------------------------------------------------
# 4. アウトカム変数の確認
# -----------------------------------------------------------------------------

cat("\n=== アウトカム変数（離脱除外後） ===\n")

cat("\n【悪夢 (drug_related_nightmare_final)】\n")
cat("陽性:", sum(df_analysis$drug_related_nightmare_final, na.rm = TRUE), "\n")
cat("報告率:",
    round(mean(df_analysis$drug_related_nightmare_final, na.rm = TRUE) * 100, 2), "%\n")

cat("\n【睡眠麻痺 (drug_related_sp_final)】\n")
cat("陽性:", sum(df_analysis$drug_related_sp_final, na.rm = TRUE), "\n")
cat("報告率:",
    round(mean(df_analysis$drug_related_sp_final, na.rm = TRUE) * 100, 2), "%\n")

# -----------------------------------------------------------------------------
# 5. 薬剤別サマリー
# -----------------------------------------------------------------------------

cat("\n=== 薬剤別サマリー ===\n")

drug_summary <- df_analysis %>%
  group_by(drug, class) %>%
  summarise(
    n = n(),
    nightmare = sum(drug_related_nightmare_final, na.rm = TRUE),
    nightmare_rate = mean(drug_related_nightmare_final, na.rm = TRUE) * 100,
    sp = sum(drug_related_sp_final, na.rm = TRUE),
    sp_rate = mean(drug_related_sp_final, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(nightmare_rate))

print(drug_summary, n = 20)

# -----------------------------------------------------------------------------
# 6. Rating (10点満点) の確認
# -----------------------------------------------------------------------------

cat("\n=== Rating (5-point scale) ===\n")

if ("rating" %in% colnames(df_analysis)) {
  cat("rating カラム: あり\n")
  cat("非欠損:", sum(!is.na(df_analysis$rating)), "/", nrow(df_analysis), "\n")
  cat("Mean:", round(mean(df_analysis$rating, na.rm = TRUE), 2), "\n")
  cat("SD:", round(sd(df_analysis$rating, na.rm = TRUE), 2), "\n")

  # サイト別
  cat("\nサイト別 Rating:\n")
  print(df_analysis %>%
          group_by(site) %>%
          summarise(
            n = n(),
            mean_rating = round(mean(rating, na.rm = TRUE), 2),
            .groups = "drop"
          ))
} else {
  cat("WARNING: rating カラムが見つかりません\n")
}

# -----------------------------------------------------------------------------
# 7. データ保存（後続スクリプト用）
# -----------------------------------------------------------------------------

# グローバル環境に保存
cat("\n=== データ準備完了 ===\n")
cat("df_analysis: 解析対象データ (", nrow(df_analysis), "件)\n")
cat("drug_summary: 薬剤別サマリー\n")
cat("drug_class: 薬剤クラス定義\n")
