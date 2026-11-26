# =============================================================================
# 04_sensitivity.R
# Sensitivity analysis: English-only (excluding the hand-searched Japanese site)
#
# Main analysis uses three patient review platforms: webmd + askapatient + kusurimil.
# This sensitivity analysis excludes the hand-searched Japanese data (kusurimil, N=291)
# and re-evaluates whether the main findings hold in the English-only subset
# (webmd + askapatient, N=7,537).
# =============================================================================

library(tidyverse)
library(openxlsx)

# 01_load_data.R と 03_main_prr_forest.R を先に実行しておくこと

cat("=== Sensitivity Analysis: English-only (kusurimil除外) ===\n\n")
cat("Abbreviations: NAD, nightmares and abnormal dreams; SP, sleep paralysis\n\n")

# Helper
format_3digit <- function(x) {
  ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
}

drug_order <- c(
  "suvorexant", "lemborexant", "daridorexant",
  "ramelteon",
  "trazodone", "doxepin",
  "zolpidem", "eszopiclone", "zaleplon",
  "temazepam", "triazolam"
)

class_order <- c("DORA", "MRA", "Sedating AD", "Z-drug", "BZD")

# -----------------------------------------------------------------------------
# 英語2サイトに限定
# -----------------------------------------------------------------------------

english_sites <- c("webmd.com", "askapatient")
df_en <- df_analysis %>% filter(site %in% english_sites)

cat("English-only N:", nrow(df_en), "(メイン N:", nrow(df_analysis),
    ", 除外 kusurimil:", nrow(df_analysis) - nrow(df_en), ")\n")
cat("サイト別件数:\n")
print(df_en %>% count(site, sort = TRUE))

# -----------------------------------------------------------------------------
# 薬剤別 Table
# -----------------------------------------------------------------------------

en_drug <- df_en %>%
  group_by(drug, class) %>%
  summarise(
    n = n(),
    nad = sum(drug_related_nightmare_final == 1, na.rm = TRUE),
    nad_rate = round(mean(drug_related_nightmare_final, na.rm = TRUE) * 100, 2),
    sp = sum(drug_related_sp_final == 1, na.rm = TRUE),
    sp_rate = round(mean(drug_related_sp_final, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(drug = factor(drug, levels = drug_order)) %>%
  arrange(drug) %>%
  filter(!is.na(drug))

cat("\n--- 薬剤別 (英語のみ) ---\n")
print(en_drug, n = 15)

# -----------------------------------------------------------------------------
# 5群omnibus
# -----------------------------------------------------------------------------

df_en <- df_en %>%
  mutate(class_f = factor(class, levels = class_order))

cat("\n--- NAD Omnibus (5 classes, 英語のみ) ---\n")
nad_tbl <- table(df_en$class_f, df_en$drug_related_nightmare_final)
print(nad_tbl)
exp_nad <- suppressWarnings(chisq.test(nad_tbl))$expected
cat("Min expected count:", round(min(exp_nad), 1), "\n")

if (any(exp_nad < 5)) {
  set.seed(42)
  nad_om <- fisher.test(nad_tbl, simulate.p.value = TRUE, B = 100000)
  nad_om_method <- "Fisher-Freeman-Halton (MC, B=100,000)"
} else {
  nad_om <- chisq.test(nad_tbl)
  nad_om_method <- paste0("Chi-square (df=", nad_om$parameter, ")")
}
cat("Method:", nad_om_method, " | p =", format.pval(nad_om$p.value, digits = 3), "\n")

cat("\n--- SP Omnibus (5 classes, 英語のみ) ---\n")
sp_tbl <- table(df_en$class_f, df_en$drug_related_sp_final)
print(sp_tbl)
exp_sp <- suppressWarnings(chisq.test(sp_tbl))$expected
cat("Min expected count:", round(min(exp_sp), 1), "\n")

if (any(exp_sp < 5)) {
  set.seed(42)
  sp_om <- fisher.test(sp_tbl, simulate.p.value = TRUE, B = 100000)
  sp_om_method <- "Fisher-Freeman-Halton (MC, B=100,000)"
} else {
  sp_om <- chisq.test(sp_tbl)
  sp_om_method <- paste0("Chi-square (df=", sp_om$parameter, ")")
}
cat("Method:", sp_om_method, " | p =", format.pval(sp_om$p.value, digits = 3), "\n")

# -----------------------------------------------------------------------------
# DORA vs each class post-hoc (Holm補正)
# -----------------------------------------------------------------------------

df_dora_en <- df_en %>% filter(class == "DORA")
other_classes <- c("MRA", "Sedating AD", "Z-drug", "BZD")

posthoc_en <- map_dfr(other_classes, function(cl) {
  df_other <- df_en %>% filter(class == cl)

  dora_nm <- sum(df_dora_en$drug_related_nightmare_final == 1, na.rm = TRUE)
  dora_n  <- nrow(df_dora_en)
  other_nm <- sum(df_other$drug_related_nightmare_final == 1, na.rm = TRUE)
  other_n  <- nrow(df_other)

  nm_mat <- matrix(c(other_nm, other_n - other_nm, dora_nm, dora_n - dora_nm), nrow = 2)
  nm_fisher <- fisher.test(nm_mat)

  dora_sp  <- sum(df_dora_en$drug_related_sp_final == 1, na.rm = TRUE)
  other_sp <- sum(df_other$drug_related_sp_final == 1, na.rm = TRUE)
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

posthoc_en <- posthoc_en %>%
  mutate(
    nad_p_holm = p.adjust(nad_p_raw, method = "holm"),
    sp_p_holm  = p.adjust(sp_p_raw,  method = "holm")
  )

cat("\n--- Post-hoc: DORA vs Each Class (Holm補正, 英語のみ) ---\n")
print(posthoc_en %>% mutate(across(where(is.numeric), ~round(., 4))))

# -----------------------------------------------------------------------------
# Excel出力
# -----------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "Drug Table")
writeData(wb, "Drug Table", en_drug, startRow = 1)
addStyle(wb, "Drug Table",
         createStyle(textDecoration = "bold", border = "bottom"),
         rows = 1, cols = 1:ncol(en_drug))
setColWidths(wb, "Drug Table", cols = 1:ncol(en_drug), widths = 12)

addWorksheet(wb, "Class Omnibus")
omnibus_summary <- tibble(
  outcome = c("NAD", "SP"),
  method = c(nad_om_method, sp_om_method),
  p_value = c(format.pval(nad_om$p.value, digits = 3),
              format.pval(sp_om$p.value, digits = 3))
)
writeData(wb, "Class Omnibus", omnibus_summary, startRow = 1)
addStyle(wb, "Class Omnibus",
         createStyle(textDecoration = "bold", border = "bottom"),
         rows = 1, cols = 1:ncol(omnibus_summary))
setColWidths(wb, "Class Omnibus", cols = 1:3, widths = c(10, 40, 14))

addWorksheet(wb, "DORA vs Each (Post-hoc)")
posthoc_display <- posthoc_en %>%
  mutate(across(where(is.numeric), ~round(., 4)))
writeData(wb, "DORA vs Each (Post-hoc)", posthoc_display, startRow = 1)
addStyle(wb, "DORA vs Each (Post-hoc)",
         createStyle(textDecoration = "bold", border = "bottom"),
         rows = 1, cols = 1:ncol(posthoc_display))
setColWidths(wb, "DORA vs Each (Post-hoc)",
             cols = 1:ncol(posthoc_display), widths = 14)

saveWorkbook(wb, "output/tables/sensitivity_english_only.xlsx", overwrite = TRUE)
cat("\nSaved: output/tables/sensitivity_english_only.xlsx\n")

# -----------------------------------------------------------------------------
# 保存サマリー
# -----------------------------------------------------------------------------

cat("\n=== Sensitivity Analysis Complete ===\n")
cat("Tables:\n")
cat("  - output/tables/sensitivity_english_only.xlsx\n")
