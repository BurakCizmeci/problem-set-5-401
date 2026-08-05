# ============================================================
# PSIR401 / POLS532 – Short Assignment V
# Dataset: PSIRS401-POLS532 Short Assignment V 052126.xlsx
# ============================================================

# Profesyonel R Markdown / Quarto Raporlaması İçin Gerekli Paketler
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------
# Load data (Dosya yolun ve orijinal verin tamamen korundu)
# ------------------------------------------------------------------
df <- read_excel("/Users/burak/Desktop/assignment5.xlsx")

# ==================================================================
# QUESTION 1: Frequency Tables (Raw / Before Recoding)
# ==================================================================

# ── f1101: Anger toward SEC ───────────────────────────────────────
f1101_labels <- c("1" = "No Anger", "2" = "2", "3" = "3", "4" = "4", 
                  "5" = "A Lot of Anger", "99" = "Don't Know/No Answer")

f1101_raw <- as.data.frame(table(Value = df$f1101, useNA = "ifany")) %>%
  rename(Frequency = Freq) %>%
  mutate(
    Label   = recode(as.character(Value), !!!f1101_labels, .default = as.character(Value)),
    Percent = round(Frequency / sum(Frequency) * 100, 2),
    `Cum. %` = round(cumsum(Percent), 2)
  ) %>%
  select(Value, Label, Frequency, Percent, `Cum. %`)

# KableExtra ile Şık Tablo Çıktısı
f1101_raw %>%
  kbl(caption = "f1101 – Anger toward the Supreme Election Council (1=No Anger … 5=A Lot of Anger)", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)

# ── e03: Probability of Voting ────────────────────────────────────
e03_labels <- c("0" = "Definitely Not Voting", "10" = "Definitely Voting", "99" = "Don't Know/No Answer")

e03_raw <- as.data.frame(table(Value = df$e03, useNA = "ifany")) %>%
  rename(Frequency = Freq) %>%
  mutate(
    Label   = recode(as.character(Value), !!!e03_labels, .default = as.character(Value)),
    Percent = round(Frequency / sum(Frequency) * 100, 2),
    `Cum. %` = round(cumsum(Percent), 2)
  ) %>%
  select(Value, Label, Frequency, Percent, `Cum. %`)

e03_raw %>%
  kbl(caption = "e03 – Probability of Voting in June 23 Elections (0=Definitely Not … 10=Definitely)", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 2 & 3: Recode Missing Values, Cross-Tab & Chi-Squared Test
# ==================================================================

df <- df %>%
  mutate(
    f1101_clean = if_else(f1101 == 99, NA_real_, f1101),
    e03_clean   = if_else(e03   == 99, NA_real_, e03)
  )

# Ki-Kare Testi Hesaplaması
cross_tab <- table(`f1101 (Anger->SEC)` = df$f1101_clean, `e03 (Prob.Vote)` = df$e03_clean, useNA = "no")
chi_result <- chisq.test(cross_tab)

# Kritik Değer Hesaplamaları
df_chi   <- chi_result$parameter
alpha    <- 0.05
chi_crit <- qchisq(1 - alpha, df = df_chi)

# Test Sonuçlarını Arkadaşının Formatında Tablolaştırma
data.frame(
  Statistic = c("Observed Chi-Squared", "Degrees of Freedom", "Critical Chi-Squared (alpha = 0.05)", "p-value"),
  Value     = c(round(chi_result$statistic, 3), df_chi, round(chi_crit, 3), format.pval(chi_result$p.value, digits = 3))
) %>%
  kbl(caption = "Chi-squared Test: Anger toward SEC and Probability of Voting", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 4: Cross-tabulation c01 (Party Closest To) × e0402
# ==================================================================

party_labels <- c("1" = "AK Parti", "2" = "CHP", "3" = "HDP", "4" = "IYI Party", 
                  "5" = "MHP", "6" = "SP", "90" = "Other", "96" = "None / No Party", "99" = "DK / NA")

df <- df %>%
  mutate(c01_label = recode(as.character(c01), !!!party_labels, .default = "Other"))

cross_tab_q4 <- table(`c01 (Party)` = df$c01_label, `e0402 (Prob->İmamoğlu)` = df$e0402, useNA = "ifany")

# Karmaşık konsol çıktısı yerine kable ile matris görünümü
cross_tab_q4 %>%
  as.data.frame.matrix() %>%
  kbl(caption = "Party Closest to Self by Probability to Vote for Imamoglu (Raw)", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 9) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 5: Create Trichotomous 'pid' Variable
# ==================================================================

df <- df %>%
  mutate(
    pid = case_when(
      c01 %in% c(1, 5) ~ 1,        # AK Parti or MHP
      c01 %in% c(2, 4, 6) ~ 2,     # CHP, SP, IYI
      c01 == 3 ~ 3,                 # HDP
      TRUE ~ NA_real_
    ),
    pid_label = case_when(
      pid == 1 ~ "AKP / MHP",
      pid == 2 ~ "CHP / IYI / SP",
      pid == 3 ~ "HDP",
      TRUE     ~ NA_character_
    )
  )

pid_tab <- as.data.frame(table(pid = df$pid_label, useNA = "ifany")) %>%
  rename(Frequency = Freq)

pid_tab %>%
  kbl(caption = "Trichotomous Party ID Variable (pid)", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 6: Descriptive Statistics of e0402 by pid Group
# ==================================================================

df <- df %>%
  mutate(e0402_clean = if_else(e0402 == 99, NA_real_, e0402))

desc_q6 <- df %>%
  filter(!is.na(pid)) %>%
  group_by(`Party ID` = pid_label) %>%
  summarise(
    Mean = round(mean(e0402_clean, na.rm = TRUE), 3),
    SD   = round(sd(e0402_clean,   na.rm = TRUE), 3),
    N    = sum(!is.na(e0402_clean)),
    .groups = "drop"
  )

desc_q6 %>%
  kbl(caption = "Probability to Vote for Imamoglu by Party ID", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 7: T-Test (Equal Variances) – HDP vs. People's Alliance
# ==================================================================

df <- df %>%
  mutate(
    hdp_vs_pa = case_when(
      pid == 3 ~ 1,   # HDP
      pid == 1 ~ 0,   # People's Alliance
      TRUE     ~ NA_real_
    )
  )

q7_data <- df %>% filter(!is.na(hdp_vs_pa) & !is.na(e0402_clean))
ttest_eq <- t.test(e0402_clean ~ hdp_vs_pa, data = q7_data, var.equal = TRUE)

# T-test sonuçlarını kable formatına sokma
data.frame(
  Statistic = c("Mean (People's Alliance = 0)", "Mean (HDP = 1)", "Mean difference", "t-statistic", "Degrees of freedom", "p-value", "95% CI (lower)", "95% CI (upper)"),
  Value     = c(round(ttest_eq$estimate[1], 3), round(ttest_eq$estimate[2], 3), round(ttest_eq$estimate[1] - ttest_eq$estimate[2], 3), round(ttest_eq$statistic, 3), round(ttest_eq$parameter, 2), format.pval(ttest_eq$p.value, digits = 3), round(ttest_eq$conf.int[1], 3), round(ttest_eq$conf.int[2], 3))
) %>%
  kbl(caption = "Equal Variances t-test: Probability to Vote for Imamoglu", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 8: Welch T-Test (Unequal Variances)
# ==================================================================

ttest_uneq <- t.test(e0402_clean ~ hdp_vs_pa, data = q7_data, var.equal = FALSE)

data.frame(
  Statistic = c("Mean (People's Alliance = 0)", "Mean (HDP = 1)", "Mean difference", "t-statistic", "Degrees of freedom", "p-value", "95% CI (lower)", "95% CI (upper)"),
  Value     = c(round(ttest_uneq$estimate[1], 3), round(ttest_uneq$estimate[2], 3), round(ttest_uneq$estimate[1] - ttest_uneq$estimate[2], 3), round(ttest_uneq$statistic, 3), round(ttest_uneq$parameter, 2), format.pval(ttest_uneq$p.value, digits = 3), round(ttest_uneq$conf.int[1], 3), round(ttest_uneq$conf.int[2], 3))
) %>%
  kbl(caption = "Unequal Variances t-test", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 9: akp_dum Variable & Mean Prob. to Vote for Yıldırım
# ==================================================================

df <- df %>%
  mutate(
    e0401_clean = if_else(e0401 == 99, NA_real_, e0401),
    akp_dum = case_when(
      c05 == 1 ~ 1,   # AK Parti partisan
      c05 == 5 ~ 0,   # MHP partisan
      TRUE     ~ NA_real_
    )
  )

# Geniş formatta (Wide Pivot) Koşullu Ortalamalar Tablosu
means_q9 <- df %>%
  filter(!is.na(akp_dum) & !is.na(f1101_clean) & !is.na(e0401_clean)) %>%
  group_by(Party = if_else(akp_dum == 1, "Mean (AKP)", "Mean (MHP)"), f1101_clean) %>%
  summarise(Mean_e0401 = round(mean(e0401_clean, na.rm = TRUE), 3), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = f1101_clean, values_from = Mean_e0401) %>%
  rename(`Anger toward SEC (1 No, 5 A Lot)` = Party)

means_q9 %>%
  kbl(caption = "Mean Probability to Vote for Yildirim by Anger toward SEC and Party", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)


# ==================================================================
# QUESTION 10: Line Plot of Mean Probabilities (Orijinal Renklerin Korundu)
# ==================================================================

plot_data <- df %>%
  filter(!is.na(akp_dum) & !is.na(f1101_clean) & !is.na(e0401_clean)) %>%
  group_by(f1101_clean, akp_dum) %>%
  summarise(Mean_e0401 = mean(e0401_clean), .groups = "drop") %>%
  mutate(Partisanship = factor(akp_dum, levels = c(0, 1), labels = c("MHP supporters", "AKP supporters")))

p10 <- ggplot(plot_data, aes(x = f1101_clean, y = Mean_e0401, color = Partisanship, group = Partisanship, shape = Partisanship)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Anger toward the Supreme Election Council (1 = No Anger, 5 = A Lot)", breaks = 1:5) +
  scale_y_continuous(name = "Mean Probability to Vote for Yildirim (0-10)", limits = c(0, 10), breaks = seq(0, 10, 2)) +
  scale_color_manual(values = c("AKP supporters" = "#E69500", "MHP supporters" = "#B00020")) + # Senin Orijinal Renklerin
  labs(title = "Mean Probability to Vote for Yildirim", subtitle = "Higher values indicate stronger intention to vote for Yildirim") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

print(p10)
ggsave("Q10_mean_prob_plot.png", plot = p10, width = 7, height = 5, dpi = 150)


# ==================================================================
# BONUS: Histograms (%) of f1101 for AKP and MHP Partisans
# ==================================================================

bonus_tab <- df %>%
  filter(!is.na(akp_dum) & !is.na(f1101_clean)) %>%
  mutate(Group = factor(akp_dum, levels = c(1, 0), labels = c("AKP supporters", "MHP supporters"))) %>%
  group_by(Group, f1101_clean) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p_bonus <- ggplot(bonus_tab, aes(x = factor(f1101_clean), y = pct, fill = Group)) +
  geom_col(show.legend = FALSE, color = "white", width = 0.75) +
  facet_wrap(~ Group) +
  scale_fill_manual(values = c("AKP supporters" = "#E69500", "MHP supporters" = "#B00020")) + # Senin Orijinal Renklerin
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.4, size = 3) +
  scale_y_continuous(limits = c(0, max(bonus_tab$pct) * 1.15)) +
  labs(x = "Anger toward the Supreme Election Council (1 = No Anger, 5 = A Lot)", y = "Percent of Group (%)", 
       title = "Distribution of Anger toward the Supreme Election Council", subtitle = "Within-group percentages, by bypassing") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

print(p_bonus)
