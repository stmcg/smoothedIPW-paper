# Data Dictionary

## Core Variables

| Variable Name     | Description |
|-------------------|-------------|
| `subjid`          | Patient ID |
| `month`           | Time interval *k* |
| `treatment`       | Medication initiated at baseline |
| `weight_t1_ind`   | Indicator of weight measurement in interval *k+1* |
| `wtchg`           | Weight in interval *k+1* minus baseline |
| `wtchg_ge5_ind_t` | Indicator of weight change ≥ 5% (interval *k+1* vs. baseline) |
| `death_t1`        | Indicator of death in interval *k+1* |
| `grace_end_t`     | Indicator of a patient being at the end of their grace period (i.e., those who in interval *k−1* were in the last month of their prescription) |
| `censor_grace`    | Indicator of whether a patient should be artificially censored due to violating the treatment strategy |

---

## Baseline-Only Covariates

| Variable Name | Description |
|---------------|-------------|
| `age_t0`      | Age at baseline |
| `female`      | Sex |
| `race_white`, `race_black`, `race_aapi`, `race_other_multiple` | Race categories: White, Black, Asian American/Pacific Islander, Other/Multiple |
| `hispanic`    | Hispanic ethnicity |
| `site`        | Site |
| `year_t0`     | Year of initiation |

---

## Time-Varying Covariates

| Variable Name | Description |
|---------------|-------------|
| `bmi_t0`, `bmi_t` | BMI at baseline (`_t0`) and follow-up (`_t`) |
| `wtchg_6mo_t0_catgain`, `wtchg_6mo_t0_catloss`, `wtchg_6mo_t0_catsame`, `wtchg_6mo_t_catgain`, `wtchg_6mo_t_catloss`, `wtchg_6mo_t_catsame` | Weight change in the last 6 months categories at baseline (`_t0`) and follow-up (`_t`): weight gain, weight loss, no weight change |
| `medicaid_t0`, `medicaid_t1` | Medicaid status at baseline (`_t0`) and follow-up (`_t`) |
| `smoke_t0`, `smoke_t1` | Smoking status at baseline (`_t0`) and follow-up (`_t`) |
| `cov_wtloss_t0`, `cov_wtloss_t` | Weight loss medication prescription at baseline (`_t0`) and follow-up (`_t`) |
| `cov_stimulants_t0`, `cov_stimulants_t` | Stimulants prescription at baseline (`_t0`) and follow-up (`_t`) |
| `cov_steroid_t0`, `cov_steroid_t` | Steroid prescription at baseline (`_t0`) and follow-up (`_t`) |
| `AED_rxlt15_t0`, `AED_rxlt15` | Antiseizure medication prescription <15 months ago at baseline (`_t0`) and follow-up |
| `FGA_rxlt15_t0`, `FGA_rxlt15` | First-generation antipsychotics prescription <15 months ago at baseline (`_t0`) and follow-up |
| `SGA_rxlt15_t0`, `SGA_rxlt15` | Second-generation antipsychotics prescription <15 months ago at baseline (`_t0`) and follow-up |
| `Insulin_rxlt15_t0`, `Insulin_rxlt15` | Insulin prescription <15 months ago at baseline (`_t0`) and follow-up |
| `Biguanide_txlt15_t0`, `Biguanide_txlt15` | Biguanide prescription <15 months ago at baseline (`_t0`) and follow-up |
| `GLP1_txlt15_t0`, `GLP1_txlt15` | GLP1 prescription <15 months ago at baseline (`_t0`) and follow-up |
| `SGLT2_txlt15_t0`, `SGLT2_txlt15` | SGLT2 prescription <15 months ago at baseline (`_t0`) and follow-up |
| `OtherDIAB_txlt15_t0`, `OtherDIAB_txlt15` | Other diabetes medication prescription <15 months ago at baseline (`_t0`) and follow-up |
| `HTN_txlt15_t0`, `HTN_txlt15` | Antihypertensive prescription <15 months ago at baseline (`_t0`) and follow-up |
| `t_otherrx` | Antidepressant prescription other than the initiated medication in current month |
| `sum_encounter_t0_cat1`, `sum_encounter_t0_cat2`, `sum_encounter_t0_cat3`, `sum_encounter_t0_cat4`, `sum_encounter_cat1`, `sum_encounter_cat2`, `sum_encounter_cat3`, `sum_encounter_cat4` | Number of months with a healthcare encounter (last 6 months) at baseline (`_t0`) and follow-up:<br>• `cat1`: 0 months<br>• `cat2`: 1–2 months<br>• `cat3`: 3–4 months<br>• `cat4`: 5–6 months |

---

## Time-Varying Covariates (Diagnosis <12 vs ≥12 Months)

Example variables:  

- `Hyperthyroidism_dxlt12_t0`, `Hyperthyroidism_dxge12_t0`, `Hyperthyroidism_dxlt12`, `Hyperthyroidism_dxge12`  
  → Hyperthyroidism diagnosis less than 12 months ago (`lt12`) or ≥12 months ago (`dxge12`) at baseline (`_t0`) and follow-up.

The same naming convention applies to:  

- **Hypothyroidism** (`Hypothyroidism_dxlt12`, `Hypothyroidism_dxge12`, …)  
- **Type 1 diabetes** (`T1D`)  
- **Type 2 diabetes** (`T2D`)  
- **Growth conditions** (`Growth_conditions`)  
- **Abnormal glucose** (`Abnormal_glucose`)  
- **Polycystic ovary syndrome** (`PCOS`)  
- **Depression** (`Depression`)  
- **Obsessive compulsive disorder** (`OCD`)  
- **Post-traumatic stress disorder** (`PTSD`)  
- **Eating disorders** (`Eating_disorders`)  
- **Bipolar disorder** (`Bipolar`)  
- **Anxiety** (`Anxiety`)  
- **Mental health disorders** (`Mental_health`)  
- **Neuropathic pain** (`Neuropathic_pain`)  
- **Migraine** (`Migraine`)  
- **Attention deficit hyperactivity disorder** (`ADHD`)  
- **Asthma** (`Asthma`)  
- **Liver cirrhosis** (`Liver_Cirrhosis`)  
- **Heart failure** (`Heart_Failure`)  
- **Chronic kidney disease** (`Chronic_Kidney_Disease`)  
- **Charlson comorbidity index >1** (`CCI_Comorb`)  
- **Alcohol abuse** (`Alcohol_abuse`)  
- **Cancer** (`cancer`)  
- **Bariatric surgery** (`bariatric`)  
- **Pregnancy** (`pregnant`)  