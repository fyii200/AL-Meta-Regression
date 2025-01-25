## Emmetropic axial length (AL) growth curve: A meta-regression of 28 studies
 
**cleaned_data.csv:** Extracted emmetrope-specific data from 28 primary studies

#### 1) ***MetaRegressionCodeTruncated.R***:
R code (runs on ***cleaned_data.csv***) used to generate ethnic-specific and combined AL growth curves (Mean AL vs Mean age) in emmetropes aged 6 to 24 years.

#### 2) ***cleaned_data.csv***:

*Cleaned dataset in CSV format comprising 19 columns and 43 rows. Each row corresponds to a specific datapoint. The codebook below summarises the content of the dataset.*

| Column name | Data type | What it represents |
| :---:   | :---: | :---: |
| **study** | Integer | Each integer represents one of the 28 studies (papers) included in the meta-regression.   |
| **dataset** | Integer | Each integer represents one of the 26 unique datasets used by the included studies. Martinez et al. and Philip et al. worked with Sydney Myopia Study data, while Li et al. and Li et al. worked with Anyang Childhood Eye Study data. Data from different age groups were used. |
| **author** | Character | Last name of first author and year of publication. |
| **title** | Character | Full title.  |
| **location** | Character | Country or city in which the participants were recruited. Note: "hk" refers to Hong Kong.  |
| **design** | Character | Study design where "cs" refers to cross-sectional, while "long" refers to longitudinal. |
| **n** | Integer | Number of eyes. |
| **female** | Integer | Number of females (if provided). |
| **male** | Integer | Number of males (if provided). |
| **AL_mean** | Numeric | Mean axial length (specific to emmetropes).  |
| **AL_sd** | Numeric | Standard deviation of axial length (specific to emmetropes) |
| **age_mean** | Numeric | Mean sample age (specific to emmetropes)   |
| **age_sd** | Numeric | Standard deviation of sample age (specific to emmetropes)   |
| **biometer** | Character | Optical biometry: IOLMaster or Lenstar |
| **cyclo** | Boolean (y/n) | If "y", cycloplegic refraction was performed; if "n" non-cycloplegic refraction was performed.   |
| **ser_mean** | Numeric | Mean spherical equivalent refraction, SER (specific to emmetropes, if provided).  |
| **ser_sd** | Numeric | Standard deviation of spherical equivalent refraction, SER (specific to emmetropes, if provided).   |
| **emm_def** | Numeric (range) | Definition of emmetropia: ±0.50D SER; -0.25D to +0.50D SER; -0.25D to +0.75D SER; -0.25D to +1.25D SER; -0.50D to +0.75D SER; -0.50D to +1.00D SER; -0.50D to +1.25D; ±0.50D spherical power. |
| **emm_code** | Integer | 1 to 8 where each number corresponds to a given definition of emmetropia (**emm_def**) following the order specified above. |

## How to cite
```

Yii F. Emmetropic eye growth in East Asians and non-East Asians. Ophthalmic Physiol Opt. 2023; 00: 1–7. https://doi.org/10.1111/opo.13195

```




