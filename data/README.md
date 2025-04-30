# Gender discrepancies at the Inquisitorial trial in Giaveno, Italy, 1335 (data overview)

![R format](https://img.shields.io/badge/data_format-R-blue)
[![ERC funding](https://img.shields.io/badge/funding-ERC-green)](https://cordis.europa.eu/project/id/101000442)

## Description

The dataset is stored in an RData file named data.RData. This file contains a list called `castellario` (named after the surname of the inquisitor overseeing the trial) with five data frame objects: `nodes`, `edges`, `keyevents`, `kinship`, and `circumstances`. Below is a detailed description of each data frame:

`nodes`: A 351 x 7 data frame providing person-level information. The variables include:
- `id`: Unique identifier.
- `label`: Name of the individual.
- `sex`: Gender of the individual ("m" for males, "f" for females).
- `dead`: Dichotomous indicator of whether the individual was deceased at the time of the trial.
- `occupation_type`: Type of profession.
- `origin_or_residence`: Place of origin or residence.
- `waldensian_master`: Dichotomous indicator of whether the individual was identified as a Waldensian master from another region.

`edges`: A 1,074 x 4 data frame documenting the denunciations reported to the inquisitor in an enriched edge list format. The columns include:
- `source`: ID of the denouncer.
- `target`: ID of the person or group being denounced. If nobody, the value reads `NA` (not applicable).
- `time`: Date when the denunciation was reported.
- `summoned`: Dichotomous indicator of whether the denouncer was summoned to appear before the inquisitor.

`keyevents`: A 166 x 3 data frame with details about key events during the trial. The columns are:
- `id`: ID of the person concerned.
- `time`: Date of the event.
- `type`: Type of event (public summons, torture, or deposition).

`kinship`: A 277 x 3 data frame containing an edge list of individuals with some form of kinship affiliation. Some kin ties were inferred following the procedure used in a previous study (see [here](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/2_Castellario_kinship_ties_and_congregations.R))
The columns include:
- `V1`: ID of person i.
- `V2`: ID of person j.
- `role`: Type of affiliation between i and j (e.g., son, wife, brother, sister).

`circumstances`: A 152 x 3 data frame containing information about aggravating circumstances concerning the individual in question. The colums are:
- `id`: ID of the person concerned.
- `time`: Date when the information was reported to the inquisitor.
- `type`: Type of aggravating circumstance (Acquaintance with Martinus, Acquaintance with Franciscus, Host of congregation)

## File list

- [data.RData](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/data/data.RData)

## Source

The data was collected from Merlo, Grado G. 1977. _Eretici e inquisitori nella società piemontese del Trecento: con l’edizione dei processi tenuti a Giaveno dall’inquisitore Alberto De Castellario (1335) e nelle Valli di Lanzo dall’inquisitore Tommaso Di Casasco (1373)_. Turin: Claudiana Editrice. 
The original Latin manuscript is held at the Archives of the Order of Preachers in Rome (Archivio generale dell’Ordine dei Predicatori, Rome, MS II.64, ff. 1r-111v).

## Contact information

For any questions, please contact:
- Davor Salihović (davor.salihovic@gmail.com)
