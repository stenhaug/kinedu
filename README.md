# cogsci submission (5/29/2020)

This repo produces the paper ["The latent factor structure of developmental change in early childhood"](cogsci/_submission_final/thelatent_0529.pdf) by Ben Stenhaug & Mike Frank. Data is made available by [Kinedu Inc.](https://www.kinedu.com/)

The paper can be reproduced as follows:

1. [data-clean/data-clean.Rmd](data-clean/data-clean.Rmd) is the code that takes the raw data (not provided) and produces the rds files in [data-clean](data-clean) (these are <100mb so they are included in this repo)

2. Each of the Rmd files in [data-models](data-models) ([models_exploratory.Rmd](data-models/models_exploratory.Rmd), [models_bifactor.Rmd](data-models/models_bifactor.Rmd), and [age_partitioned_models.Rmd](data-models/age_partitioned_models.Rmd)) contain the code to produce the models used in the paper. However, these take about one day to fit on a local machine. We have uploaded our saved models to [figshare](https://figshare.com/projects/The_latent_factor_structure_of_developmental_change_in_early_childhood/75189) which you should download and place in the [data-models](data-models) folder.

3. [cogsci/cogsci.Rmd](cogsci/cogsci.Rmd) is the main file which knits to create the paper.
