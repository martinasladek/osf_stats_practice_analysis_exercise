# RMT: Statistical practice: Analysis Exercise

This repository contains the data and the materials for Chatper 3 of the thesis The Role of Robust Statistical Methods in the Credibility Movement of Psychological Science. The associated OSF page can be found here: <https://osf.io/72gf3/>.

The repository is organised as follows:

-   `r_docs`

    -   `data_cleaning` contains the script that processes researchers' practice as coded by MS and VP

    -   `analysis_imputation` contains the script used for running multiple imputation

    -   `analysis_models` takes the imputed datasets and runs statistical models reported in the paper.

    -   `analysis_smmaries` works with the models to generate summaries of the results

-   `data`

    -   `raw_data`

        -   `statistical_practice_2021_analysis_exercise_coding` contains the dataset with coded researchers' practice

    -   `processed_data`

        -   `analysis_exercise_processed_data` - is the original data file for all participants processed by `data_cleaning` script. It also contains all the demographic information.

        -   `imp_data_0` is the original data file from the imputed list returned by the `mice` R package.

        -   `imp_data_long` - imputed dataset used for fitting the models reported in the paper.

-   `scripts`

    -   `helpers.R` - a collection of small helper functions.
