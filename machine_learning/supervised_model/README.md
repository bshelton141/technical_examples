# Supervised Model Development


## supervised_model.R

The [supervised_model.R](https://github.com/b-shelton/technical_examples/blob/master/machine_learning/supervised_model/supervised_model.R) script is a fully reproducible example to develop and compare 5 types of predictive modeling algorithms: Logistic Regression, Decision Tree, Random Forest, and Gradient Boosting. The data used is a filtered version of the "loan.csv" data found in the "lending-club-loan-data.zip" file housed on kaggle.com's site here: https://kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip. The supervised_model.R script is completely reproducible, so no manual data ingestion needs to be performed.

The goal of the script is to determine the model that is best at predicting which loans are "Charged Off" or "Fully Paid" (using AUC as the evaluation metric). This script is intended to demonstrate the following abilities:
- Data Pre-Processing
- Dimensionality Reduction
- Data Visualization
- Model Training, Testing and Validation


## transaction_fraud.ipynb

The [transaction_fraud.ipynb](https://github.com/bshelton141/technical_examples/blob/master/machine_learning/supervised_model/transaction_fraud.ipynb) script is a Jupyter notebook displaying the EDA, development, and comparison of several predictive modeling algorithms to detect credit card transaction fraud. The [transaction.py](https://github.com/bshelton141/technical_examples/blob/master/machine_learning/supervised_model/transactions.py) script is simply the code from the abovementioned Jupyter notebook, for easier reproducability in whichever Python environment.
