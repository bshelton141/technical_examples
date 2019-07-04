
# ---------------------------------------
# Load the data
# ---------------------------------------

# Read the zip file from the https link
import urllib.request
from zipfile import ZipFile
from io import BytesIO

url_loc = urllib.request.urlopen('https://s3.us-east-2.amazonaws.com/example.data/transactions.zip')
zfile = ZipFile(BytesIO(url_loc.read()))

# Create the temporary directory to store the zip file's content
import tempfile

temp_dir = tempfile.TemporaryDirectory()

# Extract the zip file's content into the temporary directory
zfile.extractall(temp_dir.name)

# Read the zip file's JSON content into a Pandas dataframe
import os
import pandas as pd

json_path = (temp_dir.name + '/' + os.listdir(temp_dir.name)[0])
data = pd.read_json(json_path, lines = True)

# Close the temporary directory
import shutil
shutil.rmtree(temp_dir.name)


# ---------------------------------------
# Explore the data
# ---------------------------------------

data.shape

# Check for null values
null_count = data.isnull().sum()

# Check for empty string values
empty_string_count = (data.values == '').sum(axis = 0)

# Check for med_provname_choice values
unique_values = data.nunique()

# Put the three above items together
explore_df = pd.DataFrame({'null_count': null_count,
          'empty_string_count': empty_string_count,
          'unique_values': unique_values})\
.reset_index()\
.sort_values(by = ['empty_string_count', 'unique_values'], ascending = False)

# Check for freqRatio,
# which is the ratio of the first most frequent value compared to the second most frequent
import numpy as np

frequency_ratios = pd.DataFrame([])
def keep_top2(column_name):
    try:
        f2 = data.groupby(column_name).size().reset_index()
        f2.columns = ['value', 'count']
        f3 = f2.nlargest(2, 'count')
        f4 = pd.DataFrame({'index': [column_name], 'freqRatio': f3['count'].iloc[0]/f3['count'].iloc[1]})
        return f4
    except:
        f4 = pd.DataFrame({'index': [column_name], 'freqRatio': np.nan})
        return f4

for i in data.columns:
    f5 = keep_top2(i)
    frequency_ratios = frequency_ratios.append(f5)

# Merge the freqRatio with explore_df and print
pd.merge(explore_df, frequency_ratios, on = 'index')

# Evaluate summary statistics of relevant numeric columns
data['creditLimit'] = data['creditLimit'].astype(np.float64)
num_data = data.select_dtypes(include = ['float64'])
num_data.describe()

# Print the correlation matrix of relevant numeric columns
num_data.corr()


# ---------------------------------------
# Creating a Histogram of `transactionAmount`
# ---------------------------------------

import matplotlib.pyplot as plt
import scipy.stats

def histogram_creator(column_name, transformation):
    if transformation == 'box cox':
        x = pd.Series(scipy.stats.boxcox(data[column_name] + .01)[0])
        plot_title = 'Box Cox Distribution of Individual Transaction Amounts'
        x_label = 'Box Cox Transformed Transaction Amount'
        text_x = 15; text_y = 12500
    else:
        x = data[column_name]
        plot_title = 'Distribution of Individual Transaction Amounts'
        x_label = 'Transaction Dollar Amount'
        text_x = 750; text_y = 50000


    result = plt.hist(x, bins = 100, color = 'w', edgecolor = 'k')
    plt.axvline(x.mean(),
                color = 'r',
                linestyle = 'dashed',
                linewidth = 1,
                label = ('Mean: ' + str(round(x.mean(), 2))))
    plt.axvline(x.median(),
                color = 'b',
                linestyle = 'dashed',
                linewidth = 1,
                label = ('Median: ' + str(round(x.median(), 2))))
    plt.legend(loc = 'upper right')
    plt.text(text_x, text_y,
             ('Skewness Coeffiecient: ' +
              str(round(scipy.stats.skew(x), 1))),
             bbox = dict(boxstyle = 'square', alpha = 0.5))
    plt.title(plot_title, color = 'k')
    plt.xlabel(x_label, color = 'k')
    plt.ylabel('Count of Transactions', color = 'k')

histogram_creator(column_name = 'transactionAmount',
                  transformation = 'none')

histogram_creator(column_name = 'transactionAmount',
                  transformation = 'box cox')


# ---------------------------------------
# Data Wrangling: Identifying Reversals
# ---------------------------------------

# Filter for just reversals transaction types
reversals = data[data['transactionType'] == 'REVERSAL']

# Summarize reversals and their prevelance in the larger data set
pd.DataFrame({'reversal_count': [len(reversals)],
              'pct_of_total': [round(len(reversals) / len(data), 3)],
              'reversal_dollars': [round(sum(reversals['transactionAmount']), 2)],
              'pct_of_total_dollars': [round(sum(reversals['transactionAmount']) / sum(data['transactionAmount']), 3)]},
             columns = ['reversal_count', 'pct_of_total', 'reversal_dollars', 'pct_of_total_dollars'])

# Create a way to identify both reversals and the transaction it was associated with.
data['transactionKey'] = (data['accountNumber'].map(str) + '_' +
                          data['merchantName'].map(str) + '_' +
                          data['transactionDateTime'].str.slice(0, 10).map(str) + '_' + # get just the date, not the timestamp portion
                          data['transactionAmount'].map(str))

data['datelessTransactionKey'] = (data['accountNumber'].map(str) + '_' +
                                  data['merchantName'].map(str) + '_' +
                                  data['transactionAmount'].map(str))

data = data.sort_values(['datelessTransactionKey', 'merchantName', 'transactionDateTime'])

data['rev_association'] = np.where((((data['datelessTransactionKey'].shift(-1) == data['datelessTransactionKey']) &
                                     (data['transactionType'].shift(-1) == "REVERSAL")) |
                                    (data['transactionType'] == 'REVERSAL')),
                                   '1',
                                   '0')

data[data['rev_association'] == '1'][['accountNumber',
                                      'transactionDateTime',
                                      'merchantName',
                                      'transactionAmount',
                                      'transactionType',
                                      'rev_association']].head(8)

# Summarize reversal-associated transactions and their prevelance in the larger data set
reversal_assc = data[data['rev_association'] == '1']

pd.DataFrame({'reversal_assc_count': [len(reversal_assc)],
              'pct_of_total': [round(len(reversal_assc) / len(data), 3)],
              'reversal_assc_dollars': [round(sum(reversal_assc['transactionAmount']), 2)],
              'pct_of_total_dollars': [round(sum(reversal_assc['transactionAmount']) / sum(data['transactionAmount']), 3)]},
             columns = ['reversal_assc_count', 'pct_of_total', 'reversal_assc_dollars', 'pct_of_total_dollars'])


# ---------------------------------------
# Data Wrangling: Identifying Multi-Swipe Transactions
# ---------------------------------------
# For this exercise, we will consider a multi-swipe to be a uplicate charge of a customer's card by:
# the same merchant, for the same transactionAmount, within a five minute window.

# Format the transactionDateTime field to dtype datetime64.
data['transactionDateTime'] = pd.to_datetime(data['transactionDateTime'])

# Calculate the number of minutes betwen swipes for the same transactionKey.
data['time_from_dupe'] = np.where((data['transactionKey'].shift(1) == data['transactionKey']),
                                  (data['transactionDateTime']-(data['transactionDateTime'].shift(1))).astype('timedelta64[m]'),
                                   np.nan)

# Identify all records where the time of the duplpicate transaction was within 5 min. of
# the same account, merchant, and amount, AND is not a 'REVERSAL'.
data['multi_swipe'] = np.where((data['time_from_dupe'] >= 5) | (pd.isnull(data['time_from_dupe'])) | (data['transactionType'] == 'REVERSAL'),
                                   '0',
                                   '1')

# Identify unique transactionKey values associated with multi-swipes.
multi_swipe_keys = data[data['multi_swipe'] == '1']['transactionKey'].drop_duplicates()

# View transactions that have the same transactionKey as a multi-swipe key.
data[data['transactionKey'].isin(multi_swipe_keys)][['customerId',
                                                    'transactionDateTime',
                                                    'merchantName',
                                                    'transactionAmount',
                                                    'transactionType',
                                                    'multi_swipe']].head(8)

# Summarize multi-swipe transactions and their prevelance in the larger data set
multi_swipe = data[data['multi_swipe'] == '1']

pd.DataFrame({'multi_swipe_count': [len(multi_swipe)],
              'pct_of_total': [round(len(multi_swipe) / len(data), 3)],
              'multi_swipe_dollars': [round(sum(multi_swipe['transactionAmount']), 2)],
              'pct_of_total_dollars': [round(sum(multi_swipe['transactionAmount']) / sum(data['transactionAmount']), 3)]},
             columns = ['multi_swipe_count', 'pct_of_total', 'multi_swipe_dollars', 'pct_of_total_dollars'])


# ---------------------------------------
# ---------------------------------------
# Machine Learning Development
# ---------------------------------------
# ---------------------------------------


# ---------------------------------------
# Exploratory Data Analysis - Trending over Time
# ---------------------------------------

# View the balance (or imblance) of the 'isFraud' field
data['isFraud'].value_counts()
# View the percent of records that where `'isFraud' == True`
round(len(data[data['isFraud'] == True]) / len(data), 4)

# Display non-Fraud and Fraud transaction volumes over time
import seaborn as sns

def volume_over_time(timepart):
    overtime = data.groupby([getattr(data['transactionDateTime'].dt, timepart), 'isFraud']).size().reset_index()
    overtime.columns = [('transaction_'+ timepart), 'isFraud', 'transactionCount']

    g = sns.FacetGrid(overtime, col = 'isFraud', col_wrap = 2, sharey = False)
    g.map(plt.plot, ('transaction_'+ timepart), 'transactionCount')
    g.axes[0].set_ylim(0)
    g.axes[1].set_ylim(0)
    g.set_xticklabels(rotation = 45)

for i in ['month', 'dayofweek', 'day', 'hour']:
    volume_over_time(i)

# Display the percentage of fraud transactions over time
def percent_fraud_time(timepart):
    perc_overtime1 = data.groupby([getattr(data['transactionDateTime'].dt, timepart), 'isFraud']).agg({'transactionAmount': 'count'})
    perc_overtime2 = data.groupby(getattr(data['transactionDateTime'].dt, timepart)).agg({'transactionAmount': 'count'})
    perc_overtime = perc_overtime1.div(perc_overtime2, level = 'transactionDateTime').reset_index()
    perc_overtime.columns = [('transaction_' + timepart), 'isFraud', 'fraud_percentage']
    perc_overtime = perc_overtime[perc_overtime['isFraud'] == 1]

    plt.plot(perc_overtime[('transaction_' + timepart)], perc_overtime['fraud_percentage'], color='b')
    plt.title('Percentage of Fraudulent Transactions over Time', color = 'k')
    plt.xlabel((timepart + ' of 2016'), color = 'k')
    plt.ylabel('Fraud % of Transactions', color = 'k')

percent_fraud_time('month')


# ---------------------------------------
# Feature Engineering - Parts of the transactionDateTime
# ---------------------------------------

# Add fields to indicate the month, dayofweek, and hour of each transaction
data['transactionMonth'] = data['transactionDateTime'].dt.month
data['transactionDayofWeek'] = data['transactionDateTime'].dt.dayofweek
data['transactionHour'] = data['transactionDateTime'].dt.hour

# ---------------------------------------
# Feature Engineering - trnasactionDateTime and transactionAmount Distribution Distances
# ---------------------------------------

# Create a variable to measure standardized distinace from 0 in both the time between
# the current transaction and the last transaction of the same merchant type for each customer,
# as well as the dollar amount.
data = data.sort_values(['customerId', 'transactionDateTime'])
data['time_prev'] = np.where((data['customerId'].shift(1) == data['customerId']),
                             (data['transactionDateTime']-(data['transactionDateTime'].shift(1))).astype('timedelta64[m]'),
                              np.nan)

# Function for getting standard deviation and mean when grouping by one field
def standardized_pieces(groupby_column_name, column_name):
    return data\
      .groupby([groupby_column_name], as_index = False)[column_name]\
      .aggregate({('std_' + groupby_column_name + column_name): np.std, ('avg_' + groupby_column_name + column_name): np.mean})

c_time = standardized_pieces('customerId', 'time_prev')
c_amount = standardized_pieces('customerId', 'transactionAmount')
m_amount = standardized_pieces('merchantCategoryCode', 'transactionAmount')

data2 = pd.merge(data, c_time, on = ['customerId'])
data2 = pd.merge(data2, c_amount, on = ['customerId'])
data2 = pd.merge(data2, m_amount, on = ['merchantCategoryCode'])

# Function for calculating z-scores
def single_group_zscores(groupby_column_name, column_name):
    return np.where(((data2[('std_' + groupby_column_name + column_name)].isnull()) |\
                     (data2[column_name].isnull()) |\
                     (data2[('std_' + groupby_column_name + column_name)] == 0)),
                    0,
                    ((data2[column_name] - data2[('avg_' + groupby_column_name + column_name)]) /\
                     data2[('std_' + groupby_column_name + column_name)]))

data2['z_c_tfp'] = single_group_zscores('customerId', 'time_prev')
data2['z_c_ta'] = single_group_zscores('customerId', 'transactionAmount')
data2['z_m_ta'] = single_group_zscores('merchantCategoryCode', 'transactionAmount')


# Get for customerId and merchantCategoryCode
data2 = data2.sort_values(['customerId', 'merchantCategoryCode', 'transactionDateTime'])
data2['time_prev_sim_merch'] = np.where(((data2['customerId'].shift(1) == data2['customerId']) &\
                                         (data2['merchantCategoryCode'].shift(1) == data2['merchantCategoryCode'])),
                                  (data2['transactionDateTime']-(data2['transactionDateTime'].shift(1))).astype('timedelta64[m]'),
                                   np.nan)

# Function for getting standard deviation and mean when grouping by customerId and merchantCategoryCode
def standardized_pieces_multi(column_name):
    return data2\
      .groupby(['customerId', 'merchantCategoryCode'], as_index = False)[column_name]\
      .aggregate({('std_cm' + column_name): np.std, ('avg_cm' + column_name): np.mean})

cm_time = standardized_pieces_multi('time_prev_sim_merch')
cm_amount = standardized_pieces_multi('transactionAmount')
data2 = pd.merge(data2, cm_time, on = ['customerId', 'merchantCategoryCode'])
data2 = pd.merge(data2, cm_amount, on = ['customerId', 'merchantCategoryCode'])

def multi_group_zscores(column_name):
    return np.where(((data2[('std_cm' + column_name)].isnull()) |\
                     (data2[column_name].isnull()) |\
                     (data2[('std_cm' + column_name)] == 0)),
                    0,
                    ((data2[column_name] - data2[('avg_cm' + column_name)]) /\
                     data2[('std_cm' + column_name)]))

data2['z_cm_tfp'] = multi_group_zscores('time_prev_sim_merch')
data2['z_cm_ta'] = multi_group_zscores('transactionAmount')






# ---------------------------------------
# Exploratory Data Analysis - Fraud Distribution by Merchant Category Code
# ---------------------------------------
def category_counts():
    byMC = data2[data2['isFraud'] == True].groupby('merchantCategoryCode')\
      .agg({'transactionAmount': 'count'})
    byMC.columns = ['transactionCount']
    byMC = byMC.sort_values('transactionCount', ascending = False)
    byMC['cumpercentage'] = byMC['transactionCount'].cumsum() / byMC['transactionCount'].sum()
    x_pos = np.arange(len(byMC.index))

    fig, ax = plt.subplots()
    ax.bar(x_pos, byMC['transactionCount'], color = 'C0')
    ax2 = ax.twinx()
    ax2.plot(x_pos, byMC['cumpercentage'], color = 'C3', marker = 'D', ms = 7)
    ax.tick_params(axis = 'y', colors = 'C0')
    ax2.tick_params(axis = 'y', colors = 'C3')
    ax.grid(False)
    ax2.grid(False)
    plt.xticks(x_pos, byMC.index, rotation = '90')
    plt.title('Fraud-by-Category Bar and Pareto Charts')
    ax.set_xlabel('Merchant Category')
    ax.set_ylabel('Fraud Transaction Count')
    ax2.set_ylabel('Cummulative % of Fraud Transactions', rotation = 270, labelpad = 15)

category_counts()


# ---------------------------------------
# Exploratory Data Analysis - Fraud/Not Fraud Distribution Differences
# ---------------------------------------
def numeric_boxplots():
    melted = data2.melt(id_vars = 'isFraud', value_vars = ['currentBalance',
                                                           'availableMoney',
                                                           'creditLimit',
                                                           'transactionAmount',
                                                           'z_c_ta',
                                                           'z_c_tfp',
                                                           'z_m_ta',
                                                           'z_cm_ta',
                                                           'z_cm_tfp'])
    g = sns.FacetGrid(melted, col = 'variable', col_wrap = 3, sharey = False)
    g.map(sns.boxplot, 'isFraud', 'value', showfliers = False)
    g.fig.subplots_adjust(top = 0.9)
    g.fig.suptitle('Numeric Variable Box Plots by Fraud ID')

numeric_boxplots()


# ---------------------------------------
# Initial Feature Selection
# ---------------------------------------

initSet = data2[['isFraud',
                 'creditLimit',
                 'availableMoney',
                 'transactionAmount',
                 'currentBalance',
                 'transactionMonth',
                 'transactionDayofWeek',
                 'transactionHour',
                 'z_c_ta',
                 'z_c_tfp',
                 'z_m_ta',
                 'z_cm_ta',
                 'z_cm_tfp',
                 'acqCountry',
                 'merchantCountryCode',
                 'merchantCategoryCode',
                 'posEntryMode',
                 'posConditionCode',
                 'transactionType',
                 'cardPresent',
                 'expirationDateKeyInMatch',
                 'rev_association',
                 'multi_swipe']]

initSet['label'] = np.where(initSet['isFraud'] == True, 1, 0)
initSet = initSet.drop('isFraud', axis = 1)


del [data, data2]
# ---------------------------------------
# Pre-Processing
# ---------------------------------------

# Check out the data pulled into initSet.
initSet.shape

# Check for null values
null_count = initSet.isnull().sum()

# Check for empty string values
empty_string_count = (initSet.values == '').sum(axis = 0)

# Check for med_provname_choice values
unique_values = initSet.nunique()

# Put the three above items together
explore_initSet = pd.DataFrame({'null_count': null_count,
          'empty_string_count': empty_string_count,
          'unique_values': unique_values})\
.reset_index()\
.sort_values(by = ['empty_string_count', 'unique_values'], ascending = False)

frequency_ratios = pd.DataFrame([])
def keep_top2(column_name):
    try:
        f2 = initSet.groupby(column_name).size().reset_index()
        f2.columns = ['value', 'count']
        f3 = f2.nlargest(2, 'count')
        f4 = pd.DataFrame({'index': [column_name], 'freqRatio': f3['count'].iloc[0]/f3['count'].iloc[1]})
        return f4
    except:
        f4 = pd.DataFrame({'index': [column_name], 'freqRatio': np.nan})
        return f4

for i in initSet.columns:
    f5 = keep_top2(i)
    frequency_ratios = frequency_ratios.append(f5)

# Merge the freqRatio with explore_df and print
pd.merge(explore_initSet, frequency_ratios, on = 'index')


# Convert all non-float64 dtypes to 'str'.
initSet[['transactionMonth',
         'transactionDayofWeek',
         'transactionHour',
         'cardPresent',
         'expirationDateKeyInMatch']] = initSet[['transactionMonth',
                                                 'transactionDayofWeek',
                                                 'transactionHour',
                                                 'cardPresent',
                                                 'expirationDateKeyInMatch']].astype(str)

# Replace all blank strings with np.nan.
initSet = initSet.replace(r'^\s*$', np.nan, regex = True)


# Split the data into train and test sets
# --------------------------------------------------
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(initSet.drop('label', axis = 1),
                                                    initSet['label'],
                                                    test_size=0.30,
                                                    random_state=42)

# ## Missing Value Imputation
# Replace the no.nan values of X_train and X_test with X_train's mode.
null_count = initSet.isnull().sum()

for i in null_count[null_count > 0].index:
    X_train[i].fillna(X_train[i].mode()[0], inplace = True)
    X_test[i].fillna(X_train[i].mode()[0], inplace = True)

# Validate that there are no more nulls
X_train.isnull().sum().sum()
X_test.isnull().sum().sum()

# ## One-hot encoding
# Categorical dummy variable development
train_X = pd.get_dummies(X_train)
test_X = pd.get_dummies(X_test)

# Check to see if there are dummy variables in the train set that aren't in the test set,
# and synthetically create the variables in the test set.
missing_cols = set(train_X.columns) - set(test_X.columns)

if len(missing_cols) == 0:
    print('Train set does not contain any fields not included in test set.')
else:
    for i in missing_cols:
        test_X[i] = 0
        print(test_X[i].name + ' variable synthetically created in test set.')

# Ensure that the test set does not have any dummy variables not included in the train set
# and that the variables are in the same order.
test_X = test_X[train_X.columns]

# ## Downsample training set for model development
from sklearn.utils import resample

train_Xy = pd.concat([train_X, y_train], axis = 1)
train_Xy['label'].value_counts()

train0 = train_Xy[train_Xy['label'] == 0]
train1 = train_Xy[train_Xy['label'] == 1]

# DOwnsample the majority class
train0_downsampled = resample(train0,
                              replace = False,
                              n_samples = len(train1),
                              random_state = 123)

# Combine minority class with downsampled majority class
train = pd.concat([train0_downsampled, train1])

# Display new class counts
train['label'].value_counts()

train_X = train.drop('label', axis = 1)
train_y = train['label']


# ---------------------------------------
# ## Random Forest - Base Model Development
# ---------------------------------------

from sklearn.ensemble import RandomForestClassifier

base_rf = RandomForestClassifier(n_estimators = 200,
                                 bootstrap = True,
                                 max_features = 'sqrt')

base_rf.fit(train_X, train_y)

# Predict on the test set
rf_base_predictions = base_rf.predict(test_X)

# Probabilities for each class
rf_base_probs = base_rf.predict_proba(test_X)[:, 1]

# View the most important features
rf_fi = pd.DataFrame({'feature': list(train_X.columns),
                      'importance': base_rf.feature_importances_})\
                    .sort_values('importance', ascending = False)
rf_fi.head(10)


# ---------------------------------------
# Random Forest Model Hyperparameter Tuning
# ---------------------------------------

# First try a random grid search to get an idea where to start
from sklearn.model_selection import RandomizedSearchCV

# Set the hyperparameters to randomly try
n_estimators = [200, 1000]
max_features = ['sqrt']
max_depth = [20, 40, 60]
min_samples_split = [2, 5, 10]
min_samples_leaf = [1, 2, 3]
bootstrap = [True]

random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap
               }

rf_rsearch = RandomForestClassifier()
rf_random = RandomizedSearchCV(estimator = rf_rsearch,
                               param_distributions = random_grid,
                               n_iter = 10,
                               cv = 4,
                               scoring = 'roc_auc',
                               verbose = 2,
                               random_state = 42,
                               n_jobs = -1)

rf_random.fit(train_X, train_y)

# View the best fit params
rf_random.best_params_

# Predict on the test set
rf_random_predictions = rf_random.predict(test_X)

# Probabilities for each class
rf_random_probs = rf_random.predict_proba(test_X)[:, 1]


# Create ROC Graph
from sklearn.metrics import precision_score, recall_score, roc_auc_score, roc_curve

default = {}
default['recall'] = recall_score(y_test, [1 for _ in range(len(y_test))])
default['precision'] = precision_score(y_test, [1 for _ in range(len(y_test))])
default['roc'] = 0.5

baseline = {}
baseline['recall'] = recall_score(y_test, rf_base_predictions)
baseline['precision'] = precision_score(y_test, rf_base_predictions)
baseline['roc'] = roc_auc_score(y_test, rf_base_probs)

random = {}
random['recall'] = recall_score(y_test, rf_random_predictions)
random['precision'] = precision_score(y_test, rf_random_predictions)
random['roc'] = roc_auc_score(y_test, rf_random_probs)

default_fpr, default_tpr, _ = roc_curve(y_test, [1 for _ in range(len(y_test))])
base_fpr, base_tpr, _ = roc_curve(y_test, rf_base_probs)
random_fpr, random_tpr, _ = roc_curve(y_test, rf_random_probs)

def baseline_roc_chart():
    plt.figure(figsize = (6, 4))
    plt.rcParams['font.size'] = 10
    plt.plot(default_fpr, default_tpr, color = 'r')
    plt.plot(base_fpr, base_tpr, color = 'b', label = 'baseline RF')
    plt.plot(random_fpr, random_tpr, color = 'm', label = 'randomSearch RF')
    plt.legend(loc = 'lower right')
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('ROC Curves')

baseline_roc_chart()
print('Baseline Random Forest AUC: ' + str(roc_auc_score(y_test, rf_base_probs)))
print('Random Grid Search Random Forest AUC: ' + str(roc_auc_score(y_test, rf_random_probs)))



# ---------------------------------------
# ## XGBoost Model Development
# ---------------------------------------
import xgboost as xgb
from scipy.sparse import csr_matrix

sparse_train_X = csr_matrix(train_X)
xgb_train = xgb.DMatrix(sparse_train_X, label = train_y)
