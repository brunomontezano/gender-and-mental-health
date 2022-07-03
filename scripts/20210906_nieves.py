### BASIC EXAMPLE OF CHI-SQUARED TEST IN PYTHON ###
### USING NUMPY AND PANDAS FOR DATA MANIPULATION ###

# Imports
import pandas as pd
import numpy as np
from scipy.stats import chi2_contingency

# Read dataset
df = pd.read_csv('../data/coorte-t1-t2-24-08-17.csv')

# Print dataset
print(df)

# Create filtered dataset
df_t2 = df.loc[(df["Bipolar_conferido"] == "1") | (df["Bipolar_conferido"] == "0")]

# Check some counts
df_t2['Bipolar_conferido'].value_counts()

df_t2['depressao'].value_counts()

df_t2['a03sexo_t2'].value_counts()

# Create cross-table to perform chi-squared test on
chisqt = pd.crosstab(df_t2.a03sexo_t2, df_t2.depressao, margins=True)

# Check the table
print(chisqt)

# Create crude array
value = np.array([chisqt.iloc[0][0:5].values,
                  chisqt.iloc[1][0:5].values])
                  
# Perform the chi-squared test
print(chi2_contingency(value)[0:3])
