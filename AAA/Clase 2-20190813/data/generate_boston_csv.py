from os.path import dirname, join
import pandas as pd
from sklearn.datasets import load_boston

# Este script toma el UCI ML housing dataset tomado de sklearn y lo guarda en un csv

boston = load_boston()

boston_data = load_boston()
df_boston = pd.DataFrame(boston_data.data,columns=boston_data.feature_names)
df_boston['target'] = pd.Series(boston_data.target)
df_boston.head()

df_boston.to_csv(join(dirname(__file__), 'boston_data.csv'), sep=';', index=False)

with open(join(dirname(__file__), 'boston_metadata.md'), 'w') as f:
    f.write(boston.DESCR)
