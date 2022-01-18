import numpy as np
import pandas as pd

def fct_Standardize(mesures, means, stds, colonnes):
    resultats = {}
    for col in colonnes:
        resultats[col] = list((mesures[col]-means[col])/stds[col])
    return pd.DataFrame(resultats)

def fct_StandardizeInverse(mesures, means, stds, colonnes):
    resultats = {}
    for col in colonnes:
        resultats[col] = list(mesures[col]*stds[col]+means[col])
    return pd.DataFrame(resultats)