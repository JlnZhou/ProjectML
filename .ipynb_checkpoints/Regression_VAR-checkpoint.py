import numpy as np
import pandas as pd

from sklearn.preprocessing import SplineTransformer
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import GammaRegressor

from statsmodels.tsa.api import VAR

def fct_Regression_VAR_predict(mesures, AR_fit, lag, liste_dates, liste_stations):
    resultats = pd.DataFrame()
    for curr_date in liste_dates:
        date_present = curr_date + pd.DateOffset(days = -8)
        date_debut = date_present + pd.DateOffset(days = -lag+1)
        obs = mesures[(mesures["Date"] >= date_debut) & (mesures["Date"] <= date_present)][liste_stations]
        pred = AR_fit.forecast(np.array(obs), 7)
        curr_resultat = pd.DataFrame({"Date": [curr_date]})
        curr_resultat[liste_stations] = pred[6,:]
        resultats = pd.concat([resultats, curr_resultat])
    return resultats
