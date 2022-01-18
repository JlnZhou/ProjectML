import numpy as np
import pandas as pd

from sklearn.preprocessing import SplineTransformer
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import GammaRegressor

def fct_Regression_SplineGamma_fit(mesures_train, liste_stations, n_knots):
    ## Fonction pour déterminer les splines cycliques, GAM gamma
    # mesures: dataframe X d'entrainement
    # n_knots: le nombre de noeuds
    
    mesures_stations = mesures_train[["Date"] + liste_stations]
    # On prend en abscisse le delta de temps par rapport au 01/01 de l'année en cours
    mesures_tofit = pd.DataFrame()
    liste_annee = np.unique(mesures_stations["Date"].apply(lambda x: x.year))
    for curr_annee in liste_annee:
        mesures_annee = mesures_stations[mesures_stations["Date"].apply(lambda x: x.year == curr_annee)]
        n_mesures = len(mesures_annee)
        mesures_annee.index = range(n_mesures)
        jour_an = pd.to_datetime(str(curr_annee)+"/01/01")
        jours_delta = (mesures_annee["Date"] - jour_an).apply(lambda x: x.delta)
        jours_delta.index = range(n_mesures)
        mesures_annee = pd.concat([mesures_annee, pd.DataFrame({"Delta": jours_delta})], axis = 1)
        mesures_tofit = pd.concat([mesures_tofit, mesures_annee])
    
    resultat = pd.DataFrame({"Delta": np.unique(mesures_tofit["Delta"])})
    # Calcul de la spline
    for code in liste_stations:
        X = mesures_tofit[["Delta", code]]
        spline = SplineTransformer(n_knots=n_knots,
                                   extrapolation="periodic")
        model = make_pipeline(spline, GammaRegressor(alpha = 0))
        model.fit(X[["Delta"]], X[code])
        composante = model.predict(resultat[["Delta"]])
        composante = pd.DataFrame(composante, columns = [code])
        resultat = pd.concat([resultat, composante], axis = 1)
    resultat["Delta"] = pd.to_timedelta(resultat["Delta"]).apply(lambda x: x.days)
    return resultat

def fct_Regression_SplineGamma_predict(fit, liste_dates, liste_stations):

    max_jours = np.max(fit["Delta"])
    
    # On retourne la bonne valeur
    resultat = pd.DataFrame({"Date": liste_dates})
    liste_annees = np.unique(liste_dates.apply(lambda x: x.year))
    predictions = []
    for curr_annee in liste_annees:
        dates_annee = pd.DataFrame({"Date": liste_dates[liste_dates.apply(lambda x: x.year == curr_annee)]})
        jour_an = pd.to_datetime(str(curr_annee)+"/01/01")
        dates_annee["Delta_Jour"] = (dates_annee - jour_an)
        dates_annee["Delta_Jour"] = dates_annee["Delta_Jour"].apply(lambda x: x.days)
        for index, row in dates_annee.iterrows():
            predictions.append(fit[fit["Delta"] == row["Delta_Jour"]%(max_jours+1)][liste_stations].iloc[0,:])
    resultat[liste_stations] = predictions
    return resultat


def fct_Regression_SplineGamma_residus(fit, mesures, liste_stations):
    
    liste_dates = mesures["Date"]
    predictions = fct_Regression_SplineGamma_predict(fit, liste_dates, liste_stations)
    
    resultat = pd.DataFrame({"Date": liste_dates})
    for code in liste_stations:
        residus = mesures[code] - predictions[code]
        resultat[code] = residus
    return resultat