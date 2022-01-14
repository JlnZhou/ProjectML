import pandas as pd

def fct_Modele_Naif(mesures_train, liste_dates, liste_stations):
    ## Fonction de prédiction pour le Modèle Naif 
    # mesures: dataframe X
    # dates: dataframe avec les colonnes Jour, Mois, Annee pour les dates que l'on veut prévoir

    # On extrait les valeurs recherchées
    # Tout d'abord on calcule la date qu'il faut chercher
    dates_predictions_X = pd.DataFrame({"Date": liste_dates + pd.DateOffset(days=-8)})
    resultat = pd.DataFrame({"Date": liste_dates})
    resultat["Date_X"] = dates_predictions_X["Date"]
    predictions = []
    for index, row in resultat.iterrows():
        predictions.append(list(mesures_train[mesures_train["Date"] == row["Date_X"]][liste_stations].iloc[0,:]))
    resultat[liste_stations] = predictions
    resultat = resultat[liste_stations + ["Date"]]
    return resultat