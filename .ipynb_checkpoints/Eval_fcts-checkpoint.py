import numpy as np
import pandas as pd

def fct_RMSE(Y_true, Y_pred, liste_stations):
    Y_true = Y_true.sort_values(by = ["Date"])
    Y_pred = Y_pred.sort_values(by = ["Date"])
    
    liste_rmse = []
    for code in liste_stations:
        liste_rmse.append(np.sqrt(np.mean(np.square(Y_true[code].values - Y_pred[code].values))))
    resultat = pd.DataFrame({"Code station": liste_stations, "RMSE": liste_rmse})
    return resultat
    
def fct_MAE(Y_true, Y_pred, liste_stations):
    Y_true = Y_true.sort_values(by = ["Date"])
    Y_pred = Y_pred.sort_values(by = ["Date"])
    
    liste_mae = []
    for code in liste_stations:
        liste_mae.append(np.mean(np.abs(Y_true[code].values - Y_pred[code].values)))
    resultat = pd.DataFrame({"Code station": liste_stations, "MAE": liste_mae})
    return resultat

def fct_R2(Y_true, Y_pred, liste_stations):
    Y_true = Y_true.sort_values(by = ["Date"])
    Y_pred = Y_pred.sort_values(by = ["Date"])
    
    liste_r2 = []
    for code in liste_stations:
        moyenne_true = np.mean(Y_pred[code].values)
        liste_r2.append(1- np.sum(np.square(Y_pred[code].values - Y_true[code].values))/np.sum(np.square(Y_true[code].values - moyenne_true)))
    resultat = pd.DataFrame({"Code station": liste_stations, "R2": liste_r2})
    return resultat