def fct_scores_mse(Y_true, Y_pred, liste_stations):
    
    Y_true = Y_true.sort_values(by = ["Date"])
    Y_pred = Y_pred.sort_values(by = ["Date"])
    
    liste_mse = []
    for code in liste_stations:
        liste_mse.append(numpy.mean(np.square(Y_true[code] - Y_pred[code])))
    resultat = pd.DataFrame({"Code station": liste_stations, "MSE": liste_mse})
    return resultat