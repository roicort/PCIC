import numpy as np

class GaussianNaiveBayes:
    """
    Gaussian Naive Bayes Classifier
    """

    def __init__(self):
        self.prior = None # Probabilidad a priori de cada clase
        self.mean = None # Media de cada clase
        self.var = None # Varianza de cada clase

    def MLE(self, X, y):
        """
        Maximum Likelihood Estimation
        """
        n, d = X.shape # Obtiene el número de instancias y características
        classes = np.unique(y) # Obtiene las clases únicas
        self.prior = np.bincount(y) / n # Calcula la probabilidad a priori de cada clase
        self.mean = np.array([X[y == i].mean(axis=0) for i in classes]) # Calcula la media de cada clase
        self.var = np.array([X[y == i].var(axis=0) for i in classes]) # Calcula la varianza de cada clase

    def MAP(self, X, y):
        """
        Maximum A Posteriori Estimation
        """
        n, d = X.shape # Obtiene el número de instancias y características
        classes = np.unique(y) # Obtiene las clases únicas
        self.prior = np.bincount(y) / n # Calcula la probabilidad a priori de cada clase 
        self.mean = np.array([X[y == i].mean(axis=0) for i in classes]) # Calcula la media de cada clase
        self.var = np.array([X[y == i].var(axis=0) + 1e-3 for i in classes]) # Calcula la varianza de cada clase

    def fit(self, X, y, method='MLE'):
        """
        Fit the model
        """
        if method == 'MLE':
            self.MLE(X, y)
        elif method == 'MAP':
            self.MAP(X, y)
        else:
            raise ValueError('Invalid method')
        
        # Laplace smoothing for prior probabilities
        self.prior = (self.prior + 1) / (np.sum(self.prior) + len(self.prior))

    def predict(self, X):
        """
        Predict the class labels for the input data
        """
        X = np.array(X)  # Nos aseguramos de que X sea un arreglo de numpy
        n, d = X.shape # Obtiene el número de instancias y características
        y_pred = np.zeros(n) # Inicializa el arreglo de predicciones
        for i in range(n): # Itera sobre las instancias
            #print(f"Instance {i}: {X[i]}")
            #print(f"Prior probabilities: {self.prior}")
            #print(f"Mean of each class: {self.mean}")
            #print(f"Variance of each class: {self.var}")
            
            # Calcula la probabilidad de cada clase utilizando logaritmos
            log_likelihood = -0.5 * np.sum(np.log(2 * np.pi * self.var) + ((X[i] - self.mean) ** 2 / self.var), axis=1)
            #print(f"Log-likelihood for instance {i}: {log_likelihood}")
            
            log_prob = np.log(self.prior) + log_likelihood  # Suma el logaritmo de la probabilidad a priori y la log-verosimilitud
            #print(f"Log-posterior probabilities for instance {i}: {log_prob}")
            
            y_pred[i] = np.argmax(log_prob) # Asigna la clase con mayor probabilidad
        return y_pred # Regresa las predicciones