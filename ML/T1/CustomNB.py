import numpy as np

class GaussianNaiveBayes:
    """
    Gaussian Naive Bayes Classifier
    """

    def __init__(self, convariance=False):
        """
        Inicializa el clasificador Naive Bayes Gaussiano
        convariance: bool
            Si es True, se calcula la matriz de covarianza para cada clase.
            Si es False, se calcula la varianza para cada clase.
        """
        self.priors = None # Probabilidad a priori de cada clase
        self.means = None # Media de cada clase
        self.vars = None # Varianza de cada clase
        self.classes = None # Clases únicas
        self.instances = None # Número de instancias
        self.features = None # Número de características
        self.covariances = False
        if convariance:
            self.covariances = True
            

    def MLE(self, X, y):
        """
        Maximum Likelihood Estimation

        Busca los parámetros que maximizan la probabilidad de los datos observados, 
        sin considerar ninguna información previa.
        """
        if self.covariances:
            self.prior = np.bincount(y) / self.instances # Calcula la probabilidad a priori de cada clase
            self.mean = np.array([X[y == i].mean(axis=0) for i in self.classes]) # Calcula la media de cada clase
            self.covariances = np.array([np.cov(X[y == i], rowvar=False) for i in self.classes]) # Calcula la matriz de covarianza de cada clase

        else:
            self.prior = np.bincount(y) / self.instances
            self.mean = np.array([X[y == i].mean(axis=0) for i in self.classes])
            self.var = np.array([X[y == i].var(axis=0) for i in self.classes]) # Calcula la varianza de cada clase

    def MAP(self, X, y):
        """
        Maximum A Posteriori Estimation

        Busca los parámetros que maximizan la probabilidad 
        posterior, combinando la probabilidad de los datos observados con una distribución previa sobre los parámetros.
        """
        self.prior = np.bincount(y) / self.instances # Calcula la probabilidad a priori de cada clase 
        self.mean = np.array([X[y == i].mean(axis=0) for i in self.classes]) # Calcula la media de cada clase
        self.var = np.array([X[y == i].var(axis=0) + 1e-3 for i in self.classes]) # Calcula la varianza de cada clase

    def fit(self, X, y, method='MLE', laplace=False):
        """
        Fit the model
        """

        self.instances = X.shape[0] # Número de instancias
        self.features = X.shape[1] # Número de características
        self.classes = np.unique(y) # Obtiene las clases únicas

        cov = self.covariances

        if method == 'MLE':
            self.MLE(X, y)
        elif method == 'MAP':
            self.MAP(X, y)
        else:
            raise ValueError('Invalid method')
        
        if laplace:
            self.prior = (self.prior + 1) / (np.sum(self.prior) + len(self.prior))
            self.mean = (self.mean + 1) / (np.sum(self.mean) + len(self.mean))
            self.var = (self.var + 1) / (np.sum(self.var) + len(self.var))
            if cov:
                self.covariances = (self.covariances + 1) / (np.sum(self.covariances) + len(self.covariances))

    def predict(self, X):
        """
        Predict the class labels for the input data
        """
        self.instances = X.shape[0] # Número de instancias para predecir

        posteriori = np.zeros(self.instances) # Inicializa el arreglo de predicciones

        for i in range(self.instances): # Itera sobre las instancias

            # Calcula la probabilidad de cada clase utilizando logaritmos
            log_likelihood = np.zeros(len(self.classes)) # Inicializa las log-verosimilitudes

            if self.covariances is not False:
                for c in range(len(self.classes)):
                    cov_matrix = self.covariances[c]
                    inv_cov = np.linalg.inv(cov_matrix)
                    det_cov = np.linalg.det(cov_matrix)
                    diff = X[i] - self.mean[c]
                    log_likelihood[c] = -0.5 * (np.log(det_cov) + np.dot(diff, np.dot(inv_cov, diff.T)))

            else:
                log_likelihood = -0.5 * np.sum(np.log(2 * np.pi * self.var) + ((X[i] - self.mean) ** 2 / self.var), axis=1)
            
            posterior = np.log(self.prior) + log_likelihood  # Suma el logaritmo de la probabilidad a priori y la log-verosimilitud
            posteriori[i] = np.argmax(posterior) # Asigna la clase con mayor probabilidad

        return posteriori # Regresa las predicciones