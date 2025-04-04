import numpy as np

class BinaryLogisticRegression:
    """
    Logistic Regression Classifier
    """

    def __init__(self, lr=0.01, epochs=1000):
        """
        Inicializa el clasificador de Regresión Logística
        """
        self.weights = None
        self.bias = None
        self.lr = lr
        self.epochs = epochs

    def sigmoid(self, z):
        """
        Función sigmoide
        """
        return 1 / (1 + np.exp(-z))

    def MLE(self, X, y):
        """
        Maximum Likelihood Estimation

        Ajusta los parámetros usando gradiente descendente.
        """

        for _ in range(self.epochs):
            linear_model = np.dot(X, self.weights) + self.bias
            predictions = self.sigmoid(linear_model)

            # Gradientes
            dw = (1 / X.shape[0]) * np.dot(X.T, (predictions - y))
            db = (1 / X.shape[0]) * np.sum(predictions - y)

            # Actualización de parámetros
            self.weights -= self.lr * dw
            self.bias -= self.lr * db

    def MAP(self, X, y, lr=0.01, epochs=1000, alpha=1.0):
        """
        Maximum A Posteriori Estimation

        Ajusta los parámetros usando gradiente descendente con regularización L2.
        """

        for _ in range(epochs):
            linear_model = np.dot(X, self.weights) + self.bias
            predictions = self.sigmoid(linear_model)

            # Gradientes con regularización L2
            dw = (1 / X.shape[0]) * np.dot(X.T, (predictions - y)) + (alpha / X.shape[0]) * self.weights
            db = (1 / X.shape[0]) * np.sum(predictions - y)

            # Actualización de parámetros
            self.weights -= lr * dw
            self.bias -= lr * db

    def fit(self, X, y, method='MLE', laplace=False, lr=0.01, epochs=1000, alpha=1.0):
        """
        Ajusta el modelo a los datos
        """
        self.instances = X.shape[0]  # Número de instancias
        self.features = X.shape[1]  # Número de características 
        self.classes = np.unique(y)  # Obtiene las clases únicas

        self.weights = np.zeros(X.shape[1])
        self.bias = 0

        if method == 'MLE':
            self.MLE(X, y)
        elif method == 'MAP':
            self.MAP(X, y)
        else:
            raise ValueError('Invalid method')

    def predict(self, X):
        """
        Predice las etiquetas de clase para los datos de entrada
        """
        linear_model = np.dot(X, self.weights) + self.bias
        probabilities = self.sigmoid(linear_model)
        return (probabilities >= 0.5).astype(int)