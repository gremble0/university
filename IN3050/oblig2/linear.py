import numpy as np

from common import Classifier, add_bias


class LinearRegressionClassifier(Classifier):
    bias: float
    weights: np.ndarray

    def __init__(self, bias: float=-1) -> None:
        self.bias = bias

    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs=10) -> None:
        if self.bias:
            X = add_bias(X, self.bias)

        n_datapoints, n_features = X.shape

        self.weights = weights = np.zeros(n_features)

        for _ in range(epochs):
            weights -= learning_rate / n_datapoints * X.T @ (X @ weights - T)

    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return (X @ self.weights) > threshold
