from abc import ABC, abstractmethod
import numpy as np


# TODO: make these methods?
def add_bias(X: np.ndarray, bias: float) -> np.ndarray:
    biases = np.ones((X.shape[0], 1)) * bias
    return np.concatenate((biases, X), axis=1)


def accuracy(predicted: np.ndarray, gold: np.ndarray) -> float:
    return np.mean(predicted == gold)


class Classifier(ABC):
    """Abstract base class for classifiers"""
    bias: float
    weights: np.ndarray

    def __init__(self, bias: float=-1) -> None:
        self.bias = bias

    @abstractmethod
    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray: ...

    @abstractmethod
    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs: int=10) -> None: ...


class LinearRegressionClassifier(Classifier):
    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs: int=10) -> None:
        if self.bias:
            X = add_bias(X, self.bias)

        n_datapoints, n_features = X.shape

        self.weights = np.zeros(n_features)

        for _ in range(epochs):
            self.weights -= learning_rate / n_datapoints * X.T @ (X @ self.weights - T)

    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return (X @ self.weights) > threshold


class LogisticRegressionClassifier(Classifier):
    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs: int=10) -> None:
        if self.bias:
            X = add_bias(X, self.bias)

        n_datapoints, n_features = X.shape
        
        self.weights = np.zeros(n_features)
        
        for _ in range(epochs):
            self.weights -= learning_rate / n_datapoints *  X.T @ (self._forward(X) - T)      
    
    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return (self._forward(X) > threshold).astype('int')

    def predict_probability(self, X: np.ndarray) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return self._forward(X)
    
    def _forward(self, X: np.ndarray) -> np.ndarray:
        return 1 / (1 + np.exp(-X @ self.weights))
