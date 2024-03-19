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
    losses: np.ndarray

    def __init__(self, bias: float=-1) -> None:
        self.bias = bias
        self.losses = np.array([], dtype=float)

    @abstractmethod
    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray: ...

    @abstractmethod
    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs: int=10) -> None: ...

    def _cross_entropy_loss(self, X: np.ndarray, T: np.ndarray) -> np.float64:
        return -np.mean(T * np.log(X) + (1 - T) * np.log(1 - X))

    def _mean_squared_error(self, X: np.ndarray, T: np.ndarray) -> np.float64:
        # TODO: check if right
        return np.mean((X - T) ** 2)


class LinearRegressionClassifier(Classifier):
    def fit(self, X: np.ndarray, T: np.ndarray, learning_rate: float=0.1, epochs: int=10) -> None:
        if self.bias:
            X = add_bias(X, self.bias)

        n_datapoints, n_features = X.shape

        self.weights = np.zeros(n_features)

        for _ in range(epochs):
            predictions = X @ self.weights
            self.weights -= learning_rate / n_datapoints * X.T @ (predictions - T)

            self.losses = np.append(self.losses, self._mean_squared_error(predictions, T))

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
            predictions = self._forward(X)
            self.losses = np.append(self.losses, self._cross_entropy_loss(predictions, T))

            self.weights -= learning_rate / n_datapoints * X.T @ (predictions - T)      
    
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
