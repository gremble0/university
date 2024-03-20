from abc import ABC, abstractmethod
from typing import List, Optional
import numpy as np


# TODO: make these methods?
def add_bias(X: np.ndarray, bias: float) -> np.ndarray:
    biases = np.ones((X.shape[0], 1)) * bias
    return np.concatenate((biases, X), axis=1)


def accuracy(predicted: np.ndarray, gold: np.ndarray) -> float:
    return np.mean(predicted == gold)


class Classifier(ABC):
    """Abstract base class for classifiers"""
    def __init__(self, bias: float=-1) -> None:
        self.bias = bias
        self.train_losses = np.array([], dtype=float)
        self.val_losses = np.array([], dtype=float)
        self.trained_epochs = 0

    @abstractmethod
    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray: ...

    @abstractmethod
    def fit(
        self,
        X: np.ndarray,
        T: np.ndarray,
        XV: Optional[np.ndarray], #TODO -> X_VAL, T_VAL
        TV: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.01,
        n_epochs_no_update: int=5,
    ) -> None: ...

    def _cross_entropy_loss(self, X: np.ndarray, T: np.ndarray) -> np.float64:
        return -np.mean(T * np.log(X) + (1 - T) * np.log(1 - X))

    def _mean_squared_error(self, X: np.ndarray, T: np.ndarray) -> np.float64:
        return np.mean((X - T) ** 2)


class LinearRegressionClassifier(Classifier):
    def fit(
        self, 
        X: np.ndarray, 
        T: np.ndarray, 
        XV: Optional[np.ndarray], 
        TV: Optional[np.ndarray], 
        learning_rate: float=0.1, 
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        if self.bias:
            X = add_bias(X, self.bias)
            if XV is not None:
                XV = add_bias(XV, self.bias)

        n_datapoints, n_features = X.shape

        self.weights = np.zeros(n_features)

        for _ in range(epochs):
            train_predictions = X @ self.weights
            self.train_losses = np.append(self.train_losses, self._mean_squared_error(train_predictions, T))

            # Checking condition every iteration is not optimal, but alterantive is major code duplication
            if XV is not None and TV is not None:
                val_predictions = XV @ self.weights
                self.val_losses = np.append(self.val_losses, self._mean_squared_error(val_predictions, TV))

            self.weights -= learning_rate / n_datapoints * X.T @ (train_predictions - T)

            # Should we return early?
            if self.trained_epochs < n_epochs_no_update:
                self.trained_epochs += 1
            elif self.train_losses[self.train_losses.size - 1] - \
                 self.train_losses[self.train_losses.size - n_epochs_no_update - 1] + tol > 0:
                return
            else:
                self.trained_epochs += 1

    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return (X @ self.weights) > threshold


class LogisticRegressionClassifier(Classifier):
    def fit(
        self,
        X: np.ndarray,
        T: np.ndarray,
        XV: Optional[np.ndarray],
        TV: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        if self.bias:
            X = add_bias(X, self.bias)
            if XV is not None:
                XV = add_bias(XV, self.bias)

        n_datapoints, n_features = X.shape
        
        self.weights = np.zeros(n_features)
        
        for _ in range(epochs):
            train_predictions = self._forward(X)
            self.train_losses = np.append(self.train_losses, self._cross_entropy_loss(train_predictions, T))

            if XV is not None and TV is not None:
                val_predictions = self._forward(XV)
                self.val_losses = np.append(self.val_losses, self._cross_entropy_loss(val_predictions, TV))

            self.weights -= learning_rate / n_datapoints * X.T @ (train_predictions - T)      

            if self.trained_epochs < n_epochs_no_update:
                self.trained_epochs += 1
            elif self.train_losses[self.train_losses.size - 1] - \
                 self.train_losses[self.train_losses.size - n_epochs_no_update - 1] + tol > 0:
                return
            else:
                self.trained_epochs += 1

    def predict_probability(self, X: np.ndarray) -> np.ndarray:
        if self.bias:
            X = add_bias(X, self.bias)

        return self._forward(X)
    
    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        return (self.predict_probability(X) > threshold).astype("int")

    def _forward(self, X: np.ndarray) -> np.ndarray:
        return 1 / (1 + np.exp(-X @ self.weights))


class MultiLogisticRegressionClassifier(LogisticRegressionClassifier):
    def fit(
        self,
        X: np.ndarray,
        T: np.ndarray,
        XV: Optional[np.ndarray],
        TV: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        self.classifiers: List[LogisticRegressionClassifier] = []
        # class_ because class is a keyword
        for class_ in np.unique(T):
            T_BINARY = (T == class_).astype("int")
            TV_BINARY = (TV == class_).astype("int") if TV is not None else None

            classifier = LogisticRegressionClassifier()
            classifier.fit(X, T_BINARY, XV, TV_BINARY, learning_rate, epochs, tol, n_epochs_no_update)

            self.classifiers.append(classifier)

    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        probabilities = [probability for probability in
                         map(lambda classifier: classifier.predict_probability(X), self.classifiers)]
        print(probabilities)
        # return (self.predict_probability(X) > threshold).astype("int")
