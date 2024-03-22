from abc import ABC, abstractmethod
from typing import List, Optional
import numpy as np


def add_bias(X: np.ndarray, bias: float) -> np.ndarray:
    biases = np.ones((X.shape[0], 1)) * bias
    return np.concatenate((biases, X), axis=1)


def accuracy(predicted: np.ndarray, gold: np.ndarray) -> float:
    return np.mean(predicted == gold)


def cross_entropy_loss(X: np.ndarray, T: np.ndarray) -> np.float64:
    return -np.mean(T * np.log(X) + (1 - T) * np.log(1 - X))


def mean_squared_error_loss(X: np.ndarray, T: np.ndarray) -> np.float64:
    return np.mean((X - T) ** 2)



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
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray], #TODO -> X_VAL, T_VAL
        T_VAL: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.01,
        n_epochs_no_update: int=5,
    ) -> None: ...


class LinearRegressionClassifier(Classifier):
    def fit(
        self, 
        X_TRAIN: np.ndarray, 
        T_TRAIN: np.ndarray, 
        X_VAL: Optional[np.ndarray], 
        T_VAL: Optional[np.ndarray], 
        learning_rate: float=0.1, 
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        if self.bias:
            X_TRAIN = add_bias(X_TRAIN, self.bias)
            if X_VAL is not None:
                X_VAL = add_bias(X_VAL, self.bias)

        n_datapoints, n_features = X_TRAIN.shape

        self.weights = np.zeros(n_features)

        for _ in range(epochs):
            train_predictions = X_TRAIN @ self.weights
            self.train_losses = np.append(self.train_losses, mean_squared_error_loss(train_predictions, T_TRAIN))

            # Checking condition every iteration is not optimal, but alterantive is major code duplication
            if X_VAL is not None and T_VAL is not None:
                val_predictions = X_VAL @ self.weights
                self.val_losses = np.append(self.val_losses, mean_squared_error_loss(val_predictions, T_VAL))

            self.weights -= learning_rate / n_datapoints * X_TRAIN.T @ (train_predictions - T_TRAIN)

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
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        if self.bias:
            X_TRAIN = add_bias(X_TRAIN, self.bias)
            if X_VAL is not None:
                X_VAL = add_bias(X_VAL, self.bias)

        n_datapoints, n_features = X_TRAIN.shape
        
        self.weights = np.zeros(n_features)
        
        for _ in range(epochs):
            train_predictions = self._forward(X_TRAIN)
            self.train_losses = np.append(self.train_losses, cross_entropy_loss(train_predictions, T_TRAIN))

            if X_VAL is not None and T_VAL is not None:
                val_predictions = self._forward(X_VAL)
                self.val_losses = np.append(self.val_losses, cross_entropy_loss(val_predictions, T_VAL))

            self.weights -= learning_rate / n_datapoints * X_TRAIN.T @ (train_predictions - T_TRAIN)      

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
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float=0.1,
        epochs: int=10,
        tol: float=0.001,
        n_epochs_no_update: int=5,
    ) -> None:
        self.classifiers: List[LogisticRegressionClassifier] = []
        unique_classes = np.unique(T_TRAIN)

        for class_index in unique_classes:
            T_BINARY = (T_TRAIN == class_index).astype("int")
            TV_BINARY = (T_VAL == class_index).astype("int") if T_VAL is not None else None

            classifier = LogisticRegressionClassifier(self.bias)
            classifier.fit(X_TRAIN, T_BINARY, X_VAL, TV_BINARY, learning_rate, epochs, tol, n_epochs_no_update)

            self.classifiers.append(classifier)

        # calculate losses as the sums of all the classifiers' losses divided by the number of classifiers
        self.train_losses = self.classifiers[0].train_losses
        self.val_losses = self.classifiers[0].val_losses

        for i in range(2, len(self.classifiers)):
            if len(self.classifiers[i].train_losses) > len(self.train_losses):
                self.train_losses.resize(self.classifiers[i].train_losses.shape)
            elif len(self.classifiers[i].train_losses) < len(self.train_losses):
                self.classifiers[i].train_losses.resize(self.train_losses.shape)

            if len(self.classifiers[i].val_losses) > len(self.val_losses):
                self.val_losses.resize(self.classifiers[i].val_losses.shape)
            elif len(self.classifiers[i].val_losses) < len(self.val_losses):
                self.classifiers[i].val_losses.resize(self.val_losses.shape)

            self.train_losses += self.classifiers[i].train_losses
            self.val_losses += self.classifiers[i].val_losses

        self.train_losses = np.divide(self.train_losses, len(unique_classes))
        self.val_losses = np.divide(self.val_losses, len(unique_classes))

        # calculate this classifier's trained epochs as the sum of all the binary classifiers trained epochs
        self.trained_epochs = np.sum(np.array([classifier.trained_epochs for classifier in self.classifiers]))

    def predict(self, X: np.ndarray, threshold: float=0.5) -> np.ndarray:
        probabilities = np.array([classifier.predict_probability(X) for classifier in self.classifiers])

        return np.argmax(probabilities, axis=0)


class MultiLayerLinearRegressionClassifier(Classifier):
    ...
