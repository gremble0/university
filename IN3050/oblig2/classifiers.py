from abc import ABC, abstractmethod
from typing import Optional
import numpy as np


def add_bias(X: np.ndarray, bias: float = -1) -> np.ndarray:
    # If bias is falsy (0), do not add any bias
    if not bias:
        return X

    biases = np.ones((X.shape[0], 1)) * bias
    return np.concatenate((biases, X), axis=1)


def accuracy(predicted: np.ndarray, gold: np.ndarray) -> float:
    return np.mean(predicted == gold)


# Loss functions
def cross_entropy_loss(X: np.ndarray, T: np.ndarray, epsilon: float = 1e-15) -> np.float64:
    # 'epsilon' is to avoid doing log(0)
    return -np.mean(T * np.log(X + epsilon) + (1 - T) * np.log(1 - X + epsilon))


def mean_squared_error_loss(X: np.ndarray, T: np.ndarray) -> np.float64:
    return np.mean((X - T) ** 2)


# Logistic function and its derivative:
def logistic(X: np.ndarray) -> np.ndarray:
    return 1 / (1 + np.exp(-X))


def logistic_diff(Y: np.ndarray) -> np.ndarray:
    return Y * (1 - Y)


class Classifier(ABC):
    """Abstract base class for classifiers"""

    def __init__(self, bias: float=-1) -> None:
        self.bias = bias
        self.train_losses = np.array([], dtype=float)
        self.val_losses = np.array([], dtype=float)
        self.train_accuracies = np.array([], dtype=float)
        self.val_accuracies = np.array([], dtype=float)
        self.trained_epochs = 0
        self.weights = None

    @abstractmethod
    def predict(self, X: np.ndarray, threshold: Optional[float] = None) -> np.ndarray: ...

    @abstractmethod
    def fit(
        self,
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float = 0.1,
        epochs: int = 10,
        tol: float = 0.01,
        n_epochs_no_update: int = 5,
    ) -> None: ...


class BinaryLinearRegressionClassifier(Classifier):
    def fit(
        self, 
        X_TRAIN: np.ndarray, 
        T_TRAIN: np.ndarray, 
        X_VAL: Optional[np.ndarray], 
        T_VAL: Optional[np.ndarray], 
        learning_rate: float = 0.1, 
        epochs: int = 10,
        tol: float = 0.001,
        n_epochs_no_update: int = 5,
    ) -> None:
        X_TRAIN = add_bias(X_TRAIN, self.bias)
        if X_VAL is not None:
            X_VAL = add_bias(X_VAL, self.bias)

        n_datapoints, n_features = X_TRAIN.shape

        if self.weights is None:
            self.weights = np.zeros(n_features)

        for _ in range(epochs):
            train_predictions = X_TRAIN @ self.weights
            self.train_losses = np.append(self.train_losses, mean_squared_error_loss(train_predictions, T_TRAIN))
            self.train_accuracies = np.append(self.train_accuracies, accuracy((train_predictions > 0.5).astype(int), T_TRAIN))

            # Checking condition every iteration is not optimal, but alterantive is major code duplication
            if X_VAL is not None and T_VAL is not None:
                val_predictions = X_VAL @ self.weights
                self.val_losses = np.append(self.val_losses, mean_squared_error_loss(val_predictions, T_VAL))
                self.val_accuracies = np.append(self.val_accuracies, accuracy((val_predictions > 0.5).astype(int), T_VAL))

            self.weights -= learning_rate / n_datapoints * X_TRAIN.T @ (X_TRAIN @ self.weights - T_TRAIN)

            # Should we return early?
            if self.trained_epochs < n_epochs_no_update:
                self.trained_epochs += 1
            elif self.train_losses[self.train_losses.size - 1] - \
                 self.train_losses[self.train_losses.size - n_epochs_no_update - 1] + tol > 0:
                return
            else:
                self.trained_epochs += 1

    def predict(self, X: np.ndarray, threshold: Optional[float] = 0.5) -> np.ndarray:
        if threshold is None:
            raise ValueError("'threshold' parameter should not be omitted for this class")

        X = add_bias(X, self.bias)

        return (X @ self.weights) > threshold


class BinaryLogisticRegressionClassifier(Classifier):
    def fit(
        self,
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float = 0.1,
        epochs: int = 10,
        tol: float = 0.001,
        n_epochs_no_update: int = 5,
    ) -> None:
        X_TRAIN = add_bias(X_TRAIN, self.bias)
        if X_VAL is not None:
            X_VAL = add_bias(X_VAL, self.bias)

        n_datapoints, n_features = X_TRAIN.shape

        if self.weights is None:
            self.weights = np.zeros(n_features)
        
        for _ in range(epochs):
            train_predictions = self._forward(X_TRAIN)
            self.train_losses = np.append(self.train_losses, cross_entropy_loss(train_predictions, T_TRAIN))
            self.train_accuracies = np.append(self.train_accuracies, accuracy((train_predictions > 0.5).astype(int), T_TRAIN))

            if X_VAL is not None and T_VAL is not None:
                val_predictions = self._forward(X_VAL)
                self.val_losses = np.append(self.val_losses, cross_entropy_loss(val_predictions, T_VAL))
                self.val_accuracies = np.append(self.val_accuracies, accuracy((val_predictions > 0.5).astype(int), T_VAL))

            self.weights -= learning_rate / n_datapoints * X_TRAIN.T @ (train_predictions - T_TRAIN)      

            if self.trained_epochs < n_epochs_no_update:
                self.trained_epochs += 1
            elif self.train_losses[self.train_losses.size - 1] - \
                 self.train_losses[self.train_losses.size - n_epochs_no_update - 1] + tol > 0:
                return
            else:
                self.trained_epochs += 1

    def predict_probability(self, X: np.ndarray) -> np.ndarray:
        X = add_bias(X, self.bias)

        return self._forward(X)
    
    def predict(self, X: np.ndarray, threshold: Optional[float] = 0.5) -> np.ndarray:
        if threshold is None:
            raise ValueError("'threshold' parameter should not be omitted for this class")

        return (self.predict_probability(X) > threshold).astype(int)

    def _forward(self, X: np.ndarray) -> np.ndarray:
        return 1 / (1 + np.exp(-X @ self.weights))


class MultiLogisticRegressionClassifier(Classifier):
    def fit(
        self,
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float = 0.1,
        epochs: int = 10,
        tol: float = 0.001,
        n_epochs_no_update: int = 5,
    ) -> None:
        unique_classes = np.unique(T_TRAIN)
        self.classifiers = [BinaryLogisticRegressionClassifier(self.bias) for _ in unique_classes]

        # Precompute the binary target arrays for training and validation sets
        T_TRAIN_BINARIES = [(T_TRAIN == class_index).astype(int) for class_index in unique_classes]
        T_VAL_BINARIES = [(T_VAL == class_index).astype(int) for class_index in unique_classes] if T_VAL is not None else [None] * len(unique_classes)

        for _ in range(epochs):
            for classifier, T_TRAIN_BINARY, T_VAL_BINARY in zip(self.classifiers, T_TRAIN_BINARIES, T_VAL_BINARIES):
                # Here we only train the classifiers one epoch at a time which makes it significantly slower
                # but gives us more debug information in terms of logging losses and accuracies while training
                classifier.fit(X_TRAIN, T_TRAIN_BINARY, X_VAL, T_VAL_BINARY, learning_rate, 1, tol, n_epochs_no_update)

            # Make predictions using the current state of classifiers and calculate losses and accuracies
            train_predictions = self.predict(X_TRAIN)
            self.train_losses = np.append(self.train_losses, mean_squared_error_loss(train_predictions, T_TRAIN))
            self.train_accuracies = np.append(self.train_accuracies, accuracy(train_predictions, T_TRAIN))

            if X_VAL is not None and T_VAL is not None:
                val_predictions = self.predict(X_VAL)
                self.val_losses = np.append(self.val_losses, mean_squared_error_loss(val_predictions, T_VAL))
                self.val_accuracies = np.append(self.val_accuracies, accuracy(val_predictions, T_VAL))

        # Sum trained epochs across all classifiers
        self.trained_epochs = np.sum([classifier.trained_epochs for classifier in self.classifiers])

    def predict(self, X: np.ndarray, threshold: Optional[float] = None) -> np.ndarray:
        if threshold is not None:
            raise ValueError("'threshold' parameter should be omitted for this class")

        probabilities = np.array([classifier.predict_probability(X) for classifier in self.classifiers])

        return np.argmax(probabilities, axis=0)

class BinaryMLPLinearRegressionClassifier(Classifier):
    def __init__(self, bias: float = -1, dim_hidden: int = 6):
        super().__init__(bias)
        self.dim_hidden = dim_hidden
        self.activ = logistic
        self.activ_diff = logistic_diff
        
    def forward(self, X: np.ndarray):
        hidden_outs = self.activ(X @ self.weights1)
        outputs = hidden_outs @ self.weights2[1:] + self.weights2[0]  # Exclude the bias term from weights2
        return hidden_outs, outputs
    
    def predict_probability(self, X):
        Z = add_bias(X, self.bias)
        forw = self.forward(Z)[1]
        probs = logistic(forw[:, 0])
        return probs
    
    def fit(
        self,
        X_TRAIN: np.ndarray,
        T_TRAIN: np.ndarray,
        X_VAL: Optional[np.ndarray],
        T_VAL: Optional[np.ndarray],
        learning_rate: float = 0.1,
        epochs: int = 10,
        tol: float = 0.001,
        n_epochs_no_update: int = 5,
    ) -> None:
        T_TRAIN = T_TRAIN.reshape(-1, 1)
        X_TRAIN_BIAS = add_bias(X_TRAIN, self.bias)
        dim_in = X_TRAIN.shape[1]
        dim_out = T_TRAIN.shape[1]
        
        self.weights1 = (np.random.rand(dim_in + 1, self.dim_hidden) * 2 - 1) / np.sqrt(dim_in)
        self.weights2 = (np.random.rand(self.dim_hidden + 1, dim_out) * 2 - 1) / np.sqrt(self.dim_hidden)
        
        for _ in range(epochs):
            hidden_outs, outputs = self.forward(X_TRAIN_BIAS)
            probs = logistic(outputs)
            out_deltas = (probs - T_TRAIN) * logistic_diff(probs)
            hiddenout_diffs = out_deltas @ self.weights2[1:].T
            hiddenact_deltas = hiddenout_diffs * self.activ_diff(hidden_outs)
            
            self.weights2[1:] -= learning_rate * hidden_outs.T @ out_deltas
            self.weights2[0] -= learning_rate * np.sum(out_deltas, axis=0)
            self.weights1 -= learning_rate * X_TRAIN_BIAS.T @ hiddenact_deltas

            self.train_losses = np.append(self.train_losses, cross_entropy_loss(probs, T_TRAIN))
            self.train_accuracies = np.append(self.train_accuracies, accuracy(probs, T_TRAIN))
            
            if X_VAL is not None and T_VAL is not None:
                self.val_losses = np.append(self.val_losses, cross_entropy_loss(self.predict_probability(X_VAL), T_VAL))
                self.val_accuracies = np.append(self.val_accuracies, accuracy(probs, T_VAL))

            if self.trained_epochs < n_epochs_no_update:
                self.trained_epochs += 1
            elif self.train_losses[self.train_losses.size - 1] - \
                 self.train_losses[self.train_losses.size - n_epochs_no_update - 1] + tol > 0:
                return
            else:
                self.trained_epochs += 1
    
    def predict(self, X: np.ndarray, threshold: Optional[float] = 0.5) -> np.ndarray:
        if threshold is None:
            raise ValueError("'threshold' parameter should not be omitted for this class")

        probs = self.predict_probability(X)
        return (probs > 0.5).astype(int)
