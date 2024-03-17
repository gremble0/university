import numpy as np


def standard_scaler(X: np.ndarray) -> np.ndarray:
    return (X - np.mean(X, axis=0)) / np.std(X, axis=0)
