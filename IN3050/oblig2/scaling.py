import numpy as np


def standard_scaler(X: np.ndarray) -> np.ndarray:
    return (X - np.mean(X, axis=0)) / np.std(X, axis=0)


def minmax_scaler(X: np.ndarray) -> np.ndarray:
    return (X - X.min(axis=0)) / (X.max(axis=0) - X.min(axis=0))
