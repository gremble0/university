from typing import Tuple
import numpy as np


def center_data(X: np.ndarray) -> np.ndarray:
    return X - X.mean(axis=0)


def compute_covariance_matrix(X: np.ndarray) -> np.ndarray:
    centered = center_data(X)
    return (centered.T @ centered) / (X.shape[0] - 1)


def compute_eigenvalue_eigenvectors(X: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    eig = np.linalg.eig(X)
    return eig.eigenvalues.real, eig.eigenvectors.real


def sort_eigenvalue_eigenvectors(eigval: np.ndarray, eigvec: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    return np.sort(eigval)[::-1], np.sort(eigvec)[::-1]
