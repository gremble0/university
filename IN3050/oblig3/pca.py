from typing import Tuple
from numpy.linalg.linalg import EigResult
import numpy as np


def center_data(X: np.ndarray) -> np.ndarray:
    return X - X.mean(axis=0)


def compute_covariance_matrix(X: np.ndarray) -> np.ndarray:
    centered = center_data(X)
    return (centered.T @ centered) / (X.shape[0] - 1)


def compute_eigenvalue_eigenvectors(X: np.ndarray) -> EigResult:
    eig = np.linalg.eig(X)
    return EigResult(eig.eigenvalues.real, eig.eigenvectors.real)


# Slightly different signature than from oblig, but effectively the same
def sort_eigenvalue_eigenvectors(eig: EigResult) -> EigResult:
    sorted_indicies = np.argsort(eig.eigenvalues)[::-1]
    return EigResult(eig.eigenvalues[sorted_indicies], eig.eigenvectors[:, sorted_indicies])


def pca(X: np.ndarray, n_learned_features: int) -> Tuple[np.ndarray, np.ndarray]:
    centered = center_data(X)
    cov = compute_covariance_matrix(centered)

    eig = compute_eigenvalue_eigenvectors(cov)
    eig_sorted = sort_eigenvalue_eigenvectors(eig)

    pca_eigvec = eig_sorted.eigenvectors[:, :n_learned_features]

    return pca_eigvec, centered @ pca_eigvec


def encode_decode_pca(X: np.ndarray, m: int) -> np.ndarray:
    eigvecs, P = pca(X, m)

    return P.dot(eigvecs.T)
