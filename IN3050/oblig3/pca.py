import numpy as np
#import matplotlib.pyplot as plt

#import syntheticdata


def center_data(A: np.ndarray) -> np.ndarray:
    return A - A.mean(axis=0)


def compute_covariance_matrix(A: np.ndarray) -> np.ndarray:
    centered = center_data(A)
    return A
