from typing import Tuple
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs

from classifiers import Classifier


X, T = make_blobs(
    n_samples=[400, 400, 400, 400, 400],
    centers=[[0, 1], [4, 2], [8, 1], [2, 0], [6, 0]],
    n_features=2,
    random_state=2024,
    cluster_std=[1.0, 2.0, 1.0, 0.5, 0.5]
)

indices = np.arange(X.shape[0])
rng = np.random.RandomState(2024)
rng.shuffle(indices)

X_TRAIN = X[indices[:1000],:]
X_VAL = X[indices[1000:1500],:]
X_TEST = X[indices[1500:],:]

T_MULTI_TRAIN = T[indices[:1000]]
T_MULTI_VAL = T[indices[1000:1500]]
T_MULTI_TEST = T[indices[1500:]]

T_BINARY_TRAIN = (T_MULTI_TRAIN >= 3).astype("int")
T_BINARY_VAL = (T_MULTI_VAL >= 3).astype("int")
T_BINARY_TEST = (T_MULTI_TEST >= 3).astype("int")


def plot_training_set(X: np.ndarray, T: np.ndarray, plot_name: str, path: str) -> None:
    plt.figure(figsize=(8,6))
    plt.scatter(X[:, 0], X[:, 1], c=T, s=10.0)
    plt.title(plot_name)
    plt.savefig(path)


def plot_decision_regions(
    X: np.ndarray,
    T: np.ndarray,
    classifier: Classifier,
    size: Tuple[float, float] = (8,6),
    path: str = "assets/decision-regions.png",
) -> None:
    x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
    y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1

    h = 0.02 # step size in the mesh
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    Z = classifier.predict(np.c_[xx.ravel(), yy.ravel()])

    Z = Z.reshape(xx.shape)

    plt.figure(figsize=size)
    plt.contourf(xx, yy, Z, alpha=0.2, cmap = 'Paired')
    plt.scatter(X[:,0], X[:,1], c=T, s=10.0, cmap='Paired')
    plt.xlim(xx.min(), xx.max())
    plt.ylim(yy.min(), yy.max())
    plt.title("Decision regions")
    plt.xlabel("x0")
    plt.ylabel("x1")

    plt.savefig(path)
