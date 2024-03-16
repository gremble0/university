import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs

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

def plot_training_set(features, clusters, plot_name: str, path: str) -> None:
    plt.figure(figsize=(8,6))
    plt.scatter(features[:, 0], features[:, 1], c=clusters, s=10.0)
    plt.title(plot_name)
    plt.savefig(path)

plot_training_set(X_TRAIN, T_MULTI_TRAIN, "Multi-class set", "assets/multi-class.png")
plot_training_set(X_TRAIN, T_BINARY_TRAIN, "Binary set", "assets/binary.png")
