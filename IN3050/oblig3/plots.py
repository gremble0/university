import matplotlib.pyplot as plt
import numpy as np
import pca
import syntheticdata

def plot_pca() -> None:
    X = syntheticdata.get_synthetic_data1()
    plt.scatter(X[:, 0], X[:, 1])
    plt.title("Synthetic data")
    plt.savefig("assets/synthetic-data1.png")
    plt.clf()

    X_centered = pca.center_data(X)
    plt.title("Synthetic data (centered)")
    plt.savefig("assets/synthetic-data1-centered.png")
    plt.clf()

    eigvec, P = pca.pca(X_centered, X_centered.shape[1])

    first_eigvec = eigvec[:, 0]
    plt.scatter(X_centered[:, 0], X_centered[:, 1])
    x = np.linspace(-5, 5, 1000)
    y = first_eigvec[1] / first_eigvec[0] * x
    plt.plot(x, y)
    plt.title("First eigenvector")
    plt.savefig("assets/first-eigvec.png")
    plt.clf()

    plt.scatter(P[:, 0], np.zeros_like(P[:, 0]))
    plt.title("PCA projection")
    plt.savefig("assets/pca-projection.png")
    

def main() -> None:
    plot_pca()


if __name__ == "__main__":
    main()
