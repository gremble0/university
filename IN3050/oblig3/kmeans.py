import matplotlib.pyplot as plt
import pca
import syntheticdata
from sklearn.cluster import KMeans


def main() -> None:
    X, y = syntheticdata.get_iris_data()
    _, P = pca.pca(X, X.shape[1])

    # y_hats = []
    ks = [2, 3, 4, 5]
    for i, k in enumerate(ks):
        KM = KMeans(k, random_state=0, n_init="auto")
        # y_hats.append(KM.fit_predict(P))
        y_hat = KM.fit_predict(P)
        ax = plt.subplot(len(ks) // 2, len(ks) // 2, i + 1)
        ax.set_title(f"{k=}")
        plt.scatter(P[:, 0], P[:, 1], c=y_hat)

    plt.savefig("assets/kmeans.png")


if __name__ == "__main__":
    main()
