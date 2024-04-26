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
    plt.scatter(X_centered[:, 0], X_centered[:, 1])
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
    plt.clf()
    

def plot_pca_with_labels() -> None:
    # Comment:
    # The first figure `pca-projection-with-labels-before1.png` is a 2d
    # scatterplot of points colored by their label before reducing the
    # dimensions of the graph. We can also see that this graph is almost
    # linearly separable.
    #
    # The second figure `pca-projection-with-labels-after1.png` is a 1d line
    # of points colored by their label. This plot shows a lossy compression
    # that captures the most important axis of variance. This is benefitial
    # for future classification tasks since it allows for less computationally
    # intensive calculations than in higher dimensional while maintaining
    # most of the original data.
    X, y = syntheticdata.get_synthetic_data_with_labels1()
    X_centered = pca.center_data(X)
    plt.scatter(X_centered[:, 0], X_centered[:, 1], c=y[:, 0])
    plt.title("PCA projection with labels before (dataset 1)")
    plt.savefig("assets/pca-projection-with-labels1-before.png")
    plt.clf()

    _, P = pca.pca(X_centered, X_centered.shape[1])
    plt.scatter(P[:, 0], np.ones(P.shape[0]), c=y[:, 0])
    plt.title("PCA projection with labels after (dataset 1)")
    plt.savefig("assets/pca-projection-with-labels1-after.png")
    plt.clf()

    # Comment:
    # The first figure `pca-projection-with-labels-before2.png` is a 2d
    # scatterplot of points colored by their label before reducing the
    # dimensions of the graph. Compared to the first dataset we can see a more
    # complex structure with a less apparent linear separability, containing 
    # sevaral outlying datapoints within clusters of the the other label.
    #
    # The second figure `pca-projection-with-labels-after.png` is a 1d
    # line of points colored by their label. This plot shows a lossy
    # compression that has attempted to retain most of the data from the
    # original graph unsuccessfully. Much of the information has been lost
    # since we can see from the original graph that both axises are important
    # to classify each individual datapoint, meaning we can not easily discard
    # one axis without losing a large amount of data.
    X, y = syntheticdata.get_synthetic_data_with_labels2()
    X_centered = pca.center_data(X) # unnecessary
    plt.scatter(X_centered[:, 0], X_centered[:, 1], c=y[:, 0])
    plt.title("PCA projection with labels before (dataset 2)")
    plt.savefig("assets/pca-projection-with-labels2-before.png")
    plt.clf()

    _, P = pca.pca(X_centered, X_centered.shape[1])
    plt.scatter(P[:, 0], np.ones(P.shape[0]), c=y[:, 0])
    plt.title("PCA projection with labels after (dataset 2)")
    plt.savefig("assets/pca-projection-with-labels2-after.png")
    plt.clf()


def plot_pca_iris_data() -> None:
    X, y = syntheticdata.get_iris_data()
    X_centered = pca.center_data(X)

    plt.scatter(X_centered[:, 2], X_centered[:, 1], c=y)
    plt.title("2 random features iris data")
    plt.savefig("assets/iris-data-2-random.png")
    plt.clf()

    _, P = pca.pca(X_centered, X_centered.shape[1])
    plt.scatter(P[:, 0], P[:, 1], c=y)
    plt.title("2d PCA projection of iris data")
    plt.savefig("assets/pca-projection-iris-data-2d.png")
    plt.clf()

    plt.scatter(P[:, 0], np.ones(P.shape[0]), c=y)
    plt.title("1d PCA projection of iris data")
    plt.savefig("assets/pca-projection-iris-data-1d.png")
    plt.clf()

    # Comment:
    # We can see from the 2d PCA projection that this dataset is easily
    # reduced to fewer dimensions. By first inspecting the 2d projection we
    # can see that the graph is almost linearly separable, which means we can
    # even reduce it down to 1d and still maintain most of the information.


def plot_pca_lfw_data() -> None:
    X, y, h, w = syntheticdata.get_lfw_data()
    plt.imsave("assets/lfw-data.png", X[0, :].reshape((h, w)))

    ms = [50, 100, 200, 500, 1000, X.shape[1]]
    for i, m in enumerate(ms):
        P = pca.encode_decode_pca(X, m)
        ax = plt.subplot(len(ms) // 2, len(ms) // 2, i + 1)
        ax.set_title(f"{m=}")
        plt.imshow(P[0, :].reshape((h, w)))

    plt.savefig("assets/lfw-different-ms.png")

    # Comment:
    # From these images we can see that we can mostly recreate the original
    # image by uncompressing it after running PCA on it. The reconstructed
    # image shows most of the characteristics of the original, though with
    # some amount of loss, especially for smaller values for `m`. We can also
    # see some sort of diminishing returns for increasing the value for `m`
    # closer to its original size, as the difference between m=1000 and
    # m=M.shape[1] is very minimal


def main() -> None:
    plot_pca()
    plot_pca_with_labels()
    plot_pca_iris_data()
    plot_pca_lfw_data()


if __name__ == "__main__":
    main()
