import numpy as np

from scaling import standard_scaler
from common import T_BINARY_TRAIN, T_BINARY_VAL, X_TRAIN, X_VAL, accuracy, plot_decision_regions
from linear import LinearRegressionClassifier


def test_linear_classifier(
    x_train: np.ndarray,
    t_train: np.ndarray,
    x_val: np.ndarray,
    t_val: np.ndarray,
) -> None:
    best = 0.0
    best_epochs = None
    best_learning_rate = None
    best_classifier = LinearRegressionClassifier()

    for epoch in range(100):
        for learning_rate in [0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5]:
            linear_classifier = LinearRegressionClassifier()
            linear_classifier.fit(x_train, t_train, learning_rate, epoch)

            acc = accuracy(linear_classifier.predict(x_val), t_val)

            if acc > best:
                best = acc
                best_epochs = epoch
                best_learning_rate = learning_rate
                best_classifier = linear_classifier

    print(f"Best accuracy: {best}, with parameters: {best_epochs=}, {best_learning_rate=}")
    plot_decision_regions(x_val, t_val, best_classifier)


def test_linear_classifier_without_scaling() -> None:
    # TODO: redo comment
    # Find the best parameters to optimize accuracy on validation set. Running
    # this code we will find that pretty much any sort of training will decrease
    # the accuracy of the model. The best parameters are 0 epochs, 0 learning_rate
    # (Although the learning rate does not matter when we run 0 epochs). We can
    # also get the same accuracy with a very low learning rate and in that case
    # the number of epochs doesn't matter. If you change the comparison
    # ```
    #     if acc > best: ...
    # ```
    # to
    # ```
    #     if acc >= best: ...
    # ```
    # We will get different parameters with the same accuracy.
    test_linear_classifier(X_TRAIN, T_BINARY_TRAIN, X_VAL, T_BINARY_VAL)


def test_linear_classifier_with_scaling() -> None:
    test_linear_classifier(
        standard_scaler(X_TRAIN),
        standard_scaler(T_BINARY_TRAIN),
        standard_scaler(X_VAL),
        standard_scaler(T_BINARY_VAL),
    )


def main() -> None:
    test_linear_classifier_without_scaling()
    test_linear_classifier_with_scaling()
    # plot_training_set(X_TRAIN, T_MULTI_TRAIN, "Multi-class set", "assets/multi-class.png")
    # plot_training_set(X_TRAIN, T_BINARY_TRAIN, "Binary set", "assets/binary.png")

if __name__ == "__main__":
    main()
