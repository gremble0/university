import numpy as np

from scaling import minmax_scaler, standard_scaler
from common import T_BINARY_TRAIN, T_BINARY_VAL, X_TRAIN, X_VAL, accuracy, plot_decision_regions
from linear import LinearRegressionClassifier


def test_linear_classifier(
    x_train: np.ndarray,
    t_train: np.ndarray,
    x_val: np.ndarray,
    t_val: np.ndarray,
    plot_path: str
) -> None:
    best = 0.0
    best_epochs = None
    best_learning_rate = None
    best_classifier = LinearRegressionClassifier()

    for epoch in range(100):
        for learning_rate in [0.0001, 0.001, 0.01, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5]:
            linear_classifier = LinearRegressionClassifier()
            linear_classifier.fit(x_train, t_train, learning_rate, epoch)

            acc = accuracy(linear_classifier.predict(x_val), t_val)

            if acc > best:
                best = acc
                best_epochs = epoch
                best_learning_rate = learning_rate
                best_classifier = linear_classifier

    print(f"Best accuracy: {best}, with parameters: {best_epochs=}, {best_learning_rate=}")
    plot_decision_regions(x_val, t_val, best_classifier, path=plot_path)


def test_linear_classifier_without_scaling() -> None:
    # Running this code we will find that pretty much any sort of training will
    # decrease the accuracy of the model. The best parameters are 0 epochs, 0
    # learning_rate (Although the learning rate does not matter when we run 0
    # epochs). We can also get the same accuracy with a very low learning rate
    # and in that case the number of epochs doesn't matter.
    #
    # If you change the comparison inside the test_linear_classifer function:
    # ```
    #     if acc > best: ...
    # ```
    # to
    # ```
    #     if acc >= best: ...
    # ```
    # We will get different parameters with the same accuracy.
    # 
    # All three of these alternatives are essentially the same because it
    # means that we are not really training the classifier at all.
    #
    # Either way the best result we can get here seems to be 0.604

    print("Testing linear classifier without scaling")
    test_linear_classifier(X_TRAIN, T_BINARY_TRAIN, X_VAL, T_BINARY_VAL, "assets/linear-without-scaling.png")


def test_linear_classifier_with_scaling() -> None:
    # The optimal parameters for the linear classifier scaled with the standard
    # scaler are 9 epochs with a learning rate of 0.3 resulting in an accuracy
    # of 0.762, and for the minmax scaler its 66 epochs and a learning rate of
    # 0.5 resulting in an accuracy of 0.708. Both are significant improvements
    # compared to the unscaled data.

    print("Testing linear classifier with standard scaling")
    test_linear_classifier(
        standard_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        standard_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/linear-with-scaling.png"
    )

    print("Testing linear classifier with minmax scaler")
    test_linear_classifier(
        minmax_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        minmax_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/linear-with-scaling.png"
    )


def main() -> None:
    test_linear_classifier_without_scaling()
    test_linear_classifier_with_scaling()
    # plot_training_set(X_TRAIN, T_MULTI_TRAIN, "Multi-class set", "assets/multi-class.png")
    # plot_training_set(X_TRAIN, T_BINARY_TRAIN, "Binary set", "assets/binary.png")

if __name__ == "__main__":
    main()
