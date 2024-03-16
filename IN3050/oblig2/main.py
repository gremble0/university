from common import T_BINARY_TRAIN, T_BINARY_VAL, X_TRAIN, X_VAL, accuracy, plot_decision_regions
from linear import LinearRegressionClassifier


def test_linear_classifier() -> None:
    best = 0.0
    best_params = (None, None)
    best_classifier = LinearRegressionClassifier()

    # Find the best parameters to optimize accuracy on validation set. Running
    # this code we will find that pretty much any sort of training will decrease
    # the accuracy of the model. The best parameters are 0 epochs, 0 learning_rate
    # (Although the learning rate does not matter when we run 0 epochs)
    for epoch in range(100):
        for learning_rate in [0, 0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5]:
            linear_classifier = LinearRegressionClassifier()
            linear_classifier.fit(X_TRAIN, T_BINARY_TRAIN, learning_rate, epoch)

            acc = accuracy(linear_classifier.predict(X_VAL), T_BINARY_VAL)
            print(acc, epoch, learning_rate)

            if acc > best:
                best = acc
                best_params = (epoch, learning_rate)
                best_classifier = linear_classifier

    print(best, best_params)
    plot_decision_regions(X_TRAIN, T_BINARY_TRAIN, best_classifier)


def main() -> None:
    test_linear_classifier()
    # plot_training_set(X_TRAIN, T_MULTI_TRAIN, "Multi-class set", "assets/multi-class.png")
    # plot_training_set(X_TRAIN, T_BINARY_TRAIN, "Binary set", "assets/binary.png")

if __name__ == "__main__":
    main()
