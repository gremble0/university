from typing import Type
import numpy as np

from scaling import minmax_scaler, standard_scaler
from common import T_BINARY_TRAIN, T_BINARY_VAL, T_MULTI_TRAIN, T_MULTI_VAL, X_TRAIN, X_VAL, plot_decision_regions, plot_losses
from classifiers import Classifier, BinaryLinearRegressionClassifier, BinaryLogisticRegressionClassifier, BinaryMLPLinearRegressionClassifier, MultiLogisticRegressionClassifier, accuracy


# TODO: store accuracy
def test_classifier(
    X_TRAIN: np.ndarray,
    T_TRAIN: np.ndarray,
    X_VAL: np.ndarray,
    T_VAL: np.ndarray,
    decision_plot_path: str,
    losses_plot_path: str,
    classifier: Type[Classifier],
) -> None:
    best_accuracy = 0.0
    best_epochs = None
    best_learning_rate = None
    best_tol = None
    best_c = classifier() # None?

    # Since we're brute forcing so many different combinations of parameters, this may take a while
    # however in practice we would only have to run this once per dataset so thats fine. We could also
    # add more guesses for possible values here if we wanted.
    for epochs in range(1, 100):
        for learning_rate in [0.0001, 0.001, 0.01, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5]:
            for tol in [0.0001, 0.0005, 0.001, 0.01]:
                c = classifier()
                c.fit(X_TRAIN, T_TRAIN, X_VAL, T_VAL, learning_rate, epochs, tol)

                acc = accuracy(c.predict(X_VAL), T_VAL)

                if acc > best_accuracy:
                    best_accuracy = acc
                    best_epochs = epochs
                    best_learning_rate = learning_rate
                    best_tol = tol
                    best_c = c

    # best_accuracy is calculated after the training so that accuracy may be different from the final accuray
    # in the best classifiers val_accuracies field
    print(f"Best accuracy after training: {best_accuracy}, with parameters: {best_epochs=}, {best_learning_rate=}, {best_tol=}")
    if best_c.val_losses.size > 0:
        print(f"Loss function change for value set:           {best_c.val_losses[0]:.3f} -> {best_c.val_losses[best_c.val_losses.size - 1]:.3f}")
        print(f"Accuracy change for value set while training: {best_c.val_accuracies[0]:.3f} -> {best_c.val_accuracies[best_c.val_accuracies.size - 1]:.3f}")
    else:
        print("Classifier did not improve")

    # As far as I'm aware this will always be the same as best_epochs, so we don't even
    # really need to track it, but at least it lets us extract it from outside this local
    # function
    print(f"Classifier trained for {best_c.trained_epochs} epochs\n")

    plot_decision_regions(X_VAL, T_VAL, best_c, path=decision_plot_path)

    # From the plotting of the losses i noticed that some of the classifiers
    # grow to get a lower loss on the validation set than the training set which
    # is somewhat unexpected though not unheard of. All the curves are monotone
    # as the loops above will always choose the best classifier after brute forcing
    # the best parameters, meaning if there is a combination where the graph is not
    # monotone there will always be a combination with fewer epochs that will stop
    # before the graph changes direction. There could theoretically be a combination
    # where the graph briefly changed in the wrong direction, but then later
    # changes back, but all these combinations would be eliminated by the early
    # stopping implemented in task 1.1e.
    plot_losses(best_c.train_losses, best_c.val_losses, path=losses_plot_path)


def test_linear_classifier_without_scaling() -> None:
    # When first running the below code with testing for epochs from 1..100 I
    # found that pretty much any training would decrease the accuracy of the
    # model. The best we could do was get the same accuracy as with an untrained
    # classifier. However after increasing the amount of epochs to 200 I finally
    # found an accuracy ever so slightly better than default. With a learning
    # rate of 0.01 and 194 epochs we can get an accuracy of 0.61.
    # (this is compared to the default value of 0.604). I ended up reverting
    # back to 100 epochs however as 200 took too long and would lead to
    # number overflow errors.

    print("Testing linear classifier without scaling")
    test_classifier(
        X_TRAIN,
        T_BINARY_TRAIN,
        X_VAL,
        T_BINARY_VAL,
        "assets/binary-linear-without-scaling.png",
        "assets/binary-linear-without-scaling-losses.png",
        BinaryLinearRegressionClassifier,
    )


def test_linear_classifier_with_scaling() -> None:
    # The optimal parameters for the linear classifier scaled with the standard
    # scaler are 9 epochs with a learning rate of 0.3 resulting in an accuracy
    # of 0.762, and for the minmax scaler its 66 epochs and a learning rate of
    # 0.5 resulting in an accuracy of 0.708. Both are significant improvements
    # compared to the unscaled data.
    #
    # NOTE: I also noticed that by modifying the threshold of the predict
    # function of the classifier we could further increase the accuracy on the
    # standard scaled data. Here i found that ~0.45 would be the optimal 
    # resulting in an accuracy of 0.79. Though this is definetly specific to
    # this test data so i left it out from the definition of the classifier.

    print("Testing linear classifier with standard scaling")
    test_classifier(
        standard_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        standard_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/binary-linear-standard-scaling.png",
        "assets/binary-linear-standard-scaling-losses.png",
        BinaryLinearRegressionClassifier,
    )

    print("Testing linear classifier with minmax scaler")
    test_classifier(
        minmax_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        minmax_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/binary-linear-minmax-scaling.png",
        "assets/binary-linear-minmax-scaling-losses.png",
        BinaryLinearRegressionClassifier,
    )


def test_logistic_classifier() -> None:
    # For the logistic regression classifier we find the optimal paramters to be
    # 104 epochs with a learning rate of 0.5 resulting in an accuracy of 0.766

    print("Testing binary logistic classifier with standard scaling")
    test_classifier(
        standard_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        standard_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/binary-logistic-standard-scaling.png",
        "assets/binary-logistic-standard-scaling-losses.png",
        BinaryLogisticRegressionClassifier,
    )


def test_multi_logistic_classifier() -> None:
    # Running this test takes a bit longer than the binary classifiers, because
    # we are running several binary classifiers sequentially. The optimal
    # parameters for this algorithm are 99 epochs with a learning rate of 0.5
    # resulting in an accuracy of 0.82. If we remove the tracking of losses
    # and accuracies while training and just train the binary classifiers
    # fully instead of 1 epoch at a time we can also reduce the runtime by ~4x

    print("Testing multi class logistic classifier with standard scaling")
    test_classifier(
        standard_scaler(X_TRAIN),
        T_MULTI_TRAIN,
        standard_scaler(X_VAL),
        T_MULTI_VAL,
        "assets/multi-logistic-standard-scaling.png",
        "assets/multi-logistic-standard-scaling.png",
        MultiLogisticRegressionClassifier,
    )


def test_binary_mlp_classifier() -> None:

    # print("Testing binary multi layer perceptron with no scaling")
    # test_classifier(
    #     X_TRAIN,
    #     T_BINARY_TRAIN,
    #     X_VAL,
    #     T_BINARY_VAL,
    #     "assets/binary-mlp.png",
    #     "assets/binary-mlp-losses.png",
    #     BinaryMLPLinearRegressionClassifier,
    # )

    print("Testing binary multi layer perceptron with standard scaling")
    test_classifier(
        standard_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        standard_scaler(X_VAL),
        T_BINARY_VAL,
        "assets/binary-mlp-standard-scaling.png",
        "assets/binary-mlp-standard-scaling-losses.png",
        BinaryMLPLinearRegressionClassifier,
    )


def main() -> None:
    # test_linear_classifier_without_scaling()
    # test_linear_classifier_with_scaling()
    # test_logistic_classifier()
    test_multi_logistic_classifier()
    # test_binary_mlp_classifier()


if __name__ == "__main__":
    main()
