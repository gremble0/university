from dataclasses import dataclass
from typing import Type
import numpy as np

from scaling import minmax_scaler, standard_scaler
from common import T_BINARY_TEST, T_BINARY_TRAIN, T_BINARY_VAL, T_MULTI_TEST, T_MULTI_TRAIN, T_MULTI_VAL, X_TEST, X_TRAIN, X_VAL, plot_decision_regions, plot_losses
from classifiers import Classifier, BinaryLinearRegressionClassifier, BinaryLogisticRegressionClassifier, BinaryMLPLinearRegressionClassifier, MultiLogisticRegressionClassifier, accuracy


@dataclass
class ClassifierParameters:
    learning_rate: float
    epochs: int
    tol: float


def test_classifier(
    X_TRAIN: np.ndarray,
    T_TRAIN: np.ndarray,
    X_VAL: np.ndarray,
    T_VAL: np.ndarray,
    X_TEST: np.ndarray,
    T_TEST: np.ndarray,
    decision_plot_path: str,
    losses_plot_path: str,
    classifier: Type[Classifier],
    is_binary: bool,
) -> ClassifierParameters:
    best_accuracy = 0.0
    best_epochs = None
    best_learning_rate = None
    best_tol = None
    best_c = classifier() # None?

    # Since we're brute forcing so many different combinations of parameters, this may take a while
    # however in practice we would only have to run this once per dataset so thats fine. We could also
    # add more guesses for possible values here if we wanted.
    for epochs in range(1, 20):
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


    test_prediction = best_c.predict(X_TEST)
    test_set_accuracy = accuracy(test_prediction, T_TEST)

    # best_accuracy is calculated after the training so that accuracy may be different from the final accuracy
    # in the best classifiers val_accuracies field
    print(f"Best accuracy after training:       {best_accuracy}")
    print(f"Best parameters:                    {best_epochs=}, {best_learning_rate=}, {best_tol=}")
    print(f"Final accuracy on test set:         {test_set_accuracy}")
    print(f"Loss change for validation set:     {best_c.val_losses[0]:.3f} -> {best_c.val_losses[best_c.val_losses.size - 1]:.3f}")
    print(f"Accuracy change for validation set: {best_c.val_accuracies[0]:.3f} -> {best_c.val_accuracies[best_c.val_accuracies.size - 1]:.3f}")
    # This will most of the time just be the same as best_epochs
    print(f"Number of trained epochs:           {best_c.trained_epochs}")


    if is_binary:
        true_positives = np.sum((test_prediction == 1) & (T_TEST == 1))
        if true_positives == 0:
            print("Algorithm has no true guesses, cannot calculate precision and recall")
        else:
            false_positives = np.sum((test_prediction == 1) & (T_TEST == 0))
            false_negatives = np.sum((test_prediction == 0) & (T_TEST == 1))
            print(f"Precision on test set:              {true_positives / (true_positives + false_positives)}")
            print(f"Recall on test set:                 {true_positives / (true_positives + false_negatives)}")

    print()

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

    return ClassifierParameters(best_learning_rate or 0, best_epochs or 0, best_tol or 0)


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
        X_TEST,
        T_BINARY_TEST,
        "assets/binary-linear-without-scaling.png",
        "assets/binary-linear-without-scaling-losses.png",
        BinaryLinearRegressionClassifier,
        True,
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
    # this dataset so i left it out from the definition of the classifier.

    print("Testing linear classifier with standard scaling")
    test_classifier(
        standard_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        standard_scaler(X_VAL),
        T_BINARY_VAL,
        standard_scaler(X_TEST),
        T_BINARY_TEST,
        "assets/binary-linear-standard-scaling.png",
        "assets/binary-linear-standard-scaling-losses.png",
        BinaryLinearRegressionClassifier,
        True,
    )

    print("Testing linear classifier with minmax scaler")
    test_classifier(
        minmax_scaler(X_TRAIN),
        T_BINARY_TRAIN,
        minmax_scaler(X_VAL),
        T_BINARY_VAL,
        minmax_scaler(X_TEST),
        T_BINARY_TEST,
        "assets/binary-linear-minmax-scaling.png",
        "assets/binary-linear-minmax-scaling-losses.png",
        BinaryLinearRegressionClassifier,
        True,
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
        standard_scaler(X_TEST),
        T_BINARY_TEST,
        "assets/binary-logistic-standard-scaling.png",
        "assets/binary-logistic-standard-scaling-losses.png",
        BinaryLogisticRegressionClassifier,
        True,
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
        standard_scaler(X_TEST),
        T_MULTI_TEST,
        "assets/multi-logistic-standard-scaling.png",
        "assets/multi-logistic-standard-scaling.png",
        MultiLogisticRegressionClassifier,
        False,
    )


def test_binary_mlp_classifier() -> None:
    # The best parameters will vary based on the randomness in the classifier's initialization
    # so we cannot really know the actual best parameters for certain. This will find the
    # parameters that were best for this call of `test_classifier` and then try those params
    # again 10 times and report the mean and standard deviation for that.

    # Running this will result in a number overflow due to lack of regularization or
    # scaling, regardless it will finish with mostly fine results.
    # print("Testing binary multi layer perceptron with no scaling")
    # test_classifier(
    #     X_TRAIN,
    #     T_BINARY_TRAIN,
    #     X_VAL,
    #     T_BINARY_VAL,
    #     minmax_scaler(X_TEST),
    #     T_BINARY_TEST,
    #     "assets/binary-mlp.png",
    #     "assets/binary-mlp-losses.png",
    #     BinaryMLPLinearRegressionClassifier,
    # )

    print("Testing binary multi layer perceptron with standard scaling")

    scaled_x_train = standard_scaler(X_TRAIN)
    scaled_x_val = standard_scaler(X_VAL)
    scaled_x_test = standard_scaler(X_VAL)
    best_params = test_classifier(
        scaled_x_train,
        T_BINARY_TRAIN,
        scaled_x_val,
        T_BINARY_VAL,
        scaled_x_test,
        T_BINARY_TEST,
        "assets/binary-mlp-standard-scaling.png",
        "assets/binary-mlp-standard-scaling-losses.png",
        BinaryMLPLinearRegressionClassifier,
        True,
    )

    print("Getting mean and standard deviation of accuracy for given parameters")

    accuracies = np.array([])
    for _ in range(10):
        c = BinaryMLPLinearRegressionClassifier()
        c.fit(
            scaled_x_train,
            T_BINARY_TRAIN, 
            scaled_x_val, 
            T_BINARY_VAL, 
            best_params.learning_rate, 
            best_params.epochs, 
            best_params.tol,
        )

        accuracies = np.append(accuracies, accuracy(c.predict(scaled_x_val), T_BINARY_VAL))

    # The standard deviation will likely be higher when testing classifiers with a lower number
    # of epochs, in which case the good result is likely a result of rolling a good random seed.
    # However if we keep the range of tested epochs at a decent size (100 above) we will most
    # likely have a relatively low standard deviation and a mean close to the best result.
    print(f"Standard deviation {(np.std(accuracies)):.3f}, mean {(np.mean(accuracies)):.3f}")


def main() -> None:
    # Running all these can take quite a while (~6 minutes on my machine)
    #
    # The three algorithms have vastly different results, and the difference between non-
    # scaled and scaled datasets is significant (using non scaled datasets also proved to
    # lead to number overflows). I also noticed how terribly the algorithms performed when
    # given the opportunity to optimize for the validation set and I had probably set the
    # the range of epochs to test for (100) too high, however this is a number that's easy
    # to tweak after developing the classifiers. When setting this number to something
    # lower like 20, all the numbers in the benchmark shuffle around dramatically.
    # Here the best accuracy on the validation set drops, the accuracy on the test set
    # increases (though is still somewhat low for some algorithms, some of them even
    # being below 50% when testing for up to 20 epochs), and of course the runtimes
    # are drastically shorter since we don't have to test as many combinations.

    test_linear_classifier_without_scaling()
    test_linear_classifier_with_scaling()
    test_logistic_classifier()
    test_multi_logistic_classifier()
    test_binary_mlp_classifier()


if __name__ == "__main__":
    main()
