import numpy as np
import pca
import pickle


def test_center_data() -> None:
    testcase = np.array([[3., 11., 4.3], [4., 5., 4.3], [5., 17., 4.5], [4., 13., 4.4]])
    expected = np.array([[-1., -0.5, -0.075], [0., -6.5, -0.075], [1., 5.5, 0.125], [0., 1.5, 0.025]])

    np.testing.assert_array_almost_equal(pca.center_data(testcase), expected)


def test_compute_covariance_matrix() -> None:
    testcase = np.array([[22., 11., 5.5], [10., 5., 2.5], [34., 17., 8.5], [28., 14., 7]])
    expected = np.cov(testcase.T)

    np.testing.assert_array_almost_equal(pca.compute_covariance_matrix(testcase), expected)


def test_compute_eigenvalue_eigenvectors() -> None:
    testcase = np.array([[2., 0., 0.], [0., 5., 0.], [0., 0., 3.]])
    expected_eigval = np.array([2., 5., 3.])
    expected_eigvec = np.array([[1., 0., 0.], [0., 1., 0.], [0., 0., 1.]])

    eigval, eigvec = pca.compute_eigenvalue_eigenvectors(testcase)

    np.testing.assert_array_almost_equal(eigval, expected_eigval)
    np.testing.assert_array_almost_equal(eigvec, expected_eigvec)


def test_sort_eigenvalue_eigenvectors() -> None:
    testcase = np.array([[2., 0., 0.], [0., 5., 0.], [0., 0., 3.]])
    expected_sorted_eigval = np.array([5., 3., 2.])
    expected_sorted_eigvec = np.array([[0., 0., 1.], [1., 0., 0.], [0., 1., 0.]])

    eig = pca.compute_eigenvalue_eigenvectors(testcase)
    sorted_eigval, sorted_eigvec = pca.sort_eigenvalue_eigenvectors(eig)

    np.testing.assert_array_almost_equal(sorted_eigval, expected_sorted_eigval)
    np.testing.assert_array_almost_equal(sorted_eigvec, expected_sorted_eigvec)


def test_pca() -> None:
    testcase = np.array([[22., 11., 5.5], [10., 5., 2.5], [34., 17., 8.5]])
    x, y = pca.pca(testcase, 2)
    answer1_file = open("assets/PCAanswer1.pkl", "rb")
    answer2_file = open("assets/PCAanswer2.pkl", "rb")
    answer1 = pickle.load(answer1_file)
    answer2 = pickle.load(answer2_file)
    test_arr_x = np.sum(np.abs(np.abs(x) - np.abs(answer1)), axis=0)
    np.testing.assert_array_almost_equal(test_arr_x, np.zeros(2))
    test_arr_y = np.sum(np.abs(np.abs(y) - np.abs(answer2)))
    np.testing.assert_almost_equal(test_arr_y, 0)


def main() -> None:
    test_center_data()
    test_compute_covariance_matrix()
    test_compute_eigenvalue_eigenvectors()
    test_sort_eigenvalue_eigenvectors()
    test_pca()

if __name__ == "__main__":
    main()
