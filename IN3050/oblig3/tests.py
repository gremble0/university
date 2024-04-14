import numpy as np

from pca import center_data


def test_center_data() -> None:
    testcase = np.array([[3., 11., 4.3], [4., 5., 4.3], [5., 17., 4.5], [4., 13., 4.4]])
    expected = np.array([[-1., -0.5, -0.075], [0., -6.5, -0.075], [1., 5.5, 0.125], [0., 1.5, 0.025]])
    np.testing.assert_array_almost_equal(center_data(testcase), expected)


def test_compute_covariance_matrix() -> None:
    ...


def main() -> None:
    test_center_data()
    test_compute_covariance_matrix()

if __name__ == "__main__":
    main()
