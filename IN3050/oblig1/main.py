from typing import Callable
from math import factorial
from genetic_algorithm import genetic_algorithm
from common import CITY_COORDINATES, fitness, plot
from timeit import default_timer

from exhaustive_search import exhaustive_search
from hill_climbing import hill_climbing

SIX_CITIES = dict(list(CITY_COORDINATES.items())[:6])
TEN_CITIES = dict(list(CITY_COORDINATES.items())[:10])
ALL_CITIES = CITY_COORDINATES

def set_function_name(func: Callable, name: str) -> Callable:
    func.__name__ = name
    return func



def run_and_plot(
    algorithm: Callable[[dict[str, list[float]]], tuple[str, ...]],
    city_coordinates: dict[str, list[float]]
) -> None:
    print("---------------------------------------")
    print(f"Running {algorithm.__name__} on {tuple(city_coordinates.keys())}\n")
    solution = algorithm(city_coordinates)
    print(f"Found this solution: {solution}, with a total distance of {fitness(solution)}km\n")

    filename = f"assets/{algorithm.__name__}_{len(solution)}_cities.png"
    print(f"Plotting solution and saving it as '{filename}'")
    plot(solution, filename)


def test_exhaustive_search() -> None:
    run_and_plot(exhaustive_search, SIX_CITIES)

    before_10_cities = default_timer()
    run_and_plot(exhaustive_search, TEN_CITIES)
    after_10_cities = default_timer()

    # The shortest way to travel between the first 10 cities is the following
    # sequence and is 5272.68km:
    #
    #       ('Barcelona', 'Dublin', 'Brussels', 'Hamburg', 'Copenhagen',
    #        'Berlin', 'Budapest', 'Belgrade', 'Bucharest', 'Istanbul')
    #
    # Knowing the time complexity of exhaustive search to be O(n!) we can then 
    # approximate how long it would take to run exhaustive search on all 24
    # cities programatically:

    time_10_cities = after_10_cities - before_10_cities
    factorial_ratio = factorial(24) / factorial(10)
    time_24_cities = time_10_cities * factorial_ratio

    # On my computer time_10_cities is usually ~10 seconds making time_24_cities
    # this number: 1.78 * 10^18 (~54.2 million years)

    print("\nTime to run exhaustive search on all 24 cities:", time_24_cities)


def test_hill_climbing() -> None:
    run_and_plot(hill_climbing, TEN_CITIES)
    run_and_plot(hill_climbing, ALL_CITIES)

    ten_cities_times = []
    twentyfour_cities_times = []

    for _ in range(20):
        before_ten_cities = default_timer()
        hill_climbing(TEN_CITIES)
        ten_cities_times.append(default_timer() - before_ten_cities)
        
        before_twentyfour_cities = default_timer()
        hill_climbing(ALL_CITIES)
        twentyfour_cities_times.append(default_timer() - before_twentyfour_cities)

    ten_cities_avg = sum(ten_cities_times) / len(ten_cities_times)
    twentyfour_cities_avg = sum(twentyfour_cities_times) / len(twentyfour_cities_times)
    print(f"\n{ten_cities_avg=:.5f}\n{twentyfour_cities_avg=:.5f}")


def test_genetic_algorithm() -> None:
    run_and_plot(genetic_algorithm, TEN_CITIES)
    run_and_plot(genetic_algorithm, ALL_CITIES)


def main() -> None:
    test_exhaustive_search()
    test_hill_climbing()
    test_genetic_algorithm()


if __name__ == "__main__":
    main()
