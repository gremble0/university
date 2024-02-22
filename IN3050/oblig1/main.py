from typing import Callable
from dataclasses import dataclass
from math import factorial, sqrt
from common import CITY_COORDINATES, fitness, plot_plan
from timeit import default_timer
import matplotlib.pyplot as plt

from exhaustive_search import exhaustive_search
from hill_climbing import hill_climbing
from genetic_algorithm import genetic_algorithm, genetic_algorithm_with_debug

SIX_CITIES = dict(list(CITY_COORDINATES.items())[:6])
TEN_CITIES = dict(list(CITY_COORDINATES.items())[:10])
ALL_CITIES = CITY_COORDINATES


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
    plot_plan(solution, filename)


@dataclass
class Report:
    fitnesses: list[float]
    
    @property
    def best(self) -> float:
        return min(self.fitnesses)

    @property
    def worst(self) -> float:
        return max(self.fitnesses)

    @property
    def mean(self) -> float:
        return sum(self.fitnesses) / len(self.fitnesses)

    @property
    def standard_deviation(self) -> float:
        mean = self.mean # calculate once
        return sqrt(sum([(x - mean) ** 2 for x in self.fitnesses]) / len(self.fitnesses))

    def print(self, algo_name: str, **parameters):
        print(f"Report for {algo_name} with {parameters=}")
        print("Best:", self.best)
        print("Worst:", self.worst)
        print("Mean:", self.mean)
        print("Standard deviation:", self.standard_deviation)


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

    # Since we know exhaustive search will always find the best solution there is
    # no point in making reports for different runs

    print("\nTime to run exhaustive search on all 24 cities:", time_24_cities)


def test_hill_climbing() -> None:
    run_and_plot(hill_climbing, TEN_CITIES)
    run_and_plot(hill_climbing, ALL_CITIES)

    hc_10_results: list[float] = []
    hc_24_results: list[float] = []

    for _ in range(20):
        hc_10_results.append(fitness(hill_climbing(TEN_CITIES)))
        hc_24_results.append(fitness(hill_climbing(ALL_CITIES)))

    print()
    Report(hc_10_results).print("Hill climbing", tour_length=len(TEN_CITIES))
    print()
    Report(hc_24_results).print("Hill climbing", tour_length=len(ALL_CITIES))


def test_genetic_algorithm() -> None:
    # My genetic algorithm does consistently find the shortest tour for
    # the first ten cities. (After running this function you can check this
    # by checking the plot generated under assets/genetic_algorithm_10_cities.png)

    # For the genetic algorithm the runtimes depend on the parameters we give it.
    # For my default parameters of population_size=100 and num_generations=500
    # these are the runtimes:
    #
    # SIX_CITIES: ~0.5s
    # TEN_CITIES: ~0.8s
    # ALL_CITIES: ~1.3s
    #
    # This is a significant improvement over exhaustive search, even on smaller
    # tours, where exhaustive search takes ~10 seconds for TEN_CITIES, and would
    # take several million years for ALL_CITIES.
    
    # In terms of tours inspected for exhaustive search it would have to visit every
    # possible tour. This would give the following values:
    #
    # SIX_CITIES: 6!  = 720
    # TEN_CITIES: 10! = 3628800
    # ALL_CITIES: 24! = 6.204484e+23
    #
    # for my genetic algorithm the number of tours would be equal to:
    # population_size * num_generations = 100 * 500 = 50000. This is independent
    # of the amount of cities in question, therefore its probably a good idea to
    # increase the parameter values for bigger tours.

    run_and_plot(genetic_algorithm, SIX_CITIES)
    run_and_plot(genetic_algorithm, TEN_CITIES)
    run_and_plot(genetic_algorithm, ALL_CITIES)

    # For a population size of 100 it seems it takes ~500 generations before it
    # stops regularly improving each generation. It is also not guaranteed to
    # always find the best solution. For bigger populations we could also 
    # decrease the number of generations and see similar results which makes
    # sense, however that would complicate the benchmarking below so I've left
    # the number of generations constant at 500.

    # My genetic algorithm also initially kept track of elites, but i realized
    # that is not necessary for my solution as each generation always keeps
    # the best solutions so i simply removed the elites from the algorithm

    plt.figure(figsize=(10,6))
    population_sizes = [50, 100, 200]
    num_generations = 500
    for population_size in population_sizes:
        fitnesses: list[float] = [float("inf")] * num_generations
        results: list[float] = []
        for _ in range(20):
            result, fits = genetic_algorithm_with_debug(
                ALL_CITIES,
                population_size=population_size,
                num_generations=num_generations
            )
            for i, fit in enumerate(fits):
                if fit < fitnesses[i]:
                    fitnesses[i] = fit
            results.append(fitness(result))

        plt.plot(range(num_generations), fitnesses, label=f"Population Size {population_size}")

        Report(results).print("Genetic algorithm", population_size=population_size)
        print()

    plt.xlabel("Generation")
    plt.ylabel("Best fitness for generation (lower is better)")
    plt.title("Best fitness over generations for different population sizes")
    plt.savefig("assets/genetic_algorithm_report.png")


def main() -> None:
    # Running all these functions at once could take a while (~1 minute
    # on my machine), so its probably better to only do one per run

    test_exhaustive_search()
    test_hill_climbing()
    test_genetic_algorithm()


if __name__ == "__main__":
    main()
