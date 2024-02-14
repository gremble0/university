import random

from itertools import permutations
from common import fitness


def hill_climbing(city_coordinates: dict[str, list[float]]) -> list[str]:
    all_permutations = list(permutations(city_coordinates.keys()))

    best_solution = random.choice(all_permutations)
    best_solution_fitness = fitness(best_solution)

    while ...:
        pass

    return list(best_solution)
