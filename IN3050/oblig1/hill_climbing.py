import random

from itertools import permutations
from common import fitness


def hill_climbing(city_coordinates: dict[str, list[float]]) -> list[str]:
    all_permutations = list(permutations(city_coordinates.keys()))

    best_solution = random.choice(all_permutations)
    prev_best_solution: tuple[str, ...] | None = None

    while best_solution != prev_best_solution:
        prev_best_solution = best_solution
        best_solution = better_neighbor(best_solution)

    return list(best_solution)


def better_neighbor(solution: tuple[str, ...]) -> tuple[str, ...]:
    pass
