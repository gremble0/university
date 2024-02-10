from itertools import permutations
from common import fitness


def exhaustive_search(city_coordinates: dict[str, list[float]]) -> list[str]:
    all_permutations = list(permutations(city_coordinates.keys()))

    best_solution = all_permutations[0]
    best_solution_fitness: float = fitness(best_solution) # TODO: FIX

    for permutation in all_permutations[1:]:
        permutation_fitness = fitness(permutation)

        if permutation_fitness < best_solution_fitness:
            best_solution = permutation
            best_solution_fitness = permutation_fitness

    return list(best_solution)
