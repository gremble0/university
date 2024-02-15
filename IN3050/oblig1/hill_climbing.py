import random
from common import fitness


def hill_climbing(city_coordinates: dict[str, list[float]]) -> list[str]:
    cities = list(city_coordinates.keys())
    best_solution = random.sample(cities, len(cities))
    prev_best_solution: list[str] | None = None

    while best_solution != prev_best_solution:
        prev_best_solution = best_solution
        best_solution = best_neighbor(best_solution)

    return list(best_solution)


def best_neighbor(solution: list[str]) -> list[str]:
    best = solution
    best_fitness = fitness(solution)

    for i in range(len(solution) - 1):
        for j in range(i + 1, len(solution)):
            sol = solution
            at_i = sol[i]
            sol[i] = sol[j]
            sol[j] = at_i

            sol_fitness = fitness(sol)
            if sol_fitness > best_fitness:
                best = sol
                best_fitness = sol_fitness

    return best
