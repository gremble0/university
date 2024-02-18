import random
from common import fitness


def hill_climbing(city_coordinates: dict[str, list[float]]) -> tuple[str, ...]:
    cities = list(city_coordinates.keys())
    best_solution = tuple(random.sample(cities, len(cities)))
    prev_best_solution = ()

    while best_solution != prev_best_solution:
        prev_best_solution = best_solution
        best_solution = best_neighbor(best_solution)

    return best_solution


def best_neighbor(solution: tuple[str, ...]) -> tuple[str, ...]:
    best_solution = solution
    best_distance = fitness(solution)

    for i in range(1, len(solution) - 1):
        for j in range(i + 1, len(solution)):
            if j - i == 1:
                continue

            new_solution_list = list(solution)
            new_solution_list[i:j] = reversed(solution[i:j])

            new_solution = tuple(new_solution_list)
            new_distance = fitness(new_solution)

            if new_distance < best_distance:
                best_distance = new_distance
                best_solution = new_solution

    return best_solution
