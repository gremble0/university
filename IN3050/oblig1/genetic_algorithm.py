import random

def genetic_algorithm(
    city_coordinates: dict[str, list[float]],
    population_size: int,
    num_elites: int,
) -> list[str]:
    population = random.sample(list(city_coordinates.keys()), population_size)

    return []

def mutate(solution: list[str]) -> list[str]:
    city1_i, city2_i = random.sample(range(len(solution)), 2)

    new_solution = solution[:] # copy to not mutate input
    new_solution[city1_i], new_solution[city2_i] = solution[city2_i], solution[city1_i]

    return new_solution

def crossover(solution1: list[str], solution2: list[str]) -> list[str]:
    split_index = random.choice(range(len(solution1)))

    return [solution1[i] if i < split_index else solution2[i] for i in range(len(solution1))]
