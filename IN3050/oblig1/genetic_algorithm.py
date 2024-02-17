import random

def genetic_algorithm(
    city_coordinates: dict[str, list[float]],
    population_size: int,
    num_elites: int,
) -> list[str]:
    population = random.sample(list(city_coordinates.keys()), population_size)

    while True:


    return []

def mutate(solution: list[str]) -> list[str]:
    # swap 2 random cities
    return []

def crossover(solution1: list[str], solution2: list[str]) -> list[str]:
    return []

def n_crossover(*solutions: list[str]) -> list[str]:
    return []
