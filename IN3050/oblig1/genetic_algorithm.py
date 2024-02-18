import random

from common import fitness


def genetic_algorithm(
    city_coordinates: dict[str, list[float]],
    population_size: int,
    num_elites: int,
) -> list[str]:
    # generate population_size random solutions
    cities = city_coordinates.keys()
    solutions = [random.sample(list(cities), len(cities)) for _ in range(population_size)]
    population = {tuple(solutions[i]): fitness(solutions[i]) for i in range(population_size)}

    # keep track of best seen solutions (this just makes a dict from the first
    # num_elites values in the dict)
    elites = dict(list(population.items())[:num_elites])
    
    print(population)
    while True:
        new_population = population.keys()
        for solution in new_population:
            solution = mutate(solution)

    # first list() casts from type dict_keys, second list() casts from type tuple
    return list(list(elites.keys())[0])


def mutate(solution: list[str]) -> list[str]:
    city1_i, city2_i = random.sample(range(len(solution)), 2)

    new_solution = solution[:] # copy to not mutate input
    new_solution[city1_i], new_solution[city2_i] = solution[city2_i], solution[city1_i]

    return new_solution


def crossover(solution1: list[str], solution2: list[str]) -> list[str]:
    split_index = random.choice(range(len(solution1)))

    return [solution1[i] if i < split_index else solution2[i] for i in range(len(solution1))]
