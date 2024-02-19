import random

from common import fitness


def genetic_algorithm(
    city_coordinates: dict[str, list[float]],
    population_size: int,
    num_elites: int,
    num_generations: int,
) -> tuple[str, ...]:
    # generate population_size random solutions
    cities = list(city_coordinates.keys())
    solutions = [tuple(random.sample(cities, len(cities))) for _ in range(population_size)]
    population = {solutions[i]: fitness(solutions[i]) for i in range(population_size)}

    # keep track of best seen solutions (this just makes a dict from the first
    # num_elites values in the dict)
    elites = dict(list(population.items())[:num_elites])
    
    i = 0
    while i < num_generations:
        new_population = list(population.keys())
        for j, solution in enumerate(new_population):
            new_population[j] = mutate(solution)

        i += 1

    # first list() casts from type dict_keys, second list() casts from type tuple
    return tuple(list(elites.keys())[0])


def mutate(solution: tuple[str, ...]) -> tuple[str, ...]:
    city1_i, city2_i = random.sample(range(len(solution)), 2)

    new_solution = list(solution)
    new_solution[city1_i], new_solution[city2_i] = solution[city2_i], solution[city1_i]

    return tuple(new_solution)


def crossover(solution1: list[str], solution2: list[str]) -> list[str]:
    split_index = random.choice(range(len(solution1)))

    return [solution1[i] if i < split_index else solution2[i] for i in range(len(solution1))]
