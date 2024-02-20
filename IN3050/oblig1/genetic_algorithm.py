import random

from common import fitness


def genetic_algorithm(
    city_coordinates: dict[str, list[float]],
    population_size: int = 100,
    num_elites: int = 2,
    num_generations: int = 5000,
) -> tuple[str, ...]:
    # generate population_size random solutions
    cities = list(city_coordinates.keys())
    population = dict(map(lambda solution: (solution, fitness(solution)),
                 [tuple(random.sample(cities, len(cities))) for _ in range(population_size)]))

    # keep track of best seen solutions (this just makes a dict from the first
    # num_elites values in the dict)
    elites = dict(list(population.items())[:num_elites])
    
    for _ in range(num_generations):
        population = get_next_generation(population, elites)

    return tuple(elites.keys())[0]


def get_next_generation(
    prev_gen: dict[tuple[str, ...], float],
    elites: dict[tuple[str, ...], float],
) -> dict[tuple[str, ...], float]:
    """Decides what the next generation should look like and updates
       elites in place if a better solution was found"""

    new_gen = dict(prev_gen) # copy the dict
    for solution in prev_gen:
        new_solution = mutate(solution)
        new_solution = crossover(new_solution, random.choice(tuple(prev_gen.keys())))

        # Dont calculate fitness if we already have the solution
        if new_solution in new_gen:
            continue

        mutated_fitness = fitness(new_solution)
        worst_elite = max(elites, key=lambda k: elites.get(k, float("inf")))
        if mutated_fitness < elites[worst_elite]:
            elites.pop(worst_elite)
            elites[new_solution] = mutated_fitness

        new_gen[new_solution] = mutated_fitness


    # Sort by fitness, and return a dict of the first len(prev_gen) values
    # to keep the population size constant
    return dict(sorted(new_gen.items(), key=lambda x: x[1])[:len(prev_gen)])


def mutate(solution: tuple[str, ...]) -> tuple[str, ...]:
    """Swap two random cities in the solution"""
    city1_i, city2_i = random.sample(range(len(solution)), 2)

    new_solution = list(solution)
    new_solution[city1_i], new_solution[city2_i] = solution[city2_i], solution[city1_i]

    return tuple(new_solution)


def crossover(
    solution1: tuple[str, ...],
    solution2: tuple[str, ...]
) -> tuple[str, ...]:
    split_index = random.randint(0, len(solution1) - 1)
    solution_builder = list(solution1[:split_index])
    for city in solution2:
        if city not in solution_builder:
            solution_builder.append(city)

    return tuple(solution_builder)
