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
        new_gen = dict(population) # copy the dict for temporary changes
        for solution in population:
            new_solution = mutate(solution)
            # this could be optimized by adding weights on who to crossover with
            new_solution = crossover(new_solution, random.choice(tuple(population.keys())))

            # no reason to handle this solution if we already have it
            if new_solution in new_gen:
                continue

            mutated_fitness = fitness(new_solution)
            worst_elite = max(elites, key=lambda k: elites.get(k, float("inf")))
            if mutated_fitness < elites[worst_elite]:
                elites.pop(worst_elite)
                elites[new_solution] = mutated_fitness

            new_gen[new_solution] = mutated_fitness


        # sort by fitness, and set the population to the fittest solutions
        # this is not particularly smart - pretty much just pure exploitation.
        population = dict(sorted(new_gen.items(), key=lambda x: x[1])[:len(population)])

    return tuple(elites.keys())[0]


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
