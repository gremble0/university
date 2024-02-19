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
        population = get_next_generation(population, elites)
        # print(elites)
        i += 1

    # first list() casts from type dict_keys, second list() casts from type tuple
    return tuple(list(elites.keys())[0])


def get_next_generation(
    prev_gen: dict[tuple[str, ...], float],
    elites: dict[tuple[str, ...], float],
) -> dict[tuple[str, ...], float]:
    """Decides what the next generation should look like and updates
       elites in place if a better solution was found
    """
    new_gen = dict(prev_gen) # copy the dict
    for solution in prev_gen:
        mutated = mutate(solution)

        # Dont calculate fitness if we already have the solution
        if mutated in new_gen:
            continue

        mutated_fitness = fitness(mutated)
        worst_elite = min(elites, key=elites.get)
        if mutated_fitness > elites[worst_elite]:
            print(mutated_fitness, worst_elite, elites[worst_elite])
            elites.pop(worst_elite)
            elites[mutated] = mutated_fitness

        new_gen[mutated] = mutated_fitness


    # Sort by fitness, and return a dict of the first len(prev_gen) values
    # to keep the population size constant
    return dict(sorted(new_gen.items(), key=lambda x: x[1])[:len(prev_gen)])


def mutate(solution: tuple[str, ...]) -> tuple[str, ...]:
    city1_i, city2_i = random.sample(range(len(solution)), 2)

    new_solution = list(solution)
    new_solution[city1_i], new_solution[city2_i] = solution[city2_i], solution[city1_i]

    return tuple(new_solution)


def crossover(solution1: list[str], solution2: list[str]) -> list[str]:
    split_index = random.randint(0, len(solution1) - 1)

    return [solution1[i] if i < split_index else solution2[i] for i in range(len(solution1))]
