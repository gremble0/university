from itertools import permutations
import csv

def exhaustive_search(city_coordinates: dict[str, list[float]]) -> list[str]:
    with open("assets/european_cities.csv", "r") as f:
        data = list(csv.reader(f, delimiter=";"))

    cities = sorted(city_coordinates.keys())

    indexes: dict[str, int] = {}
    for i in range(len(cities)):
        indexes[cities[i]] = i

    all_permutations = list(permutations(cities))
    shortest = all_permutations[0]
    shortest_distance = float("inf")

    for permutation in all_permutations:
        permutation_distance = 0

        for i in range(len(permutation) - 1):
            distances_from_i = data[indexes[permutation[i]] + 1]
            distance_to_next = distances_from_i[indexes[permutation[i + 1]]]
            permutation_distance += float(distance_to_next)

        if permutation_distance < shortest_distance:
            shortest = permutation
            shortest_distance = permutation_distance

    return list(shortest)
