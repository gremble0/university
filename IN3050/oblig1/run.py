from plot import plot_plan
from exhaustive import exhaustive_search


# Running the assignment tasks here
FIRST_SIX_CITIES = {
    "Barcelona": [2.154007, 41.390205], "Belgrade": [20.46, 44.79], "Berlin": [13.40, 52.52], 
    "Brussels": [4.35, 50.85], "Bucharest": [26.10, 44.44], "Budapest": [19.04, 47.50],
}

FIRST_TEN_CITIES = {
    "Barcelona": [2.154007, 41.390205], "Belgrade": [20.46, 44.79],
    "Berlin": [13.40, 52.52], "Brussels": [4.35, 50.85],
    "Bucharest": [26.10, 44.44], "Budapest": [19.04, 47.50],
    "Copenhagen": [12.57, 55.68], "Dublin": [-6.27, 53.35],
    "Hamburg": [9.99, 53.55], "Istanbul": [28.98, 41.02],
}


# Uncomment to run exhaustive search on the dictionaries above
plot_plan(exhaustive_search(FIRST_SIX_CITIES), "assets/exhaustive_6_cities.png")
plot_plan(exhaustive_search(FIRST_TEN_CITIES), "assets/exhaustive_10_cities.png")
