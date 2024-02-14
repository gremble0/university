from math import factorial
from common import CITY_COORDINATES
from plot import plot_plan
import time

from exhaustive_search import exhaustive_search
from hill_climbing import hill_climbing


SIX_CITIES = dict(list(CITY_COORDINATES.items())[:6])
TEN_CITIES = dict(list(CITY_COORDINATES.items())[:10])
ALL_CITIES = CITY_COORDINATES


# ---------- EXHAUSTIVE SEARCH ---------- 
plot_plan(exhaustive_search(SIX_CITIES), "assets/exhaustive_6_cities.png")
plot_plan(exhaustive_search(TEN_CITIES), "assets/exhaustive_10_cities.png")

# The shortest way to travel between the first 10 cities is the following sequence and is 5272.68km:
#
#       ('Barcelona', 'Dublin', 'Brussels', 'Hamburg', 'Copenhagen',
#        'Berlin', 'Budapest', 'Belgrade', 'Bucharest', 'Istanbul')
#
# On my computer this sequence consistently takes ~10 seconds to find with exhaustive search.
# Knowing the time complexity of this algorithm to be O(n!) we can then approximate how long it
# would take to run exhaustive search on all 24 cities.
time_10_cities = 10.0
factorial_ratio = factorial(24) / factorial(10)
time_24_cities = time_10_cities * factorial_ratio

print("Time to run exhaustive search on all 24 cities:", time_24_cities) # -> 1.78 * 10^18 (~54.2 million years)


# ---------- HILL CLIMBING ---------- 
plot_plan(hill_climbing(TEN_CITIES), "assets/hill_climbing_10_cities.png")
plot_plan(hill_climbing(ALL_CITIES), "assets/hill_climbing_24_cities.png")

ten_cities_times = []
twentyfour_cities_times = []

for i in range(20):
    before_ten_cities = time.time()
    hill_climbing(TEN_CITIES)
    ten_cities_times.append(time.time() - before_ten_cities)
    
    before_twentyfour_cities = time.time()
    hill_climbing(ALL_CITIES)
    twentyfour_cities_times.append(time.time() - before_twentyfour_cities)

ten_cities_avg = sum(ten_cities_times) / len(ten_cities_times)
twentyfour_cities_avg = sum(twentyfour_cities_times) / len(twentyfour_cities_times)
print(f"{ten_cities_avg=:.5f}\n{twentyfour_cities_avg=:.5f}")
