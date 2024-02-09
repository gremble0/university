import matplotlib.pyplot as plt

CITY_COORDINATES = {
    "Barcelona": [2.154007, 41.390205], "Belgrade": [20.46, 44.79], "Berlin": [13.40, 52.52], 
    "Brussels": [4.35, 50.85], "Bucharest": [26.10, 44.44], "Budapest": [19.04, 47.50],
    "Copenhagen": [12.57, 55.68], "Dublin": [-6.27, 53.35], "Hamburg": [9.99, 53.55], 
    "Istanbul": [28.98, 41.02], "Kyiv": [30.52, 50.45], "London": [-0.12, 51.51], 
    "Madrid": [-3.70, 40.42], "Milan": [9.19, 45.46], "Moscow": [37.62, 55.75],
    "Munich": [11.58, 48.14], "Paris": [2.35, 48.86], "Prague": [14.42, 50.07],
    "Rome": [12.50, 41.90], "Saint Petersburg": [30.31, 59.94], "Sofia": [23.32, 42.70],
    "Stockholm": [18.06, 60.33], "Vienna": [16.36, 48.21], "Warsaw": [21.02, 52.24]
}
MAP_BOUNDS = [-14.56, 38.43, 37.697 + 0.3, 64.344 + 2.0]
EUROPE_MAP = plt.imread("map.png")


def plot_plan(city_order: list[str]) -> None:
    _, ax = plt.subplots(figsize=(10, 10))
    ax.imshow(EUROPE_MAP, extent=MAP_BOUNDS, aspect="auto")

    next_x, next_y, index = 0, 0, 0
    for index in range(len(city_order) - 1):
        current_city_coords = CITY_COORDINATES[city_order[index]]
        next_city_coords = CITY_COORDINATES[city_order[index+1]]
        x, y = current_city_coords[0], current_city_coords[1]

        next_x, next_y = next_city_coords[0], next_city_coords[1]
        plt.plot([x, next_x], [y, next_y])

        plt.plot(x, y, 'ok', markersize=5)
        plt.text(x, y, str(index), fontsize=12)

    first_city_coords = CITY_COORDINATES[city_order[0]]
    first_x, first_y = first_city_coords[0], first_city_coords[1]
    plt.plot([next_x, first_x], [next_y, first_y])

    plt.plot(next_x, next_y, 'ok', markersize=5)
    plt.text(next_x, next_y, str(index + 1), fontsize=12)
    plt.savefig("assets/path.png")


plan = list(CITY_COORDINATES.keys())
plot_plan(plan)