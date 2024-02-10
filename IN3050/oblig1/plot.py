from pathlib import Path
import matplotlib.pyplot as plt
from common import EUROPE_MAP, MAP_BOUNDS, CITY_COORDINATES


def plot_plan(city_order: list[str], file_name: str | Path) -> None:
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
    plt.savefig(file_name)
