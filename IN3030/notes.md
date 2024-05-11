## Random
Speed of light: 299 792 458 m/s

## Cache friendliness of bubblesort
Good:
- Sequential access to the array.
- Temporal locality - frequently accesses the same elements within a short period of time
- Spatial locality - frequently accesses adjacent elements in array
Bad:
- Generally inefficient algorithm with O(n^2) complexity (not necessarily bad, but we know bubblesort is inefficient)
- Nested for loop means that after each inner for loop completes it will jump back to the start of the array (jumping around in memory is bad)
- Cache saturation - when array size is larger than the cache size the cache may need to evict data that may be needed later (more overhead for managing cache + future cache misses is bad)
