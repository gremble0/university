import matplotlib.pyplot as plt

parallel_20  = [ 1155928, 1253907, 3243512,  48633335, 44113476, 55666825 ]
parallel_100 = [ 1593436, 9963199, 15119248, 50186878, 50006029, 50247792 ]

sequential_20  = [ 29630,  307620, 341299, 3317943, 5701228, 76669546 ]
sequential_100 = [ 505127, 400890, 353489, 2877193, 7556810, 58981128 ]

arrays_sort_20  = [ 478019, 4009731, 29946943, 127585332, 719999086, 8150233535 ]
arrays_sort_100 = [ 486569, 4506705, 30373003, 120552931, 748318761, 8006538306 ]

n_values = [1000, 10000, 100000, 1000000, 10000000, 100000000]

plt.figure(figsize=(14, 6))

plt.subplot(1, 2, 1)
plt.plot(n_values, parallel_20, label='Parallel', marker='o')
plt.plot(n_values, sequential_20, label='Sequential', marker='o')
plt.plot(n_values, arrays_sort_20, label='Arrays.sort', marker='o')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('n')
plt.ylabel('Time (nanoseconds)')
plt.title('Performance for k=20')
plt.legend()

plt.subplot(1, 2, 2)
plt.plot(n_values, parallel_100, label='Parallel', marker='o')
plt.plot(n_values, sequential_100, label='Sequential', marker='o')
plt.plot(n_values, arrays_sort_100, label='Arrays Sort', marker='o')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('n')
plt.ylabel('Time (nanoseconds)')
plt.title('Performance for k=100')
plt.legend()

plt.tight_layout()
plt.savefig("graphs.png")
