import matplotlib.pyplot as plt

n_values = [100, 1000, 10000, 100000, 1000000, 10000000, 100000000]
convexHullSeq_times = [0.09958, 1.120446, 0.924767, 7.816112, 68.360861, 700.041161, 8021.676499]
convexHullPara_times = [0.526307, 0.601108, 1.611444, 10.524821, 61.973554, 1066.546036, 7177.863811]

# Plotting the data with a logarithmic scale for both axes
plt.figure(figsize=(10, 5))

plt.plot(n_values, convexHullSeq_times, marker='o', label='ConvexHullSeq')
plt.plot(n_values, convexHullPara_times, marker='s', label='ConvexHullPara')

# Logarithmic scale for both x-axis and y-axis since the values vary exponentially
plt.xscale('log')
plt.yscale('log')

# Adding titles and labels
plt.title('Run time comparison between ConvexHullSeq and ConvexHullPara (12 threads)')
plt.xlabel('Input size n (log scale)')
plt.ylabel('Run time (ms) (log scale)')
plt.legend()

# Show grid
plt.grid(True, which="both", ls="--")

# Show the plot
plt.savefig("runtimes12.png")



# New data points
n_values_new = [100, 1000, 10000, 100000, 1000000, 10000000, 100000000]
convexHullSeq_times_new = [0.10004, 1.303025, 0.918237, 8.04628, 66.799768, 681.985109, 8031.169317]
convexHullPara_times_new = [0.291849, 0.480538, 0.549358, 4.409414, 47.877187, 356.29518, 4170.588961]

# Plotting the new data with a logarithmic scale for both axes
plt.figure(figsize=(10, 5))

plt.plot(n_values_new, convexHullSeq_times_new, marker='o', label='ConvexHullSeq')
plt.plot(n_values_new, convexHullPara_times_new, marker='s', label='ConvexHullPara')

# Logarithmic scale for both x-axis and y-axis since the values vary exponentially
plt.xscale('log')
plt.yscale('log')

# Adding titles and labels
plt.title('Run time comparison between ConvexHullSeq and ConvexHullPara (4 threads)')
plt.xlabel('Input size n (log scale)')
plt.ylabel('Run time (ms) (log scale)')
plt.legend()

# Show grid
plt.grid(True, which="both", ls="--")

# Show the plot
plt.savefig("runtimes4.png")
