import matplotlib.pyplot as plt

n_values            = [100,      200,      500,       1000      ]
SEQ_NOT_TRANSPOSED  = [6668590,  6697239,  140838783, 1108943701]
SEQ_A_TRANSPOSED    = [6345113,  17084016, 256415092, 2172488206]
SEQ_B_TRANSPOSED    = [6139581,  6160700,  91920329,  745106124 ]
PARA_NOT_TRANSPOSED = [10762413, 1939121,  25713426,  189322930 ]
PARA_A_TRANSPOSED   = [9194899,  3965649,  50518649,  407426890 ]
PARA_B_TRANSPOSED   = [2531537,  1522859,  10640098,  71252642  ]

SEQ_NOT_TRANSPOSED  = [ x / 1000000 for x in SEQ_NOT_TRANSPOSED ]
SEQ_A_TRANSPOSED    = [ x / 1000000 for x in SEQ_A_TRANSPOSED   ]
SEQ_B_TRANSPOSED    = [ x / 1000000 for x in SEQ_B_TRANSPOSED   ]
PARA_NOT_TRANSPOSED = [ x / 1000000 for x in PARA_NOT_TRANSPOSED]
PARA_A_TRANSPOSED   = [ x / 1000000 for x in PARA_A_TRANSPOSED  ]
PARA_B_TRANSPOSED   = [ x / 1000000 for x in PARA_B_TRANSPOSED  ]


plt.figure(figsize=(10, 8))

plt.plot(n_values, SEQ_NOT_TRANSPOSED, marker="o", label="Sequential Not Transposed")
plt.plot(n_values, SEQ_A_TRANSPOSED, marker="o", label="Sequential A Transposed")
plt.plot(n_values, SEQ_B_TRANSPOSED, marker="o", label="Sequential B Transposed")
plt.plot(n_values, PARA_NOT_TRANSPOSED, marker="o", label="Parallel Not Transposed")
plt.plot(n_values, PARA_A_TRANSPOSED, marker="o", label="Parallel A Transposed")
plt.plot(n_values, PARA_B_TRANSPOSED, marker="o", label="Parallel B Transposed")

plt.xlabel("n")
plt.ylabel("Time (milliseconds)")
plt.ticklabel_format(style="plain", axis="y")
plt.title("Time Complexity for Various Matrix Operations")
plt.legend()
plt.grid(True)

plt.savefig("graph.png")
