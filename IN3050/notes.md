## Exam 2021
### Task 1
- Through its stochastic nature the EA can make large jumps in the fitness landscape leading to more exploration. Parallel hillclimbing on the other hand will purely exploit each solution until it finds its local optimum.
    - On this note the hillclimbing still has some degree of randomness depending on what starting points you select.
- If the EA discovers a fitter type of solution it can shift its focus to that type of solution through the selection process, leading to a more dynamic and explorative algorithm. Parallel hillclimbing on the other hand will never discover drastically different solutions, as it can only make small incremental changes to its current best solution, leading to a more static and exploitative algorithm.

### Task 2
- a: We can note the fitness proportional probability of an individual being selected for the next generation as this function: `p(i_n) = fit(i_n)/sum(fit(i_1)..fit(i_k))`, where fit is the fitness function, i is an individual and k is the number of individuals. Effectively this means that the probability of an individual being selected is equal to its fitness divided by the sum of all fitnesses in the population. This gives us the following probablities for the individuals in the table: `p(i_1) = f(i_1)/sum(fit(i_1)..fit(i_k))` = `p(i_1) = 1/6`, `p(i_2) = 2/6`, `p(i_3) = 3/6`.
- b: In tournament selection the probability of selecting any individual is the same. Since the tournament size (k) is 2 this gives any individual a 2/3 chance to participate in the tournament, and since its deterministic the fittest individual will always be selected from the tournament. This gives us the following:
    - Individual 1: is not fitter than any other individual, and `k` is larger than 1 with no replacement (cant compete against itself) so its impossible for it to be selected - 0%.
    - Individual 2: is only fitter than individual 1. For this to be selected i2 needs to be in the tournament - 2/3. It then also needs to compete against i1 - 1/2. 1/2 * 2/3 = 2/6 = 1/3
    - Individual 3: is fitter than everyone so only needs to be selected for tournament to be selected - 2/3.

    Alternative reasoning:
    - There are 3 possible tournaments - i1vi2, i1vi3, i2vi3. i1 is less fit than all others meaning it can never win a tournament - selection chance 0, i2 is only stronger than i1 meaning only i1vi2 results in it being selected - 1/3. i3 wins all tournaments it competes in which could be i1vi3 or i2vi3 - 2/3.
- c: 

## Genetic algorithm vs hill climbing
The genetic algorithm is not stochastic meaning its results has some amount of randomness involved - this comes from the mutation and crossover which we could also change to modify the randomness. Thus the genetic algorithm has a higher degree of exploration while the hillclimbing algorithm is pure exploitation. Therefore the hillclimbing algorithm(s) will always end up at the same local optimum(s) (which may be the global optimum), while the genetic algorithm could theoretically end up on any local optimum, which may be better if you for example run it a couple times to make sure youve found the global optimum.


## Selection
### Parent selection
- Fitness proportional: probability of selecting an individual is the fitness of that individual divided by the sum of all individuals fitness. Problem: depends on absolute values of individuals, e.g. 495, 500, 505 -> 0.327, 0.33, 0.337, while 0, 5, 10 -> 0, 1/3, 2/3. Solution: rank based
- Rank based: probability of selecting an individual is based on the rank of an individuals fitness in the population of all fitnesses. e.g. 495, 500, 505 -> 0, 1, 2 -> 0/3, 1/3, 2/3 and 0, 5, 10 -> 0, 1, 2 -> 0/3, 1/3, 2/3. Problem: probability still depends on global population statistics. This could be a problem when running in parallel or if there is no way to give a fitness without comparing it to another, e.g. chess. Solution: tournament selection.
- Tournament selection: compare a few(k) individuals to eachother and select the best - repeat. Probability depends on rank of individual, `k` - large tournament more selection pressure - more unlikely to be lucky, whether fittest contestant always wins (deterministic) or if we give tournament competitors probabilities of being selected based on outcome.

### Survivor selection
Filter old and new individuals to select the next generation. u(mu) old generation, lambda new generation.
- Elitism.
- (u,lambda)-selection - new generation is purely based on lambda (best solutions can be lost) - escape local optima - exploration
- (u+lambda)-selection - new generation is based on combination of u and lambda (best solutions will be kept) - exploitation - elitism
