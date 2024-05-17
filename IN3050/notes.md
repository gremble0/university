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
- c: First we must recalculate the fitness values of each individual based on fitness sharing. We can do this by checking for each of the individuals whether they are under the niche size of 3.
    - d(g1, g2) = abs(0 - 0) + abs(1 - 0) + abs(1 - 0) + abs(1 - 0) + abs(1 - 1) = 3
    - d(g1, g3) = abs(0 - 1) + abs(1 - 0) + abs(1 - 0) + abs(1 - 1) + abs(1 - 1) = 3
    - d(g2, g3) = abs(0 - 1) + abs(0 - 0) + abs(0 - 0) + abs(0 - 1) + abs(1 - 1) = 2
    We can use these values to see how much each individual should share with eachother.

    Then we can calculate the sharing components between the individuals
    - sh(g1, g2) = 1 - 3/3 = 0
    - sh(g1, g3) = 1 - 3/3 = 0
    - sh(g2, g3) = 1 - 2/3 = 1/3

    Then we can calculate the updated fitness values for each individual
    - f'(g1) = f(g1)/sum(sh(d(g1, g1))..sh(d(g1, g3))) // Range notation is a bit unnecessary here but its just to show that the algorithm uses a sum from the first individual up to the u(mu)'th individual summing them all up
        - f'(g1) = 1/(1 + 0 + 0) = 1
    - f'(g2) = 2/(1 + 0 + 1/3) = 2/(4/3) = 1.5
    - f'(g3) = 3/(1 + 0 + 1/3) = 3/(4/3) = 2.25

## Task 3
- a: In machine learning categorical features are features that can be categorized into a predefined set of possible values. If we look at the table we can say that `spam`, `winner occurs`, `format` and `number` are categorical features - these are sort of like `enum`s in some programming languages. Numerical features are features that are described by some number within an undefined range (if the range is defined for example integers from 1-100 it can be argued that the feature is categorical since we already know all the possible values beforehand). Numerical features from the table are `chars`, `line breaks` and `dollar occurs`.
- b: Decision trees - categorical
     kNN - numerical
     Perceptron - numerical
     Logistic regression - numerical
     MLP - numerical
- c: To convert categorical features to numerical features you can simply assign a numeric value to each category for a feature - this is exactly what `enum`s do in some programming languages. For example we could say that `no` in `winner occurs` has a value of 0 and `yes` has a value of 1. For numbers: `number.none = 0`, `number.small = 1`. `number.big = 2`. Both `format` and `spam` are also binary so they can also be represented as 0 or 1 like `winner occurs`.
- d: I would first convert all the categorical features to numerical as described in task c - this is necessary for the classifier to work on the dataset. When working with numerical features its also generally a good idea to scale the dataset before using it. In the spam email we can see that there is a large difference in absolute values between some of the numerical features, e.g. `chars` and `line breaks` are orders of magnitude larger than the converted categorical features and `dollar occurs`. This could impact the classifier but would be negated by scaling the data beforehand.
- e: In this case we would have to convert the numerical features to categorical ones. To do this we could decide for each feature - 1: decide a sensible amount of categories for that feature, 2: decide the numerical ranges for each category. We could do this by doing some educated guesses or programatically finding some sensible values. Some educated guesses for the spam email example could be: `chars 0 = 0`, `chars 1..1000 = 1`, `chars 1001..1000 = 2`, `chars >10000 = 3`. `line breaks 0 = 0`, `line breaks 1..10 = 1`, `line breaks 11..100 = 2`, `line breaks 101..1000 = 3`, `line breaks >1001 = 4`. And something similar for `dollar occurs`. We would then apply these ranges to the dataset before handing it to the classifier - this also has the benefit of acting as a sort of scaler to the dataset.

## Task 4
h(x1) = -w0 + w1*x1 = -(-1) + 0.1* ???

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

## Diversity
### Explicit approaches
- Fitness sharing: individuals share their fitness with similar solutions. `f'(i) = f(i)/sum(sh(d(i,i_1))..sh(d(i,i_u)))`. Amount of individuals on each peak is proportional to the height of that peak (more individuals on higher peaks, fewer on smaller).
- Crowding: individuals only compete against closest neighbours. Amount of individuals is equal on each peak.
