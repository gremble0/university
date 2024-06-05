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
- a: w = (0.1, 0.1, 0.2), x1 = (1, 1), add bias term -1 -> x'1 = (-1, 1, 1)
w * x'1 = (0.1, 0.1, 0.2) * (-1, 1, 1) = 0.1 * -1 + 0.1 * 1 + 0.2 * 1 = -0.1 + 0.1 + 0.2 = 0.2
0.2 > 0 -> class 1.

- b: w_i = w_i - learning_rate * (predicted - target) * x_i
w = (0.1, 0.1, 0.2) - 0.2 * (1 - 0) * (-1, 1, 1)
w = (0.1, 0.1, 0.2) - 0.2 * (-1, 1, 1)
w = (0.1, 0.1, 0.2) - (-0.2, 0.2, 0.2) # NOTE: double negative!
w = (0.3, -0.1, 0)

## Task 5
- a: (bias, x1, x2) = (-1, 2, -1) -> take the weighted sum of these by dotting with the weights going into the hidden layer
h3 = (0.3, 0.6, 0.9) . (-1, 2, -1) = 0.3 * -1 + 0.6 * 2 + 0.9 * -1 = -0.3 + 1.2 - 0.9 = 0
THEN apply activation function, logistic function in this case, on this weighted sum:
exp(x) is equivalent to `e^x` where e is eulers number ~2.718.
a_3 = 1/(1 + exp(-h3)) = 1/2 = 0.5

- b: 
h1 = (0.1, 0.4, 0.7) . (-1, 2, -1) = 0.1 * -1 + 0.4 * 2 + 0.7 * -1 = -0.1 + 0.8 - 0.7 = 0
We have already calculated the activation function of 0 to be 0.5, so a_1 = 0.5

Starting to see a pattern, but lets calculate h2 just to be sure

h2 = (0.2, 0.5, 0.8) . (-1, 2, -1) = 0.2 * -1 + 0.5 * 2 + 0.8 * -1 = -0.2 + 1 - 0.8 = 0
a_2 = 0.5 (same as for h1 and h3)

Final layer = `(-1, 0.5, 0.5, 0.5)`. We then dot these with the weights to get the final output.
(-1, -1, 2, 3) . (-1, 0.5, 0.5, 0.5) = -1 * -1 + -1 * 0.5 + 2 * 0.5 + 3 * 0.5 = 1 - 0.5 + 1 + 1.5 = 3

- c:
we calculated a_3 in task a to be 0.5.
w_3 = w_3 - learning_rate * (predicted - target) * a_3 = 3 - 0.1 * (3 - 5) * 0.5 = 3 - 0.1 * -2 * 0.5 = 3 - 0.1 * -1 = 3 + 0.1 = 3.1

- d:
????????????????????????????????????? stupid

## Task 6
- a: Overfitting is when a machine learning model is trained too much on some training data leading to poor generalization if you tried to train it on new data it has not previously trained on. If we look at the picture we can see that the green line is clearly overfitted as it is bending in an unnatural shape to try to perfectly classify every single outlying datapoint - this model would probably not have the greatest performance on untrained data. The purple line is clearly underfitted as it is just a linear function which does not really fit to the dataset at all. The black line is a good middle ground between the two and is probably the model you would use on untrained datasets of a similar type.
- b: If a kNN classifier is struggling with overfitting I would suggest to increase the k value. With a smaller value for k the classifier can be overly sensitive to outlying datapoints, classifying them correctly for the test dataset instead of giving a more generalizable curve. This problem can be addressed by increasing the k value, as long as we don't make k too big causing underfitting (as for example the purple classifier in the figure).
- c: Inductive bias is some sort of preexisting assumption about what a trained model should look like. In terms of the perceptron and logistic regression classifiers this is the fact that they are linear meaning they would not be able to produce curves such as the black or green in the figure. Inductive bias is necessary for an algorithm to be able to generalize from a finite test set into a general population.

## Task 7
- a: `Q[center, right] = Q[center, right] + learning_rate * (reward + discount_rate * max(Q[center_right, :) - Q[center, right])` =
     `1 + 0.1 * (1 + 0.1 * 10 - 1)` = `1.1`

    Not required in task
    `Q[center_right, down] = Q[center_right, down] + learning_rate * (reward + discount_factor * max(Q[bottom_right, :]) - Q[center_right, down])` =
    `0 + 0.1 * (1 + 0.1 * 0 - 0)` = `0.1`
- b: SARSA calculates the action we actually took instead of the optimal `(max Q[s,a])`, thus the part of the formula that previously was `up` with a value of 10 is now `down` with a value of 0
    `Q[center, right] = Q[center, right] + learning_rate * (reward + discount_factor * Q[center', right'] - Q[center, right])` =
    `1 + 0.1 * (1 + 0.1 * 0 - 1)`
- c: The two different algorithms give different results here because they base their calculations off of two different possible paths. The on policy algorithm SARSA calculates based on the actual path we have taken - right, down, the off policy algorithm Q-learning calculates based on the theoretically best next step in each node leading to the path - right, up. Thus we get two different results.
- d: Greedy policy - 0%, will always choose the highest q-value - `up`.
     Epsilon-greedy - 10% chance to choose a random direction instead of the best one, then we have a 1/4 chance of choosing `down` if the 10% fires -> 0.1 * 0.25 = 0.025.
     `Softmax = (exp(Q_(s,t)(a)) / temperature) / sum(b, exp(Q_(s,t)(b)))` = `(exp(0) / 1) / (exp(0) + exp(0) + exp(0) + exp(10)) / 1` = `1 / 1 + 1 + 1 + 22026.4657948` = 0.0000045

## Task 8
- a: [Separable](./separable.png)
     [Unseperable](./unseparable.png)
- b: Kmeans works by first initializing k random centers, then every datapoint calculates the distance (euclidian, manhattan, etc) to each of the centers and identifies with the cluster belonging to the closest centroid. Then it recalculates the centers of each cluster by taking the mean of each dimension for all datapoints assigned to each cluster. It then repeats the distance calculation + centroid updating until the centers stop moving significantly or until some number of generations have been reached. Since the identities of the datapoints changes for each iteration (for some of them at least) the centers will therefore also shift around accordingly.
- c: Kmeans works on the first dataset because for every datapoint the classes of their closest neighbors are the same as their own - for all cat datapoints all their closest neighbors are cats, same for dogs. For the other dataset there is an outlying `dog` datapoint who's distance to the center of the cat cluster is shorter than some of the other dogs, thus it will always be classified as a cat.

## Task 9
[L systems](./lsystems.png)

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

## Perceptron
Update weights formula:
w_i = w_i - learning_rate * (predicted - target) * x_i

## Formulas
- Dot product: `a . b = a_1 * b_1 + a_2 * b_2 + ... + a_n * b_n`
- (np.)exp: `exp(x) = e^x`

Perceptron
- Perceptron update weights: `w_i = w_i - learning_rate * (predicted - target) * x_i`
- logistic activation: `1/(1 + exp(-x))`
- Backpropagation activation functions - one for hidden layer(s) `g` and one for output layer `f`: ``

Reinforcement learning:
- SARSA: `Q[s,a] = (1 - learning_rate) * Q[s,a] + learning_rate * (reward + discount_rate * Q[s',a'])`
- Q-learning: `Q[s,a] = (1 - learning_rate) * Q[s,a] + learning_rate * (reward + discount_rate * max(Q[s',:]))`
    - Update `Q[s,a]` in Q-learning: `Q[s,a] = Q[s,a] + learning_rate * (reward_+1 + discount_factor * max(Q[s_+1,a]) - Q[s,a])`

## Exam 2023
### 1
- Supervised learning: can be used to train a model that classifies images.
- Unsupervised learning: can be used to generate recommended content on social media.
- Reinforcement learning: can be used to teach a model how to path through a maze.

### 2
- a: 
```python
N_ITERATIONS = 200

def tsp_hill_climbing(cities):
    # copy the parameter
    best = cities[:]
    # keep track of the best distance to not have to recalculate it inside loop every time
    best_distance = EVALUATE_DISTANCE(best)

    for i in range(N_ITERATIONS):
        neighbor = GENERATE_RANDOM_NEIGHBOR(best)
        neighbor_distance = EVALUATE_DISTANCE(neighbor)
        # update best if we have found an improvement
        if neighbor_distance < best_distance:
            best = neighbor
            best_distance = neighbor_distance
        
    return best

print(tsp_hill_climbing(GENERATE_RANDOM_SOLUTION()))
```
- b:
```python
N_ITERATIONS = 200

def tsp_simulated_annealing(cities):
    best = cities[:]
    best_distance = EVALUATE_DISTANCE(best)

    # temparature starts high
    temperature = N_ITERATIONS

    for i in range(N_ITERATIONS):
        neighbor = GENERATE_RANDOM_NEIGHBOR(best)
        neighbor_distance = EVALUATE_DISTANCE(neighbor)

        if neighbor_distance < best_distance:
            # if neighbor is better explore it
            best = neighbor
            best_distance = neighbor_distance
        elif SHOULD_I_EXPLORE(best, neighbor, temperature):
            # if neighbor is worse, maybe explore it anyways
            best = neighbor
            best_distance = neighbor_distance

        temperature -= 1

    return best

print(tsp_simulated_annealing(GENERATE_RANDOM_SOLUTION()))
```

### 3
The representation lets us encode some pre existing knowledge about the shape of the problem into our solution allowing us to more easily model the problem. For example for the TSP we could represent the problem as a permutation problem since this would by definition not allow undesired solutions such as visiting the same cities more than once/not visiting some cities, etc. If we were to represent the problem in some other way - for example as an array of integers we would allow these undesried solutions which we would later have to filter out.
### 4
- a: Exploration should start high since we don't yet know what solutions are good or bad, and decrease as the process converges around a few possible good solutions. A way to do this in RL is to start with a high epsilon value in an epsilon greedy algorithm and decrease it as the search progresses.
- b: Exploration should start high since we don't yet know what solutions are good or bad, and decrease as the process converges around a few possible good solutions. A way to do this in EA is to start with a high mutation rate and decrease it as the search progresses.
### 5
- a: The training data is used to actively update the weights in the model as it's learning by comparing the models predicted values with some golden standard of target values. The test data is applied after the training is done (its important to not have it spill into or use it while training) to get a generalized accuracy for how the model performs on untrained data of the same shape as the training data, thus giving us a sense of whether the model is underfitted, overfitted, good, bad, etc.
- b: We keep two (or three with a validation set) datasets to separate between what data we are using to actively update the model (train) and what data we are using to benchmark the performance of (test) the model. These need to be separated in order to ensure that the model is not cheating by integrating the data from the test set before its supposed to. If we did not do this it would be sort of like getting the answers to an exam before you have started it.
- c: The validation set is used while training, not to update the weights, but to make sure the model is making steady generalizable progress and not overfitting to the training set. If we notice a decline in accuracy on the validation set we can stop the training early and possible roll back to a previous generation to get the best generalizable model. Different models and parameters may perform differently on this validation set which may not be representative of the model's true general accuracy, which is why we need another fully independent test set to verify this accuracy.
- d: Cross-validation works by splitting the training set into n equally sized bins. We then run n experiments on these where the k'th bin is used as the test set and the rest of the bins as the training set. This gives us a larger test dataset by using the whole training dataset in different ways, which is especially benefitial when we don't have that much data.
### 6
- a: A perceptron is an algorithm for binary classification. It takes some vector of input and applies weights to each input value, often combined with a bias term of 1 or -1. If the input consists of m features the perceptron has m weights + a threshold weight w0. It then takes the weighted sum of the inputs before it runs that sum through some activation function (for example logistc, relu, etc), which produces a predicted output based on the input. It can then compare the output with some gold standard to check if it predicted right. If the weighted sum is greater than the threshold w0, the perceptron classifies the input as positive, otherwise negative class. If it predicted wrongly it can then update the weights based on this formula: `weight_i = weight_i - learning_rate * (predicted - target) * feature_i`.
- b:
    1. It could only produce linear classifiers. Binary classification.
    2. A new technique where you could add several hidden layers to the perceptron was invented - gradient descent, backpropagation.
    3. Faster hardware - GPUs, more data - internet, better algorithms.
### 7
- a: `logistic(x) = 1/(1 + e^(-x))`
- b: `h1(x1, x2) = logistic(bias * v01 + x1 * v11 + x2 * v21)` = `1/(1 + e^(-(-1 * v01 + x1 * v11 + x2 * v21))`
- c: `out(x1, x2) = logistic(bias * w0 + logistic(h1(x1, x2)) * w1 + logistic(h2(x1, x2)) * w2)` = `1/(1 + e^(-(-1 * w0 + a1 * w1 + a2 * w2)))`
- d: `update_w2(a2) = w2 - learning_rate * (predicted - target) * a2` = `w2 - 0.1 * (predicted - 1) * a2`
- e: ~`update_v12(a1) = v12 - learning_rate * (predicted - target) * a1` = `v12 - 0.1 * (predicted - 1) * a1`~

- Realized I was doing 4050 exam so now we jump to 3050 exam 2023 lol

### 9
- a: An agent with a greedy policy will always take the action with the largest reward. We can follow the agent for 5 steps to see if it reaches the large reward. 
    1. `max(Q[center, :])` = 10 -> move right
    2. `max(Q[center_right, :]` = 10 -> move up
    3. `max(Q[top_right, :]` = 10 -> move down
    4. `max(Q[center_right, :]` = 10 -> move up
    5. `max(Q[top_right, :]` = 10 -> move down
    - We can see here that we have entered an infinite loop of going back and forward between center_right and top_right. This is because when in these states the highest rewards are simply to go back and forth between them, meaning they will never reach the big reward `R`.
- b: Instead of a greedy policy we can use an epsilon greedy policy, which introduces a variable `epsilon` which is a number between 0 and 1 that acts as the probability to choose a random action instead of the action with the biggest reward. If we are lucky with this policy the following could happen:
    1. We choose some epsilon value, for example 0.1
    2. We choose our first action - epsilon does not trigger (90% chance) and we pick the greedy option -> move right, we are now in the state center_right.
    3. We choose our second action - epsilon triggers (10% chance) meaning we choose a random action instead of moving up which is the greedy action.
        - We randomly choose moving down (25% chance)
    - This sequence is rather unlikely (0.9 * 0.1 * 0.25 = 0.0225 = 2.25%) but it is a possible way of reaching the big reward. There are also many other combinations which are all possible with an epsilon greedy policy.
### 10
- a:
    1. Choose `k` random datapoints.
    2. Classify each datapoint based on proximity to each randomly chosen datapoint.
    3. Calculate the center of each of every cluster.
    4. Classify each datapoint based on proximity to each of the new centers.
    5. Repeat step 3-4 until centers stop moving significantly or some amount of iterations have passed.
- b: An autoencoder is a model that can reproduce an output similar to the input. Some use cases are - removing noise from audio, generating data and compression + decompression.
### 11
Biases in machine learning algorithm is when a model reinforces some stereotype based on the statistics in the data it is trained on. For example if a model is trained to predict whether a defendant in a court case is guilty or not based on their personal traits they will predict true or false based on whether the defendant has personality traits that are common for guilty defendants. This means that it will likely categorize a young black male as guilty and a pregnant white mother as innocent. The result of this is that in terms of percentages the model may have a good accuracy, but the absolute number of false positives may be a lot higher for some types of people. Whether this is fair is up for debate and is why biases in machine learning can be problematic.
- ML model may pick up on human biases present in the test data and associate certain attributes with being good or bad, instead of being independent.
