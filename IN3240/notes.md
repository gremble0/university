## Exam 2022
### 1
- Use case
- EP/BVA
- State transition
- Decision table
### 2
- Implementation phase
### 3
- Type
- Level
- Type
- Level
- Level
- Level
- Type
- Type
### 4
- Analytical
- Model
- Consultative
- Methodical
### 5
- Acceptance X - System
### 6
- System X - integration
- Integration X - system
- Acceptance
- Unit
### 7
- Beta
### 8
- Walkthrough lead by author, inspection trained mod
### 9
- Question assumptions
### 10
- Num undetected errors
### 11
- Tester
- Test leader
- Tester
- Test leader
### 12
- Acceptance
### 13
- 50, 51, 55, 56, 60, 61
### 14
- BVA
### 15
- a:
    - 1:
        - A1: travel freely
        - A2: quarantine hotel
        - A3: home quarantine
    - 2:
        - Adult?
        - Vaccine pass?
        - Negative test?
- b:
+----------------+---+---+---+---+---+---+---+---+
| Rule           | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
+----------------+---+---+---+---+---+---+---+---+
| Adult?         | X | X | X | X |   |   |   |   |
| Vaccine pass?  | X | X |   |   | X | X |   |   |
| Negative test? | X |   | X |   | X |   | X |   |
+----------------+---+---+---+---+---+---+---+---+
| Action         | 1 | 2 | 2 | 2 | 1 | 3 | 1 | 3 |
+----------------+---+---+---+---+---+---+---+---+
* 1 and 2 in the action row correspond to A1 and A2 described in part 15a.

- c:
First we can combine rules 2-4 as any combination of vaccine pass? and negative test? other than both being true will result in A2.
+----------------+---+-----+---+---+---+---+
| Rule           | 1 | 2-4 | 5 | 6 | 7 | 8 |
+----------------+---+-----+---+---+---+---+
| Adult?         | X |  X  |   |   |   |   |
| Vaccine pass?  | X |  -  | X | X |   |   |
| Negative test? | X |  -  | X |   | X |   |
+----------------+---+-----+---+---+---+---+
| Action         | 1 |  2  | 1 | 3 | 1 | 3 |
+----------------+---+-----+---+---+---+---+
`-`: does not matter

Then we can combine rule 5 and 7 since if the person is a child with a negative test it does not matter whether they have a vaccine pass
+----------------+---+-----+-----+---+---+
| Rule           | 1 | 2-4 | 5&7 | 6 | 8 |
+----------------+---+-----+-----+---+---+
| Adult?         | X |  X  |     |   |   |
| Vaccine pass?  | X |  -  |  -  | X |   |
| Negative test? | X |  -  |  X  |   |   |
+----------------+---+-----+-----+---+---+
| Action         | 1 |  2  |  1  | 3 | 3 |
+----------------+---+-----+-----+---+---+

Finally we can combine rule 6 and 8 since if the person is a child with no negative test it also does not matter if they have a vaccine pass
+----------------+---+-----+-----+-----+
| Rule           | 1 | 2-4 | 5&7 | 6&8 |
+----------------+---+-----+-----+-----+
| Adult?         | X |  X  |     |     |
| Vaccine pass?  | X |  -  |  -  |  -  |
| Negative test? | X |  -  |  X  |     |
+----------------+---+-----+-----+-----+
| Action         | 1 |  2  |  1  |  3  |
+----------------+---+-----+-----+-----+

- d:
1. This represents rule 1
2. This represents rule 2
3. This represents rule 7
4. This represents rule 6
These 4 rules covers combined cover each of the logically equivalent combinations meaning we do not need more test cases to cover any more test cases.

### 16
- a: The shortest path is not the same as the one that covers as many states as possible so combining both of these into one is impossible. If we instead want to find the longest DAG (directional acyclical graph - meaning in this context the longest path that never backtracks/returns to a previously visited state) through the state transition diagram we can also do this. This path will also be different from both the shortest path and the path that covers the most states. I believe the question is asking for the longest DAG, but I will give both the shortest path and the longest DAG path.
    - Shortest:    S1 -> S3 -> S4 -> S5 -> S9 -> S10 -> S12, coverage 7/12
    - longest DAG: S1 -> S3 -> S4 -> S5 -> S7 -> S8 -> S9 -> S10 -> S12, coverage 9/12
- b: For 100% state coverage we need use cases that cover all possible states. There are two pairs of mutually exclusive states we can not backtrack from - (S5, S6) and (S11, S12). Since both S11 and S12 are reachable from all paths following both S5 and S6 we would only need 2 use cases to cover all states. One or two of these use cases would also need to cover S2 as this is not in the main control flow. An example of 2 use cases that give 100% state coverage is:
    1. S1 -> S2 -> S3 -> S4 -> S5 -> S7 -> S8 -> S9 -> S10 -> S11
    2. S1 -> S3 -> S4 -> S6 -> S9 -> S10 -> S12
    - which gives us 2 use cases
- c: If we assume the same definition i offered in 16a - only with not backtracking over the same transitions instead of states - we would get the following path:
    - S1 -> S2 -> S1 -> S3 -> S4 -> S5 -> S7 -> S8 -> S9 -> S10 -> S12, coverage 10/19
    - We could backtrack from S4 -> S1 in this path, but this would involve re transitioning over already visited transitions from S1 -> S3 and S3 -> S4. If we still wanted to do this the path would become:
    - S1 -> S2 -> S1 -> S3 -> S4 -> S1 -> S3 -> S4 -> S5 -> S7 -> S8 -> S9 -> S10 -> S12, coverage 11/19
- d: There are very few steps where we can backtrack in this diagram, which means we need quite a few use cases to cover all possible transitions. Here is an example of some use cases that combined gives us 100% transition coverage:
    - Covers S1 -> S2, S2 -> S1
        - S1 -> S2 -> S3 -> S4 -> S1 -> S3 -> S4 -> S5 -> S7 -> S9 -> S10 -> S11
    - Covers transitions from S5
        - S1 -> S3 -> S3 -> S4 -> S5 -> S8 -> S9 -> S10 -> S12
        - S1 -> S3 -> S4 -> S5 -> S9 -> S10 -> S12
    - Covers transitions from S6
        - S1 -> S3 -> S4 -> S6 -> S7 -> S9 -> S10 -> S12
        - S1 -> S3 -> S4 -> S6 -> S8 -> S9 -> S10 -> S12
        - S1 -> S3 -> S4 -> S6 -> S9 -> S10 -> S12
    - These 6 paths will combined give us 100% state transition coverage
### 17
- Dynamic testing is related to test techniques that require the code to be executed, static testing is the opposite. Static testing is therefore easier to employ earlier in the development process since it can be applied to smaller portions of the code without needing it to function as a whole - for example by compiling some file(s) and reviewing any possible warnings/errors or using some third party service like sonarcloud. After the code passes the static tests you can move onto dynamic testing to make sure the program as a whole works as expected. These test techniques are complementary meaning they will catch different types of bugs, so you should employ both of them in your development process.
### 18
[V-model](scuffed-v.png)
### 19
- a: A test script is a sequence of operations that describe how to run a test. These instructions are written in a language the executor can understand, normally a programming language for automated tests.
- b: A linear script only considers one specific use case, whereas a generic script can accept some parameters to generalize the script to a wider variety of use cases, letting the same script be reused for different use cases. A linear script may be easier to write and is some times sufficient, but a generic script is good practice as it can simplify future test efforts.
- c: A data driven script separates the test data from the test script, taking it in as a parameter. A keyword driven script also factorizes out some keywords or actions that the program can execute on the test data, allowing for further generelization by now only requiring some keywords and data to execute a test rather than having to rewrite similar boilerplate.
### 20
- a: Human Computer Interaction
- b: HCI is defined as the degree to which a product can be used by specified users to achieve specified goals with effectiveness, efficiency and satisfaction in the specified context. Its purpose is to improve the product from the users perspective.
- c:
    - Interface standards
    - Usability
    - Interface dynamics
    - Aesthetics
### 21
- Entry and exit criteria describe some criteria that need to be met before we can start a test iteration and what conditions need to be met before we can say that a test iteration has succeeded and we can move on.
- Planning phase
- Test leader
- Testers keep track but test leader decides

## Exam 2021
### 1
- Regression
### 2
- Decision table
- EP/BVA
- State transition
- Use case
### 3
- Impact
- Likelihood
### 20
- Static doesnt require code to run - can be used earlier - planning
- Dynamic requries code to run - 4 test stages (component, integration, system, acceptance) + during maintenance
- Complementary, catches different errors - use both. Broadly - static finds the causes and concrete places that are causing errors, dynamic finds if there are any errors, not where they are.
### 22
- Automation is easier earlier on, low effort and high reward in component - integration, harder in system, very hard in acceptance.
### 23
- Exploratory testing is testing where the tester is largely guessing where errors could be, using their experience and common patterns to find errors.
- Experience based
- Good when there is little documentation/design for the code and in legacy code - if few know a lot in this area of the system. Often as a supplement to more methodical and formal testing. Also when there is time pressure
### 24
- Personas describe a fictive person based on real statistics. For example if you want your program to be accessible you can develop a persona of an elderly woman bad with computers with bad vision, bad hearing, etc. Keep the personas traits in mind when developing, bad vision - have good contrast and large enough text, etc. Bad hearing - have subtitles, volume control, etc. Bad with computers - be intuitive and user friendly.
