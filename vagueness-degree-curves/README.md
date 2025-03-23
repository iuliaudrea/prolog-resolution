# Vagueness and Degree Curves

## Overview
This project illustrates handling vagueness using fuzzy logic and degree curves, implemented with SWI-Prolog and a Java graphical interface. It estimates a hotel's review score based on two vague input ratings: **cleanliness** and **location**.

## Predicates and Rules

The review estimation uses three fuzzy predicates for cleanliness (**poor**, **decent**, **excellent**) and two predicates for location (**bad**, **good**). Each predicate has associated degree curves defined on a scale from 0 to 10.

### Rules:
- If cleanliness is **poor** or location is **bad**, then the review is **low**.
- If cleanliness is **decent**, then the review is **moderate**.
- If cleanliness is **excellent** or location is **good**, then the review is **high**.

The final **review score** ranges from 1 (lowest) to 5 (highest) and is computed by aggregating these fuzzy predicates.

## Degree Curves
Each predicate is represented by a mathematical degree function indicating its truth value for given inputs:
- Cleanliness: `poor(x)`, `decent(x)`, `excellent(x)`
- Location: `bad(x)`, `good(x)`
- Review: `low(x)`, `moderate(x)`, `high(x)`

## Computation Steps
1. Input ratings for cleanliness and location (0–10 scale).
2. Calculate predicate degrees.
3. Combine degrees according to rules.
4. Aggregate into a single review degree curve.
5. Perform **defuzzification** to obtain a final numeric score.

## GUI Preview
![Vagueness Interface](../images/vagueness.png)


## Example
- **Cleanliness:** 7.5
- **Location:** 5.5

Resulting discrete degree points for review:
```
(1, 0.25), (2, 0.25), (3, 0.5), (4, 0.25), (5, 0.25)
```

Final estimated review score: **3** (moderate)

## How to Run
- Ensure SWI-Prolog and Java are installed.
- Launch the Java GUI provided in the repository.
- Input cleanliness and location scores and compute the review.

