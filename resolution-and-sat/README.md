# Resolution and SAT-Solver

## Overview
This project implements resolution in propositional and first-order logic (FOL) using SWI-Prolog and a Java graphical interface. The main objective is to demonstrate logical entailment through the resolution method.

## Knowledge Base (FOL)
The knowledge base consists of natural language sentences converted to FOL:

1. Anyone who exercises and eats well is fit.  
   `∀x. (Exercises(x) ∧ EatsWell(x)) → Fit(x)`

2. Anyone who is vegetarian eats well.  
   `∀x. Vegetarian(x) → EatsWell(x)`

3. Anyone who is fit can hike.  
   `∀x. Fit(x) → CanHike(x)`

4. Anyone who goes to the gym exercises or is a trainer.  
   `∀x. GoesToGym(x) → (Exercises(x) ∨ Trainer(x))`

5. No trainer has an office job.  
   `∀x. Trainer(x) → ¬HasOfficeJob(x)`

### Question to be Entailed

"Anyone who has an office job, goes to the gym, and is vegetarian can hike."

In FOL:
```
∀x. (HasOfficeJob(x) ∧ GoesToGym(x) ∧ Vegetarian(x)) → CanHike(x)
```

## Methods Implemented
- Conversion of statements to **Conjunctive Normal Form (CNF)**
- **Resolution algorithm** with variable unification and redundancy checking (tautologies, subsumed clauses, pure literals)
- Demonstration of logical entailment by resolution (manual & automated)

## How to Run
- Ensure SWI-Prolog and Java are installed.
- Open and run the provided Java GUI.
- Select and load input files containing clauses from GUI.
- Execute resolution algorithm to verify logical entailment.

## Examples
The implementation was tested on several propositional logic examples (included in the repository), demonstrating correct identification of satisfiable and unsatisfiable cases.

| Example | Result          |
|---------|-----------------|
| i       | UNSATISFIABLE   |
| ii      | UNSATISFIABLE   |
| iii     | SATISFIABLE     |
| iv      | SATISFIABLE     |

## GUI Preview
  ![Resolution & SAT Solver Interface](../images/resolution.png)


