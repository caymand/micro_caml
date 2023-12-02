
Add - grad_19
    - grad_16
    - grad_14


# Tape v1
- z_5 = Val 1
- z_7 = Const 2
- z_5 = Val 5 
    + we add the same node twice to the tape, but with new parent
    + Maybe makes sense since it appears in two expressions?
    + Need to add its gradient with the other gradient
    + Should get renamed instead
- z_4 = Mul z_7 z_5
- z_6 = Add z_4 z_5
Reversing the tape gives this order - a topological sorting.
This means that the tape is a reverse topological sorting of the nodes.
Thereby, the tape can traversed and the gradients accumulated.

# Tape v2
- add3 = Ident z_7
- z_7 = Add z_5 x_0
- mul_2 = Ident z_5
- z_5 = Mul (Const 2) x_0

# Backward
f(x) = 2 + x + x
f'(x) = 2
Add (
    Add(zero, Mul(0, 2)),
    Mul(1, 1)
) = 1