We have a few basic abstractions:


Memory (maybe rename?)
------------
The Memory implementation stores experiences into some structure M
```
def addExperience: ((S,A,Reward,Next[S,A])) => M => M
```

`EmptyMemory` can be cleared by the algorithm (e.g. instantiating a 'blank' M).
Not supporting `EmptyMemory` means that some more complex initialization is needed,
and will be handled externally.

Policy / actor
------
A Policy uses knowledge stored in M to determine how to choose actions
```scala
type GenPolicy[S,A,M,P[_]] = (M,S) => P[A]

type GenPolicyA[S,A,M,P[_]] = (M,S) => Actions[A] => P[A]

type GenPolicyAA[S,A,AA,M,P[_]] = (M,S) => Actions[A] => P[AA]

// a stochastic policy which chooses a greedy action uniformly
def greedyUniformPolicy[S,A,M,P[_]](implicit q: QLookup[M,S,A], P:PDF[P]): GenPolicyA[S,A,M,P] =
  (m,s) => actions => P.uniform(argmaxesBy(actions)(q.qlookup(m)(s)))

def epsilonGreedy[S,A,M,P[_]](ε: Probability)(implicit q: QLookup[M,S,A], P:PDF[P]): GenPolicyA[S,A,M,P] =
  mixPolicies(P.weightedUnsafe(Map(uniformPolicy -> ε, greedyUniformPolicy -> (1-ε))))

def mixPolicies[S,A,M,P[_]](policies: P[GenPolicyA[S,A,M,P]])(implicit P:Bind[P]): GenPolicyA[S,A,M,P] =
  (m,s) => actions => P.bind(policies)(π => π(m,s)(actions))
```
`P[_]` is the type of probability distribution (or other abstraction) over actions that the policy produces.
Different policy constructors may have various requirements on `P`.  Using `Id` would produce deterministic policies.



Agent
-----
An Agent simply pairs a Policy to a Learner:
```scala
case class Agent[S,A,M,P[_]](π: GenPolicy[S,A,M,P], m: M)(implicit M:Memory3[M,S,A])
```

An AbstractAgent hides the Learner from the type signature:
```scala
sealed trait AbstractAgent[S,A,P[_]] {
  def π: GenPolicy[S,A,M,P]
  type M
  def m: M
  implicit def M: Memory3[M,S,A]
}
```

Domain Model
------
```scala
trait GenerativeModel3[F[_],S,A] {
  def sample(s: S, a: A): F[(Reward,Next[S,A])]
}
```

Action Abstraction
-----
```scala
trait AbstractAction[S,A,AA] {
  type P[_]
  type M
  implicit val M: Memory3[M,S,A]
  def experience[D[_[_],_,_],F[_]:Monad](domain: D[F,S,A], start: Next[S,A], action: AA)
                                        (implicit D: AbstractModel3[D,F,S,A], sample: P ~> F): F[(M,HistoryA[S,A],Next[S,A])]
}
```


- implicit parameters may complicate memoization
