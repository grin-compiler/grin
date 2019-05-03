# Analysis Experiment

Compare Andersen style (inclusion based) and Steensgaard (unification based) points-to analyses.  
Compare dataflow approach with unfication approach in general.  

Check runtime and memory efficiency of:
  - precise vs imprecise analyses
  - dataflow vs unicification approach
  - optimize early (STG/Lambda) vs optimize late (GRIN)

Tools:
  - dataflow: [Souffle](https://github.com/souffle-lang/souffle)
  - unification: [unifiacion-fd](https://github.com/wrengr/unification-fd)

TODO/Experiments:
  - add closure constructor support for lambda
  - keep closures in lambda
  - lambda type inference (unification)
  - experiment: stg/lambda level whole program analysis
    - with explicit stg-style closure representaion in lambda track closures and it's saturation status ; this could subsume grin's eval/apply tracking/approximation
    - implement andersen style: with souffle
      - closure tacker ; generate specialised eval and Pnode constructions during lambda -> grin compilation
      - accurate live variable/datafield analysis
      - inaccurate live variable/datafield analysis
    - implement steensgaard as typeinference: with unification-fd
        - lambda
        - grin
    - material:
        - https://github.com/wrengr/unification-fd
        - https://fineshambles.com/2017/07/26/a-type-inference-implementation-adventure/
        - https://winterkoninkje.dreamwidth.org/tag/unification
        - https://ro-che.info/articles/2017-06-17-generic-unification
        - http://nochair.net/posts/2012/03-29-unification-fd.html
        - https://bl.ocks.org/nponeccop/631dfba9180f8fa3020dff82df3290a3
    - other:
        - https://github.com/willtim/Expresso
        - https://github.com/willtim/row-polymorphism

### Context Sensitivity

It seem extremely beneficial to use Context Sensitive Analysis:
 - [Alternate Control-Flow Analyses for Defunctionalization in the MLton Compiler](https://www.cs.rit.edu/~mtf/student-resources/20155_shea_mscourse.pdf)
 - [Pushdown Control-Flow Analysis for Free](https://arxiv.org/abs/1507.03137)
 - [A model of context-sensitive pointer analysis](https://www.youtube.com/watch?v=vcj9uvRkCnc&list=PLRUJ115QHa0WMyGyP2j_1KRFJjaT0kFOu&index=3&t=0s) (video)
