## Need to make this as RMD

This is a very important reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4720993/

How is this different from UpSetR? Well a key thing is actually providing the data!

Figure 6: a) Exlcusive set (i.e. CCF + CKD but nothing else) vs. b) Aggregates which would include anyone with e.g. CCF+CKD

With exclusive sets we can determine the deviation which expresses 'surprise' of cardinality at a given intersection..

Cardinality = N
Degree = N overlaps

Things to do:
 1. Clean up code to make more generalisable (?combineR)
~~ 2. Reduce redundency ~~
~~ 3. Add tests for cardinality ~~
 4. Consistent documentation
 5. Implement deviation. 
 6. ?implement upset visualisation
