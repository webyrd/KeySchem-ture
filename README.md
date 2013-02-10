KeySchem-ture
=============

KeySchemæture: Scheme research implementation of Impro-Visor grammar-related algorithms

This is a not-quite-yet-complete implementation of the CYK-parsing-based "bricks" analyses performed by [Robert Keller's](http://www.cs.hmc.edu/~keller/) [Impro-Visor program](http://www.cs.hmc.edu/~keller/jazz/improvisor/).  A high-level description of the analyses is contained in:

Robert M. Keller, August Toman-Yih, Alexandra Schofield, and Zachary Merritt.
[A Creative Improvisational Companion Based on Idiomatic Harmonic Bricks](http://computationalcreativity.net/iccc2012/wp-content/uploads/2012/05/155-Keller.pdf).
In Proceedings of the Third International Conference on Computational Creativity, Dublin, 2012.

This code was written during the summer of 2012 by (in alphabetical order): Brittany Moore, Emilie Mitchell, Tevyn Bell, and William E. Byrd.

To run the code, load the file '''cfgnorm-tests.scm''':

'''(load "cfgnorm-tests.scm")'''

Obvious TODOs:

1. Clean up the code.

2. Add more tests.

3. Finish the analyses.

4. Speed things up!

5. Document that code.

6. Evaluate the performance of different analysis techniques.

7. Port code to [miniKanren](http://minikanren.org/).

All code tested with Petite Chez Scheme Version 8.4.