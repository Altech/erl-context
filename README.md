### Whta is this?
Code for my graduate research : Concurrent Context-Oriented Programming using Group-Wide Reflection

### Files
- runtimes
  - runtime_base : runtime of simple actor language on Erlang
  - runtime_gwr : construct group-wide reflection on runime_base
  - runtime_gwrc : add state for context-oriented programming to runime_gwr
- threadring_* : app for micro benchmark for each runtime
- sensor_* : app for evaluation including rollback for each runtime
- cross_* : example of cross-context message
- othres : sub modules
