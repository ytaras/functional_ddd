functional_ddd
==============

This is repository, where I'm trying to evolve from mutable Domain Model
to immutable one.

Model
=====
Model is fairly simple here. Here I'll try to describe Ubiquitous
Language which could be used to describe Domain:
 - Main (and only) entity is a Work Task. 
 - Each Work Task can have following Statuses: Created, Started and
   Finished
 - When Work Task has Created State, I can Start it and it's Status
   moves to Started
 - When Work Task has Started State, I can Log hours to it and it'll add
   it to Logged Hours
 - Also I can Finish Stared Work Task and it's Status will move to
   Finished
 - I'm not able to change Finished Task

Current state
=============
Now I have mutable model, which even doesn't have repositories
implemented

Previous states
===============
TODO: Here will go steps which I took to migrate to currect state of
Domain Model

TODO
====

 # Refactor scalacheck properties - they are ugly and non Scala-ish now
