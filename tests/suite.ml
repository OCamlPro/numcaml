let suites = [
  Ints.suite;
  Floats.suite;
  Vectors.suite;
  Matrices.suite;
]

let _ =
  Tests.run "numcaml" (List.flatten suites)
