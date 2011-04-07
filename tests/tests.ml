exception Fail of string

let (@?) msg b =
  if not b then
    raise (Fail msg)

let (>::) name test main =
  try 
    test ();
    Printf.eprintf "%s %-20s OK\n" main name
  with Fail msg ->
    Printf.eprintf "%s %-20s FAIL(%s)\n" main name msg

let run main suites =
  List.iter (fun s -> s main) suites
