open! Core
open! Hardcaml
open! Advent_of_fpga

let generate_day1p1_rtl () =
  let module C = Circuit.With_interface (Day1p1.I) (Day1p1.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day1p1_top" (Day1p1.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl

let day1p1_rtl_command =
  Command.basic ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_day1p1_rtl ()]

let () =
  Command_unix.run
    (Command.group ~summary:"" [ ("day1p1", day1p1_rtl_command) ])
